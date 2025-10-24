defmodule Aprsme.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  # Configure Oban for background jobs

  @impl true
  def start(_type, _args) do
    # Initialize deployment timestamp
    Aprsme.Release.init()

    # Run migrations on startup
    migrate()

    children = [
      # Start the Telemetry supervisor
      AprsmeWeb.Telemetry,
      # Start the Ecto repository
      Aprsme.Repo,
      # Start the PubSub system
      pubsub_config(),
      # Start Redis-based rate limiter and caches (only if Redis is available)
      # Start circuit breaker
      Aprsme.CircuitBreaker,
      # Start regex cache for performance
      Aprsme.RegexCache,
      # Start device cache manager
      Aprsme.DeviceCache,
      # Start broadcast task supervisor for async operations
      Aprsme.BroadcastTaskSupervisor,
      # Start spatial PubSub for viewport-based filtering
      Aprsme.SpatialPubSub,
      # Start global streaming packets PubSub
      Aprsme.StreamingPacketsPubSub,
      # Start packet store for efficient LiveView memory usage
      AprsmeWeb.MapLive.PacketStore,

      # Start the Endpoint (http/https)
      AprsmeWeb.Endpoint,
      # Start a worker by calling: Aprsme.Worker.start_link(arg)
      # {Aprsme.Worker, arg}
      {Registry, keys: :duplicate, name: Registry.PubSub, partitions: System.schedulers_online()},
      # Start cleanup scheduler for periodic packet cleanup
      Aprsme.CleanupScheduler,
      Aprsme.Presence,
      Aprsme.PostgresNotifier,
      # Start deployment notifier
      Aprsme.DeploymentNotifier,
      # Start the packet processing pipeline
      Aprsme.PacketPipelineSupervisor
    ]

    children = children ++ redis_children()

    # Add shutdown handlers at the end, after everything else is started
    children =
      children ++
        [
          Aprsme.SignalHandler,
          Aprsme.ShutdownHandler
        ]

    children = maybe_add_cluster_components(children)
    children = maybe_add_is_supervisor(children, Application.get_env(:aprsme, :env))
    children = maybe_add_aprs_connection(children, Application.get_env(:aprsme, :env))
    # Exq is now started automatically via config, not in supervision tree

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Aprsme.Supervisor]
    {:ok, sup} = Supervisor.start_link(children, opts)

    # Now that the Repo is started, run the refresh in a background task
    # Skip in test environment to avoid DBConnection.OwnershipError
    env = Application.get_env(:aprsme, :env)

    if env != :test do
      Task.start(fn -> Aprsme.DeviceIdentification.maybe_refresh_devices() end)
    end

    {:ok, sup}
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    AprsmeWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp migrate do
    auto_migrate = Application.get_env(:aprsme, :auto_migrate, true)
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    # In cluster mode, prefer init containers or manual migration
    # to avoid race conditions between nodes
    if auto_migrate and not cluster_enabled do
      do_migrate(true)
    else
      require Logger

      if cluster_enabled do
        Logger.info("Skipping auto-migration in cluster mode")
      else
        Logger.info("Auto-migration disabled")
      end
    end

    # Gettext translations are automatically compiled during Mix compilation
  rescue
    error ->
      require Logger

      Logger.error("Failed to run migrations: #{inspect(error)}")
      # Don't crash the application, just log the error
      :ok
  end

  defp maybe_add_cluster_components(children) do
    if Application.get_env(:aprsme, :cluster_enabled, false) do
      topologies = Application.get_env(:libcluster, :topologies, [])

      # Packet receiver for distributed packets on non-leader nodes
      cluster_children =
        if topologies == [] do
          []
          # libcluster supervisor
          # Dynamic supervisor for processes managed by cluster leader
          # Leader election process
          # Connection manager that starts/stops APRS-IS based on leadership
        else
          [
            {Cluster.Supervisor, [topologies, [name: Aprsme.ClusterSupervisor]]},
            Aprsme.DynamicSupervisor,
            Aprsme.Cluster.LeaderElection,
            Aprsme.Cluster.ConnectionManager,
            Aprsme.Cluster.PacketReceiver,
            Aprsme.ConnectionMonitor
          ]
        end

      children ++ cluster_children
    else
      children
    end
  end

  defp maybe_add_is_supervisor(children, env) do
    disable_connection = Application.get_env(:aprsme, :disable_aprs_connection, false)
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    # Only add Is.IsSupervisor directly if clustering is disabled
    if env in [:prod, :dev] and not disable_connection and not cluster_enabled do
      children ++ [Aprsme.Is.IsSupervisor]
    else
      children
    end
  end

  defp maybe_add_aprs_connection(children, _env) do
    disable_connection = Application.get_env(:aprsme, :disable_aprs_connection, false)
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    # Don't add AprsIsConnection if clustering is enabled or connection is disabled
    if disable_connection or cluster_enabled do
      children
    else
      children ++ [Aprsme.AprsIsConnection]
    end
  end

  defp do_migrate(true) do
    require Logger

    Logger.info("Running database migrations...")
    Aprsme.Release.migrate()
    Logger.info("Database migrations completed")
  end

  defp pubsub_config do
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    if cluster_enabled do
      require Logger

      Logger.info("Starting distributed PubSub for clustering")

      # Phoenix PubSub automatically uses distributed Erlang clustering when nodes are connected
      {Phoenix.PubSub, name: Aprsme.PubSub}
    else
      require Logger

      Logger.info("Starting local PubSub adapter")

      {Phoenix.PubSub, name: Aprsme.PubSub}
    end
  end

  # Removed - Exq configuration is now in runtime.exs
  # defp exq_config do
  #   redis_url = System.get_env("REDIS_URL", "redis://localhost:6379")
  #   
  #   {Exq,
  #    name: Exq,
  #    host: parse_redis_host(redis_url),
  #    port: parse_redis_port(redis_url),
  #    password: parse_redis_password(redis_url),
  #    database: parse_redis_database(redis_url),
  #    concurrency: :infinite,
  #    queues: [
  #      {"default", 10},
  #      {"maintenance", 2}
  #    ],
  #    scheduler_enable: true,
  #    scheduler_poll_timeout: 200,
  #    poll_timeout: 50,
  #    redis_timeout: 5000}
  # end

  # Removed - Redis parsing functions no longer needed
  # Exq configuration is now handled in runtime.exs

  # Removed - Exq is now started automatically via config
  # defp maybe_add_exq(children) do
  #   env = Application.get_env(:aprsme, :env)
  #   if env == :test do
  #     children
  #   else
  #     children ++ [exq_config()]
  #   end
  # end

  defp redis_children do
    require Logger

    Logger.info("Starting ETS-based caching and rate limiting")

    # Create ETS tables for caching
    :ets.new(:query_cache, [:set, :public, :named_table, read_concurrency: true])
    :ets.new(:device_cache, [:set, :public, :named_table, read_concurrency: true])
    :ets.new(:symbol_cache, [:set, :public, :named_table, read_concurrency: true])

    [
      # ETS-based rate limiter
      Aprsme.RateLimiter
    ]
  end
end
