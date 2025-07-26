defmodule Aprsme.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  # Configure Oban for background jobs

  @impl true
  def start(_type, _args) do
    # Run migrations on startup
    migrate()

    :logger.add_handler(:my_sentry_handler, Sentry.LoggerHandler, %{
      config: %{metadata: [:file, :line]}
    })

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
      # Start Oban for background jobs
      {Oban, :aprsme |> Application.get_env(Oban, []) |> Keyword.put(:queues, default: 10, maintenance: 2)},
      Aprsme.Presence,
      Aprsme.PostgresNotifier,
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
    do_migrate(auto_migrate)

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
            Aprsme.Cluster.PacketReceiver
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

  defp do_migrate(false) do
    require Logger

    Logger.info("Automatic migrations disabled")
  end

  defp pubsub_config do
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)
    redis_url = System.get_env("REDIS_URL")

    if cluster_enabled and redis_url do
      require Logger

      Logger.info("Starting Redis PubSub adapter with URL: #{redis_url}")

      {Phoenix.PubSub,
       name: Aprsme.PubSub, adapter: Phoenix.PubSub.Redis, redis_pool_size: 10, node_name: node(), url: redis_url}
    else
      require Logger

      Logger.info(
        "Starting default PubSub adapter (cluster_enabled: #{cluster_enabled}, redis_url: #{inspect(redis_url)})"
      )

      {Phoenix.PubSub, name: Aprsme.PubSub}
    end
  end

  defp redis_children do
    if System.get_env("REDIS_URL") do
      require Logger

      Logger.info("Starting Redis-based caching and rate limiting")

      [
        # Redis-based rate limiter
        Aprsme.RedisRateLimiter,
        # Redis-based caches
        %{
          id: :query_cache,
          start: {Aprsme.RedisCache, :start_link, [[name: :query_cache]]}
        },
        %{
          id: :device_cache,
          start: {Aprsme.RedisCache, :start_link, [[name: :device_cache]]}
        },
        %{
          id: :symbol_cache,
          start: {Aprsme.RedisCache, :start_link, [[name: :symbol_cache]]}
        }
      ]
    else
      require Logger

      Logger.info("Starting ETS-based caching and rate limiting (no Redis URL)")

      [
        # Fallback to ETS-based implementations
        Aprsme.RateLimiter,
        %{id: :query_cache, start: {Cachex, :start_link, [:query_cache, [limit: 10_000]]}},
        %{id: :device_cache, start: {Cachex, :start_link, [:device_cache, [limit: 5_000]]}},
        %{id: :symbol_cache, start: {Cachex, :start_link, [:symbol_cache, [limit: 1_000]]}}
      ]
    end
  end
end
