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

    topologies = Application.get_env(:libcluster, :topologies) || []

    children = [
      # Start the Telemetry supervisor
      AprsmeWeb.Telemetry,
      # Start the Ecto repository
      Aprsme.Repo,
      # Start the PubSub system
      {Phoenix.PubSub, name: Aprsme.PubSub},
      # Start cache systems
      %{id: :query_cache, start: {Cachex, :start_link, [:query_cache, [limit: 10_000]]}},
      %{id: :device_cache, start: {Cachex, :start_link, [:device_cache, [limit: 5_000]]}},
      %{id: :symbol_cache, start: {Cachex, :start_link, [:symbol_cache, [limit: 1_000]]}},
      # Start circuit breaker
      Aprsme.CircuitBreaker,
      # Start device cache manager
      Aprsme.DeviceCache,

      # Start the Endpoint (http/https)
      AprsmeWeb.Endpoint,
      # Start a worker by calling: Aprsme.Worker.start_link(arg)
      # {Aprsme.Worker, arg}
      {Registry, keys: :duplicate, name: Registry.PubSub, partitions: System.schedulers_online()},
      {Cluster.Supervisor, [topologies, [name: Aprsme.ClusterSupervisor]]},
      # Start Oban for background jobs
      {Oban, :aprsme |> Application.get_env(Oban, []) |> Keyword.put(:queues, default: 10, maintenance: 2)},
      Aprsme.Presence,
      Aprsme.PostgresNotifier,
      # Start the packet processing pipeline
      Aprsme.PacketPipelineSupervisor,
      # Start the packet pipeline setup
      Aprsme.PacketPipelineSetup
    ]

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

  defp maybe_add_is_supervisor(children, env) do
    disable_connection = Application.get_env(:aprsme, :disable_aprs_connection, false)

    if env in [:prod, :dev] and not disable_connection do
      children ++ [Aprsme.Is.IsSupervisor]
    else
      children
    end
  end

  defp maybe_add_aprs_connection(children, _env) do
    disable_connection = Application.get_env(:aprsme, :disable_aprs_connection, false)

    if disable_connection do
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
end
