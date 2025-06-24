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

    topologies = Application.get_env(:libcluster, :topologies) || []

    children = [
      # Start the Telemetry supervisor
      AprsmeWeb.Telemetry,
      # Start the Ecto repository
      Aprsme.Repo,
      # Start the PubSub system
      {Phoenix.PubSub, name: Aprsme.PubSub},
      # Start Finch
      {Finch, name: Aprsme.Finch},
      # Start the Endpoint (http/https)
      AprsmeWeb.Endpoint,
      # Start a worker by calling: Aprsme.Worker.start_link(arg)
      # {Aprsme.Worker, arg}
      {Registry, keys: :duplicate, name: Registry.PubSub, partitions: System.schedulers_online()},
      {Cluster.Supervisor, [topologies, [name: Aprsme.ClusterSupervisor]]},
      # Start Oban for background jobs
      {Oban, :aprsme |> Application.get_env(Oban, []) |> Keyword.put(:queues, default: 10, maintenance: 2)},
      Aprsme.Presence,
      Aprsme.AprsIsConnection,
      Aprsme.PostgresNotifier
    ]

    children = maybe_add_is_supervisor(children, Application.get_env(:aprsme, :env))

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
  rescue
    error ->
      require Logger

      Logger.error("Failed to run migrations: #{inspect(error)}")
      # Don't crash the application, just log the error
      :ok
  end

  defp maybe_add_is_supervisor(children, env) when env in [:prod, :dev] do
    children ++ [Aprsme.Is.IsSupervisor]
  end

  defp maybe_add_is_supervisor(children, _env), do: children

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
