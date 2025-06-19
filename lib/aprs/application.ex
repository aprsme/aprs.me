defmodule Aprs.Application do
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
      AprsWeb.Telemetry,
      # Start the Ecto repository
      Aprs.Repo,
      # Start the PubSub system
      {Phoenix.PubSub, name: Aprs.PubSub},
      # Start Finch
      {Finch, name: Aprs.Finch},
      # Start the Endpoint (http/https)
      AprsWeb.Endpoint,
      # Start a worker by calling: Aprs.Worker.start_link(arg)
      # {Aprs.Worker, arg}
      {Registry, keys: :duplicate, name: Registry.PubSub, partitions: System.schedulers_online()},
      {Cluster.Supervisor, [topologies, [name: Aprs.ClusterSupervisor]]},
      # Start Oban for background jobs
      {Oban, :aprs |> Application.get_env(Oban, []) |> Keyword.put(:queues, default: 10, maintenance: 2)},
      Aprs.Presence,
      Aprs.AprsIsConnection,
      Aprs.PostgresNotifier
    ]

    children =
      if Application.get_env(:aprs, :env) in [:prod, :dev] do
        children ++ [Aprs.Is.IsSupervisor]
      else
        children
      end

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Aprs.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    AprsWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp migrate do
    if Application.get_env(:aprs, :auto_migrate, true) do
      require Logger

      Logger.info("Running database migrations...")
      Aprs.Release.migrate()
      Logger.info("Database migrations completed")
    else
      require Logger

      Logger.info("Automatic migrations disabled")
    end
  rescue
    error ->
      require Logger

      Logger.error("Failed to run migrations: #{inspect(error)}")
      # Don't crash the application, just log the error
      :ok
  end
end
