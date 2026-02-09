defmodule Aprsme.Release do
  @moduledoc """
  Used for executing DB release tasks when run in production without Mix
  installed.
  """
  alias Ecto.Adapters.SQL

  @app :aprsme

  def migrate do
    require Logger
    # Initialize deployment timestamp first
    deployed_at = init()
    Logger.info("Deployment timestamp: #{deployed_at}")

    # Gettext translations are automatically compiled during Mix compilation

    # Skip database creation when using PgBouncer
    # The database should already exist
    if System.get_env("SKIP_DB_CREATE") != "true" do
      create_database()
    end

    # Run migrations with distributed lock
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    # Configure longer timeouts for migration operations
    # 2 hours default
    migration_timeout = String.to_integer(System.get_env("MIGRATION_TIMEOUT", "7200000"))

    if cluster_enabled do
      Logger.info("Running migrations with distributed lock...")
      # Ensure repo is started for advisory lock with extended timeout
      repo_config =
        Aprsme.Repo.config()
        |> Keyword.put(:timeout, migration_timeout)
        |> Keyword.put(:pool_timeout, 60_000)

      {:ok, _} = Aprsme.Repo.start_link(repo_config)

      Aprsme.MigrationLock.with_lock(Aprsme.Repo, fn ->
        run_migrations_with_timeout(migration_timeout)
      end)
    else
      Logger.info("Running migrations without lock (single node)...")
      run_migrations_with_timeout(migration_timeout)
    end
  end

  defp create_database do
    require Logger

    case Aprsme.Repo.__adapter__().storage_up(Aprsme.Repo.config()) do
      :ok ->
        Logger.info("Database created successfully")

      {:error, :already_up} ->
        Logger.info("Database already exists")

      {:error, error} ->
        Logger.error("Failed to create database: #{inspect(error)}")
        raise "Database creation failed: #{inspect(error)}"
    end
  end

  def rollback(repo, version) do
    load_app()
    {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
  end

  defp load_app do
    Application.load(@app)
  end

  @doc """
  Initialize release-specific configuration.
  This is called during application startup.
  """
  def init do
    # Check if deployment timestamp is provided via environment variable
    # This allows k8s deployments to set a persistent timestamp
    deployed_at =
      case System.get_env("DEPLOYED_AT") do
        nil ->
          # Fallback to current time for local development or non-k8s deployments
          DateTime.utc_now()

        timestamp_str ->
          # Parse the ISO8601 timestamp from environment variable
          case DateTime.from_iso8601(timestamp_str) do
            {:ok, datetime, _offset} ->
              datetime

            {:error, _} ->
              require Logger

              Logger.warning("Invalid DEPLOYED_AT timestamp: #{timestamp_str}, using current time")
              DateTime.utc_now()
          end
      end

    # Store in application config
    Application.put_env(:aprsme, :deployed_at, deployed_at)

    # Notify about deployment after a short delay to ensure PubSub is started
    # In k8s, this will notify all connected clients about the new deployment
    if System.get_env("DEPLOYED_AT") do
      spawn(fn ->
        # Wait for application to start
        Process.sleep(10_000)

        try do
          require Logger

          Aprsme.DeploymentNotifier.notify_deployment(deployed_at)
          Logger.info("Deployment notification sent for timestamp: #{deployed_at}")
        rescue
          error ->
            require Logger

            Logger.warning("Failed to send deployment notification: #{inspect(error)}")
        end
      end)
    end

    deployed_at
  end

  @doc """
  Get the deployment timestamp.
  """
  def deployed_at do
    case Application.get_env(:aprsme, :deployed_at) do
      nil ->
        # If not initialized yet, initialize now
        timestamp = DateTime.utc_now()
        Application.put_env(:aprsme, :deployed_at, timestamp)
        timestamp

      timestamp ->
        timestamp
    end
  end

  defp run_migrations_with_timeout(timeout) do
    require Logger

    Logger.info("Running migrations with timeout: #{timeout}ms")

    # Run with extended timeout configuration
    _repo_config =
      Aprsme.Repo.config()
      |> Keyword.put(:timeout, timeout)
      |> Keyword.put(:pool_timeout, 60_000)

    {:ok, _, _} =
      Ecto.Migrator.with_repo(
        Aprsme.Repo,
        fn repo ->
          # Set session-level timeout for this connection
          # credo:disable-for-next-line
          # sobelow_skip ["SQL.Query"]
          SQL.query!(repo, "SET statement_timeout = '#{div(timeout, 1000)}s'")
          Ecto.Migrator.run(repo, :up, all: true)
        end,
        timeout: timeout
      )
  end
end
