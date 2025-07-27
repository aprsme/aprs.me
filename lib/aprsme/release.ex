defmodule Aprsme.Release do
  @moduledoc """
  Used for executing DB release tasks when run in production without Mix
  installed.
  """
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
    # Read deployment timestamp from file
    deployed_at = read_deployment_timestamp()

    # Add to application config
    Application.put_env(:aprsme, :deployed_at, deployed_at)

    deployed_at
  end

  @doc """
  Get the deployment timestamp.
  """
  def deployed_at do
    Application.get_env(:aprsme, :deployed_at) || DateTime.utc_now()
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
          Ecto.Adapters.SQL.query!(repo, "SET statement_timeout = '#{div(timeout, 1000)}s'")
          Ecto.Migrator.run(repo, :up, all: true)
        end, timeout: timeout)
  end

  defp read_deployment_timestamp do
    case File.read("/app/deployed_at.txt") do
      {:ok, timestamp} ->
        case DateTime.from_iso8601(String.trim(timestamp)) do
          {:ok, datetime, _} -> datetime
          _ -> DateTime.utc_now()
        end

      _ ->
        # Fallback to current time if file doesn't exist
        DateTime.utc_now()
    end
  end
end
