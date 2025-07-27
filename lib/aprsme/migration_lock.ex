defmodule Aprsme.MigrationLock do
  @moduledoc """
  Provides distributed locking for database migrations using PostgreSQL advisory locks.
  This ensures only one node runs migrations at a time in a clustered environment.
  """

  require Logger

  # Use a consistent lock ID for migrations
  # This is a 64-bit integer that's unlikely to conflict with other locks
  @migration_lock_id 8_764_293_847_291

  @doc """
  Attempts to acquire an exclusive advisory lock and run migrations.
  Returns :ok if migrations were run successfully or :skipped if another node holds the lock.
  """
  def with_lock(repo, fun) do
    case acquire_lock(repo) do
      :ok ->
        try do
          Logger.info("Acquired migration lock, running migrations...")
          result = fun.()
          Logger.info("Migrations completed successfully")
          result
        after
          release_lock(repo)
          Logger.info("Released migration lock")
        end

      :locked ->
        Logger.info("Another node is running migrations, waiting...")
        wait_for_migrations(repo)
        :skipped
    end
  end

  defp acquire_lock(repo) do
    # Try to acquire an exclusive advisory lock (non-blocking)
    query = "SELECT pg_try_advisory_lock($1)"
    
    case repo.query(query, [@migration_lock_id]) do
      {:ok, %{rows: [[true]]}} ->
        :ok

      {:ok, %{rows: [[false]]}} ->
        :locked

      error ->
        Logger.error("Failed to acquire migration lock: #{inspect(error)}")
        :locked
    end
  end

  defp release_lock(repo) do
    query = "SELECT pg_advisory_unlock($1)"
    
    case repo.query(query, [@migration_lock_id]) do
      {:ok, %{rows: [[true]]}} ->
        :ok

      error ->
        Logger.error("Failed to release migration lock: #{inspect(error)}")
        error
    end
  end

  defp wait_for_migrations(repo) do
    # Wait for up to 60 seconds for migrations to complete
    wait_for_migrations(repo, 60)
  end

  defp wait_for_migrations(_repo, 0) do
    Logger.warn("Timeout waiting for migrations to complete")
    :timeout
  end

  defp wait_for_migrations(repo, retries) do
    # Check if lock is still held
    query = "SELECT pg_try_advisory_lock($1)"
    
    case repo.query(query, [@migration_lock_id]) do
      {:ok, %{rows: [[true]]}} ->
        # We got the lock, which means migrations are done
        release_lock(repo)
        Logger.info("Migrations completed on another node")
        :ok

      {:ok, %{rows: [[false]]}} ->
        # Lock is still held, wait and retry
        Process.sleep(1_000)
        wait_for_migrations(repo, retries - 1)

      error ->
        Logger.error("Error checking migration lock: #{inspect(error)}")
        :error
    end
  end
end