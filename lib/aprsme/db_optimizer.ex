defmodule Aprsme.DbOptimizer do
  @moduledoc """
  Database optimization utilities that leverage PostgreSQL server configuration
  for better performance on ARM RK3588 with 16GB RAM.

  PostgreSQL key settings we're optimizing for:
  - max_connections = 100
  - shared_buffers = 1GB  
  - work_mem = 16MB
  - synchronous_commit = off
  - effective_io_concurrency = 100 (SSD optimized)
  """

  alias Aprsme.Repo
  alias Ecto.Adapters.SQL

  require Logger

  @doc """
  Execute a large batch insert using PostgreSQL COPY command for maximum performance.
  This is significantly faster than INSERT for large datasets.
  """
  def copy_insert(table_name, columns, rows) when length(rows) > 1000 do
    # Convert rows to CSV format for COPY
    csv_data = rows_to_csv(rows, columns)

    # Use COPY command which bypasses much of the overhead of INSERT
    query = """
    COPY #{table_name} (#{Enum.join(columns, ", ")})
    FROM STDIN WITH (FORMAT csv, HEADER false)
    """

    SQL.query!(Repo, query, [csv_data])
    length(rows)
  rescue
    error ->
      Logger.error("COPY insert failed: #{inspect(error)}")
      # Fall back to regular insert
      regular_batch_insert(table_name, columns, rows)
  end

  def copy_insert(table_name, columns, rows) do
    # For smaller batches, use regular insert
    regular_batch_insert(table_name, columns, rows)
  end

  @doc """
  Optimized batch insert that leverages PostgreSQL configuration
  """
  def optimized_batch_insert(schema, entries, opts \\ []) do
    # Calculate optimal batch size based on work_mem
    optimal_batch_size = calculate_optimal_batch_size(entries)

    # Split into optimal chunks
    entries
    |> Enum.chunk_every(optimal_batch_size)
    |> Enum.map(fn batch ->
      insert_opts =
        Keyword.merge(
          [
            returning: false,
            on_conflict: :nothing,
            timeout: 60_000,
            placeholders: length(batch) > 100
          ],
          opts
        )

      case Repo.insert_all(schema, batch, insert_opts) do
        {count, _} -> {:ok, count}
        error -> {:error, error}
      end
    end)
    |> Enum.reduce({0, 0}, fn
      {:ok, count}, {success, errors} -> {success + count, errors}
      {:error, _}, {success, errors} -> {success, errors + 1}
    end)
  end

  @doc """
  Calculate optimal batch size based on PostgreSQL work_mem setting (16MB)
  and estimated row size
  """
  def calculate_optimal_batch_size(entries) when is_list(entries) do
    # Estimate size of one entry (rough approximation)
    sample = List.first(entries)
    estimated_size = estimate_entry_size(sample)

    # work_mem is 16MB, leave some headroom
    # 14MB in bytes
    available_memory = 14 * 1024 * 1024

    # Calculate how many entries fit in work_mem
    max_batch = div(available_memory, estimated_size)

    # Cap at reasonable limits
    max_batch |> min(2000) |> max(100)
  end

  @doc """
  Run ANALYZE on a table after bulk inserts to update statistics
  This helps PostgreSQL make better query plans
  """
  def analyze_table(table_name) do
    SQL.query!(Repo, "ANALYZE #{table_name}", [])
    :ok
  rescue
    error ->
      Logger.warning("Failed to analyze table #{table_name}: #{inspect(error)}")
      :error
  end

  @doc """
  Vacuum a table to reclaim space and update visibility map
  Use this after large delete operations
  """
  def vacuum_table(table_name, opts \\ []) do
    full = Keyword.get(opts, :full, false)
    analyze = Keyword.get(opts, :analyze, true)

    vacuum_type = if full, do: "VACUUM FULL", else: "VACUUM"
    analyze_clause = if analyze, do: " ANALYZE", else: ""

    query = "#{vacuum_type}#{analyze_clause} #{table_name}"

    # Vacuum operations can take a long time
    SQL.query!(Repo, query, [], timeout: :infinity)
    :ok
  rescue
    error ->
      Logger.error("Failed to vacuum table #{table_name}: #{inspect(error)}")
      :error
  end

  @doc """
  Get current database statistics for monitoring
  """
  def get_connection_stats do
    query = """
    SELECT 
      count(*) as total_connections,
      count(*) FILTER (WHERE state = 'active') as active_connections,
      count(*) FILTER (WHERE state = 'idle') as idle_connections,
      count(*) FILTER (WHERE state = 'idle in transaction') as idle_in_transaction,
      count(*) FILTER (WHERE wait_event_type IS NOT NULL) as waiting_connections
    FROM pg_stat_activity
    WHERE datname = current_database()
    """

    case SQL.query(Repo, query, []) do
      {:ok, %{rows: [[total, active, idle, idle_tx, waiting]]}} ->
        %{
          total: total,
          active: active,
          idle: idle,
          idle_in_transaction: idle_tx,
          waiting: waiting
        }

      _ ->
        %{}
    end
  end

  # Private functions

  defp regular_batch_insert(table_name, columns, rows) do
    # Convert to maps for Ecto.insert_all
    entries =
      Enum.map(rows, fn row ->
        columns |> Enum.zip(row) |> Map.new()
      end)

    {count, _} =
      Repo.insert_all(table_name, entries,
        returning: false,
        on_conflict: :nothing,
        timeout: 60_000
      )

    count
  end

  defp rows_to_csv(rows, _columns) do
    Enum.map_join(rows, "\n", fn row ->
      Enum.map_join(row, ",", &escape_csv_value/1)
    end)
  end

  defp escape_csv_value(nil), do: ""

  defp escape_csv_value(value) when is_binary(value) do
    if String.contains?(value, [",", "\"", "\n"]) do
      "\"#{String.replace(value, "\"", "\"\"")}\""
    else
      value
    end
  end

  defp escape_csv_value(value), do: to_string(value)

  # Default 1KB
  defp estimate_entry_size(nil), do: 1024

  defp estimate_entry_size(entry) when is_map(entry) do
    # Rough estimation of entry size in bytes
    entry
    |> Map.values()
    |> Enum.map(&estimate_value_size/1)
    |> Enum.sum()
    # Add overhead for structure
    |> Kernel.+(100)
  end

  defp estimate_entry_size(_), do: 1024

  defp estimate_value_size(nil), do: 4
  defp estimate_value_size(value) when is_binary(value), do: byte_size(value)
  defp estimate_value_size(value) when is_integer(value), do: 8
  defp estimate_value_size(value) when is_float(value), do: 8
  defp estimate_value_size(value) when is_boolean(value), do: 1
  defp estimate_value_size(%DateTime{}), do: 8
  defp estimate_value_size(%Date{}), do: 4
  # Conservative estimate for complex types
  defp estimate_value_size(_), do: 50
end
