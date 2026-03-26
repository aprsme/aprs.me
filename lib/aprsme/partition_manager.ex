defmodule Aprsme.PartitionManager do
  @moduledoc """
  Manages daily partitions for the packets table.

  Creates future partitions and drops old ones past the retention period.
  Partition naming: packets_YYYYMMDD (e.g., packets_20260220).
  Each partition covers [YYYY-MM-DD 00:00:00, YYYY-MM-DD+1 00:00:00) UTC.
  """

  use GenServer

  alias Aprsme.Repo

  require Logger

  @check_interval to_timeout(hour: 1)
  @future_days 2

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Returns the partition table name for a given date.
  """
  @spec partition_name(Date.t()) :: String.t()
  def partition_name(%Date{} = date) do
    "packets_#{Calendar.strftime(date, "%Y%m%d")}"
  end

  @doc """
  Returns the {start, end} UTC datetime range for a partition covering the given date.
  """
  @spec partition_range(Date.t()) :: {DateTime.t(), DateTime.t()}
  def partition_range(%Date{} = date) do
    {:ok, start_dt} = DateTime.new(date, ~T[00:00:00], "Etc/UTC")
    next_date = Date.add(date, 1)
    {:ok, end_dt} = DateTime.new(next_date, ~T[00:00:00], "Etc/UTC")
    {start_dt, end_dt}
  end

  @doc """
  Ensures partitions exist for today through today + #{@future_days} days.
  Returns {:ok, list_of_created_partition_names}.
  """
  @spec ensure_partitions_exist() :: {:ok, [String.t()]}
  def ensure_partitions_exist do
    today = Date.utc_today()

    created =
      for offset <- 0..@future_days, reduce: [] do
        acc ->
          date = Date.add(today, offset)
          name = partition_name(date)

          if partition_exists?(name) do
            acc
          else
            create_partition_with_lock(date)
            [name | acc]
          end
      end

    {:ok, Enum.reverse(created)}
  end

  defp create_partition_with_lock(%Date{} = date) do
    name = partition_name(date)
    lock_key = :erlang.phash2(name)

    Repo.transaction(fn ->
      Repo.query!("SELECT pg_advisory_xact_lock($1)", [lock_key])

      if !partition_exists?(name) do
        create_partition(date)
      end
    end)

    name
  end

  @doc """
  Drops partitions older than the given number of retention days.
  Returns {:ok, list_of_dropped_partition_names}.
  """
  @spec drop_old_partitions(pos_integer()) :: {:ok, [String.t()]}
  def drop_old_partitions(retention_days \\ 7) do
    cutoff_date = Date.add(Date.utc_today(), -retention_days)

    dropped =
      list_partitions()
      |> Enum.filter(fn name ->
        case partition_date(name) do
          {:ok, date} -> Date.compare(date, cutoff_date) != :gt
          _ -> false
        end
      end)
      |> Enum.map(fn name ->
        drop_partition(name)
        name
      end)

    {:ok, dropped}
  end

  @doc """
  Lists all existing partition table names for the packets table.
  """
  @spec list_partitions() :: [String.t()]
  def list_partitions do
    case Repo.query(
           "SELECT inhrelid::regclass::text FROM pg_inherits WHERE inhparent = 'packets'::regclass ORDER BY inhrelid::regclass::text"
         ) do
      {:ok, %{rows: rows}} ->
        Enum.map(rows, fn [name] ->
          String.replace(name, ~r/^public\./, "")
        end)

      {:error, _} ->
        []
    end
  end

  # GenServer callbacks

  @impl true
  def init(_opts) do
    if Application.get_env(:aprsme, :env) != :test do
      send(self(), :manage_partitions)
    end

    {:ok, %{}}
  end

  @impl true
  def handle_info(:manage_partitions, state) do
    manage_partitions()
    Process.send_after(self(), :manage_partitions, @check_interval)
    {:noreply, state}
  end

  defp manage_partitions do
    case ensure_partitions_exist() do
      {:ok, []} ->
        :ok

      {:ok, created} ->
        Logger.info("Created #{length(created)} partition(s): #{Enum.join(created, ", ")}")
    end

    retention_days = Application.get_env(:aprsme, :packet_retention_days, 7)

    case drop_old_partitions(retention_days) do
      {:ok, []} ->
        :ok

      {:ok, dropped} ->
        Logger.info("Dropped #{length(dropped)} old partition(s): #{Enum.join(dropped, ", ")}")
    end
  rescue
    error ->
      Logger.error("Partition management failed: #{inspect(error)}")
  end

  defp partition_exists?(name) do
    case Repo.query(
           "SELECT 1 FROM pg_tables WHERE tablename = $1 AND schemaname = 'public'",
           [name]
         ) do
      {:ok, %{num_rows: n}} when n > 0 -> true
      _ -> false
    end
  end

  defp create_partition(%Date{} = date) do
    name = partition_name(date)
    {from_dt, to_dt} = partition_range(date)

    from_str = DateTime.to_iso8601(from_dt)
    to_str = DateTime.to_iso8601(to_dt)

    validate_partition_name!(name)

    Repo.query!(
      "CREATE TABLE IF NOT EXISTS #{quote_identifier(name)} PARTITION OF packets FOR VALUES FROM ('#{from_str}') TO ('#{to_str}')"
    )

    Logger.debug("Created partition #{name} [#{from_str}, #{to_str})")
    name
  end

  defp drop_partition(name) do
    # Validate partition name to prevent SQL injection
    validate_partition_name!(name)

    Repo.query!("DROP TABLE IF EXISTS #{quote_identifier(name)}")
    Logger.debug("Dropped partition #{name}")
    name
  end

  # Validates that partition name matches expected format: packets_YYYYMMDD
  # Raises if name is invalid to prevent SQL injection
  defp validate_partition_name!(name) do
    if !String.match?(name, ~r/^packets_\d{8}$/) do
      raise ArgumentError, "Invalid partition name: #{inspect(name)}"
    end
  end

  # Quotes SQL identifier to prevent injection
  defp quote_identifier(name) do
    ~s("#{String.replace(name, ~s("), ~s(""))}")
  end

  defp partition_date("packets_" <> date_str) do
    case Date.from_iso8601(
           String.slice(date_str, 0, 4) <>
             "-" <> String.slice(date_str, 4, 2) <> "-" <> String.slice(date_str, 6, 2)
         ) do
      {:ok, date} -> {:ok, date}
      _ -> nil
    end
  end

  defp partition_date(_), do: nil
end
