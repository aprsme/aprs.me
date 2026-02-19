defmodule Aprsme.Workers.PacketCleanupWorker do
  @moduledoc """
  Worker for cleaning up old APRS packet data.

  This worker is responsible for:
  1. Removing packets older than the retention period (7 days by default)
  2. Providing granular cleanup with custom age parameters
  3. Logging statistics about cleanup operations
  4. Supporting batch processing for large datasets

  Uses a single-query DELETE with a CTE for efficient batch cleanup,
  avoiding the overhead of a separate SELECT + DELETE per batch.

  ## Functions
  - `perform/1` - Standard cleanup using configured retention period
  - `cleanup_packets_older_than/1` - Cleanup packets older than specified days
  - `cleanup_packets_in_batches/2` - Batch cleanup for large datasets
  """

  import Ecto.Query

  alias Aprsme.BadPacket
  alias Aprsme.Packet
  alias Aprsme.Repo

  require Logger

  # Batch size for cleanup operations to prevent long-running transactions
  @batch_size 10_000
  # Maximum time to spend on cleanup in milliseconds (5 minutes)
  @max_cleanup_time 5 * 60 * 1000

  @spec perform(map()) :: :ok | {:error, String.t()}
  def perform(%{"cleanup_days" => days}) when is_integer(days) do
    Logger.info("Starting scheduled APRS packet cleanup for packets older than #{days} days")

    {deleted_count, _} = cleanup_packets_older_than_batched(days)

    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{days} days")

    :ok
  rescue
    error ->
      Logger.error("Packet cleanup failed: #{inspect(error)}\n#{inspect(__STACKTRACE__)}")
      {:error, "Cleanup failed: #{inspect(error)}"}
  end

  @spec perform(map()) :: :ok | {:error, String.t()}
  def perform(_args) do
    Logger.info("Starting scheduled APRS packet cleanup")

    {deleted_count, _} = cleanup_old_packets_batched()

    bad_packet_count = cleanup_old_bad_packets()

    retention_days = Application.get_env(:aprsme, :packet_retention_days, 7)

    Logger.info(
      "APRS packet cleanup complete: removed #{deleted_count} packets and #{bad_packet_count} bad packets older than #{retention_days} days"
    )

    :ok
  rescue
    error ->
      Logger.error("Packet cleanup failed: #{inspect(error)}\n#{inspect(__STACKTRACE__)}")
      {:error, "Cleanup failed: #{inspect(error)}"}
  end

  @doc """
  Perform cleanup of packets older than a specific number of days using batch processing.

  ## Parameters
  - `days` - Number of days to retain packets (packets older than this will be deleted)

  ## Returns
  - {deleted_count, batches_processed}
  """
  @spec cleanup_packets_older_than_batched(pos_integer()) :: {non_neg_integer(), non_neg_integer()}
  def cleanup_packets_older_than_batched(days) when is_integer(days) and days > 0 do
    Logger.info("Starting batched APRS packet cleanup for packets older than #{days} days")

    cutoff_time = DateTime.add(DateTime.utc_now(), -days * 86_400, :second)
    start_time = System.monotonic_time(:millisecond)

    {deleted_count, batches_processed} = cleanup_packets_in_batches(cutoff_time, start_time)

    duration = System.monotonic_time(:millisecond) - start_time

    :telemetry.execute(
      [:aprsme, :packet_cleanup, :complete],
      %{deleted: deleted_count, batches: batches_processed, duration_ms: duration},
      %{retention_days: days}
    )

    Logger.info(
      "APRS packet cleanup complete: removed #{deleted_count} packets in #{batches_processed} batches over #{duration}ms"
    )

    {deleted_count, batches_processed}
  end

  @doc """
  Clean packets older than a specific number of days.

  ## Parameters
  - `days` - Number of days to keep (packets older than this will be deleted)

  ## Returns
  - Number of packets deleted
  """
  @spec cleanup_packets_older_than(pos_integer()) :: non_neg_integer()
  def cleanup_packets_older_than(days) when is_integer(days) and days > 0 do
    {deleted_count, _} = cleanup_packets_older_than_batched(days)
    deleted_count
  end

  @doc """
  Clean packets in batches to prevent long-running transactions.

  Uses a single DELETE query with a subquery LIMIT per batch,
  avoiding the overhead of separate SELECT + DELETE round-trips.

  ## Parameters
  - `cutoff_time` - DateTime before which packets should be deleted
  - `start_time` - Monotonic time when cleanup started (for duration tracking)

  ## Returns
  - {total_deleted_count, batches_processed}
  """
  @spec cleanup_packets_in_batches(DateTime.t(), integer()) :: {non_neg_integer(), non_neg_integer()}
  def cleanup_packets_in_batches(cutoff_time, start_time) do
    cleanup_packets_in_batches(cutoff_time, start_time, 0, 0)
  end

  @doc """
  Get packet IDs for deletion in batches.

  ## Parameters
  - `cutoff_time` - DateTime before which packets should be deleted
  - `batch_size` - Maximum number of IDs to return

  ## Returns
  - List of packet IDs to delete
  """
  @spec get_packet_ids_for_deletion(DateTime.t(), pos_integer()) :: [integer()]
  def get_packet_ids_for_deletion(cutoff_time, batch_size) do
    Repo.all(
      from(p in Packet,
        where: p.received_at < ^cutoff_time,
        select: p.id,
        limit: ^batch_size
      )
    )
  rescue
    error ->
      Logger.error("Failed to get packet IDs for deletion: #{inspect(error)}")
      []
  end

  # Private functions

  defp cleanup_old_packets_batched do
    retention_days = Application.get_env(:aprsme, :packet_retention_days, 7)
    cleanup_packets_older_than_batched(retention_days)
  end

  defp cleanup_old_bad_packets do
    retention_days = Application.get_env(:aprsme, :packet_retention_days, 7)
    cutoff_time = DateTime.add(DateTime.utc_now(), -retention_days * 86_400, :second)

    {deleted_count, _} =
      Repo.delete_all(from(b in BadPacket, where: b.attempted_at < ^cutoff_time))

    deleted_count
  rescue
    error ->
      Logger.error("Failed to clean up bad packets: #{inspect(error)}")
      0
  end

  defp cleanup_packets_in_batches(cutoff_time, start_time, total_deleted, batches_processed) do
    current_time = System.monotonic_time(:millisecond)

    if current_time - start_time > @max_cleanup_time do
      Logger.info("Cleanup time limit reached (#{@max_cleanup_time}ms), stopping batch processing")
      {total_deleted, batches_processed}
    else
      try do
        deleted_count = delete_batch(cutoff_time, @batch_size)

        if deleted_count == 0 do
          {total_deleted, batches_processed}
        else
          new_total = total_deleted + deleted_count
          new_batches = batches_processed + 1

          Logger.debug("Cleanup batch #{new_batches}: deleted #{deleted_count} packets (total: #{new_total})")

          cleanup_packets_in_batches(cutoff_time, start_time, new_total, new_batches)
        end
      rescue
        error ->
          Logger.error("Failed to delete batch of packets: #{inspect(error)}")
          {total_deleted, batches_processed}
      end
    end
  end

  # Single-query batch delete using a CTE subquery.
  # This is significantly faster than SELECT IDs + DELETE by IDs
  # because it's a single database round-trip and allows PostgreSQL
  # to optimize the delete plan (sequential scan on received_at index).
  @spec delete_batch(DateTime.t(), pos_integer()) :: non_neg_integer()
  defp delete_batch(cutoff_time, batch_size) do
    # Use raw SQL for the CTE-based batch delete since Ecto doesn't
    # natively support DELETE ... WHERE id IN (SELECT ... LIMIT ...)
    {:ok, %{num_rows: deleted_count}} =
      Repo.query(
        """
        DELETE FROM packets
        WHERE id IN (
          SELECT id FROM packets
          WHERE received_at < $1
          LIMIT $2
        )
        """,
        [cutoff_time, batch_size]
      )

    deleted_count
  end
end
