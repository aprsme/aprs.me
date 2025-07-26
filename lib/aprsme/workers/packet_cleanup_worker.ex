defmodule Aprsme.Workers.PacketCleanupWorker do
  @moduledoc """
  Exq worker for cleaning up old APRS packet data.

  This worker is responsible for:
  1. Removing packets older than the retention period (1 year by default)
  2. Providing granular cleanup with custom age parameters
  3. Logging statistics about cleanup operations
  4. Supporting batch processing for large datasets

  This worker is scheduled to run every 6 hours via Exq cron feature for more frequent,
  smaller cleanup operations to prevent large deletion spikes.

  ## Functions
  - `perform/1` - Standard cleanup using configured retention period
  - `cleanup_packets_older_than/1` - Cleanup packets older than specified days
  - `cleanup_packets_in_batches/1` - Batch cleanup for large datasets
  """

  # Import modules needed for database operations
  import Ecto.Query

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

    # Count packets before cleanup for statistics
    _total_before = count_total_packets()

    # Perform the cleanup of old packets with custom age using batch processing
    {deleted_count, _} = cleanup_packets_older_than_batched(days)

    # Log results
    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{days} days")

    # Return success
    :ok
  end

  @spec perform(map()) :: :ok | {:error, String.t()}
  def perform(_args) do
    Logger.info("Starting scheduled APRS packet cleanup")

    # Count packets before cleanup for statistics
    _total_before = count_total_packets()

    # Perform the cleanup of old packets (older than 1 year by default) using batch processing
    {deleted_count, _} = cleanup_old_packets_batched()

    # Log results
    retention_days = Application.get_env(:aprsme, :packet_retention_days, 365)

    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{retention_days} days")

    # Return success
    :ok
  end

  defp count_total_packets do
    Repo.aggregate(Packet, :count, :id)
  end

  defp cleanup_old_packets_batched do
    retention_days = Application.get_env(:aprsme, :packet_retention_days, 365)
    cleanup_packets_older_than_batched(retention_days)
  end

  @doc """
  Perform cleanup of packets older than a specific number of days using batch processing.

  This function provides more granular control over packet cleanup operations and
  processes deletions in smaller batches to prevent long-running transactions.

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

    Logger.info(
      "APRS packet cleanup complete: removed #{deleted_count} packets in #{batches_processed} batches over #{duration}ms"
    )

    {deleted_count, batches_processed}
  end

  @doc """
  Clean packets older than a specific number of days.

  This function allows for more granular cleanup operations by specifying
  the exact age threshold for packet deletion.

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

  This function processes deletions in smaller batches and respects time limits
  to ensure the cleanup doesn't impact system performance.

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

  defp cleanup_packets_in_batches(cutoff_time, start_time, total_deleted, batches_processed) do
    # Check if we've exceeded the maximum cleanup time
    current_time = System.monotonic_time(:millisecond)

    if current_time - start_time > @max_cleanup_time do
      Logger.info("Cleanup time limit reached (#{@max_cleanup_time}ms), stopping batch processing")
      {total_deleted, batches_processed}
    else
      # Get batch of packet IDs to delete
      packet_ids = get_packet_ids_for_deletion(cutoff_time, @batch_size)

      case packet_ids do
        [] ->
          # No more packets to delete
          {total_deleted, batches_processed}

        ids ->
          # Delete the batch
          {deleted_count, _} = Repo.delete_all(from(p in Packet, where: p.id in ^ids))

          new_total_deleted = total_deleted + deleted_count
          new_batches_processed = batches_processed + 1

          Logger.debug(
            "Cleanup batch #{new_batches_processed}: deleted #{deleted_count} packets (total: #{new_total_deleted})"
          )

          # Continue with next batch
          cleanup_packets_in_batches(cutoff_time, start_time, new_total_deleted, new_batches_processed)
      end
    end
  end

  @doc """
  Get packet IDs for deletion in batches.

  This function queries for packet IDs that are older than the cutoff time,
  limiting the result to the specified batch size.

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
  end
end
