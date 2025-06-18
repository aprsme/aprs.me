defmodule Aprs.Workers.PacketCleanupWorker do
  @moduledoc """
  Oban worker for cleaning up old APRS packet data.

  This worker is responsible for:
  1. Removing packets older than the retention period (1 year by default)
  2. Providing granular cleanup with custom age parameters
  3. Logging statistics about cleanup operations

  This worker is scheduled to run daily at midnight UTC via Oban's cron feature.

  ## Functions
  - `perform/1` - Standard cleanup using configured retention period
  - `cleanup_packets_older_than/1` - Cleanup packets older than specified days
  """

  use Oban.Worker, queue: :maintenance, max_attempts: 3

  # Import modules needed for database operations

  alias Aprs.Packet
  alias Aprs.Packets
  alias Aprs.Repo

  require Logger

  @impl Oban.Worker
  @spec perform(Oban.Job.t()) :: :ok | {:error, String.t()}
  def perform(%Oban.Job{args: %{"cleanup_days" => days}}) when is_integer(days) do
    Logger.info("Starting scheduled APRS packet cleanup for packets older than #{days} days")

    # Count packets before cleanup for statistics
    _total_before = count_total_packets()

    # Perform the cleanup of old packets with custom age
    deleted_count = Packets.clean_packets_older_than(days)

    # Log results
    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{days} days")

    # Return success
    :ok
  end

  @spec perform(Oban.Job.t()) :: :ok | {:error, String.t()}
  def perform(%Oban.Job{args: _args}) do
    Logger.info("Starting scheduled APRS packet cleanup")

    # Count packets before cleanup for statistics
    _total_before = count_total_packets()

    # Perform the cleanup of old packets (older than 1 year by default)
    deleted_count = cleanup_old_packets()

    # Log results
    retention_days = Application.get_env(:aprs, :packet_retention_days, 365)
    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{retention_days} days")

    # Return success
    :ok
  end

  defp count_total_packets do
    Repo.aggregate(Packet, :count, :id)
  end

  defp cleanup_old_packets do
    # Use the existing function from Packets context
    Packets.clean_old_packets()
  end

  @doc """
  Perform cleanup of packets older than a specific number of days.

  This function provides more granular control over packet cleanup operations.

  ## Parameters
  - `days` - Number of days to retain packets (packets older than this will be deleted)

  ## Returns
  - Number of packets deleted
  """
  @spec cleanup_packets_older_than(pos_integer()) :: non_neg_integer()
  def cleanup_packets_older_than(days) when is_integer(days) and days > 0 do
    Logger.info("Starting APRS packet cleanup for packets older than #{days} days")

    deleted_count = Packets.clean_packets_older_than(days)

    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{days} days")

    deleted_count
  end
end
