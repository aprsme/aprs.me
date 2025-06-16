defmodule Aprs.Workers.PacketCleanupWorker do
  @moduledoc """
  Oban worker for cleaning up old APRS packet data.

  This worker is responsible for:
  1. Removing packets older than the retention period (30 days by default)
  2. Removing old packets from the displayed map (older than 1 hour)
  3. Logging statistics about the cleanup operation
  """

  use Oban.Worker, queue: :maintenance, max_attempts: 3

  # Import modules needed for database operations

  alias Aprs.Packet
  alias Aprs.Packets
  alias Aprs.Repo

  require Logger

  @impl Oban.Worker
  def perform(%Oban.Job{args: _args}) do
    Logger.info("Starting scheduled APRS packet cleanup")

    # Count packets before cleanup for statistics
    _total_before = count_total_packets()

    # Perform the cleanup of old packets (older than 30 days)
    deleted_count = cleanup_old_packets()

    # Log results
    retention_days = Application.get_env(:aprs, :packet_retention_days, 30)
    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{retention_days} days")

    # Return success
    :ok
  end

  # Schedule this job to run daily
  def schedule_cleanup do
    %{}
    |> Oban.Job.new(worker: __MODULE__, queue: :maintenance)
    |> Oban.insert()
  end

  # Schedule this job to run daily at midnight UTC
  def schedule_daily_cleanup do
    # Calculate when the next midnight UTC is
    now = DateTime.utc_now()
    tomorrow = Date.add(now, 1)

    next_midnight = %{
      now
      | year: tomorrow.year,
        month: tomorrow.month,
        day: tomorrow.day,
        hour: 0,
        minute: 0,
        second: 0,
        microsecond: {0, 6}
    }

    # Schedule the job
    %{}
    |> Oban.Job.new(worker: __MODULE__, queue: :maintenance)
    |> Oban.insert(scheduled_at: next_midnight)
  end

  defp count_total_packets do
    Repo.aggregate(Packet, :count, :id)
  end

  defp cleanup_old_packets do
    # Use the existing function from Packets context
    Packets.clean_old_packets()
  end
end
