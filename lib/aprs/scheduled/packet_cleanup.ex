defmodule Aprs.Scheduled.PacketCleanup do
  @moduledoc """
  Scheduled task to clean up old APRS packet data.

  This module is responsible for:
  1. Removing packets older than the retention period (30 days by default)
  2. Logging statistics about the cleanup operation
  """

  use GenServer

  alias Aprs.Packets

  require Logger

  # Run cleanup once per day
  @cleanup_interval_ms 24 * 60 * 60 * 1000

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{})
  end

  @impl true
  def init(state) do
    # Schedule first cleanup
    schedule_cleanup()
    {:ok, state}
  end

  @impl true
  def handle_info(:cleanup, state) do
    # Run the cleanup
    _cleanup_count = perform_cleanup()

    # Schedule next cleanup
    schedule_cleanup()

    {:noreply, state}
  end

  defp perform_cleanup do
    Logger.info("Starting scheduled APRS packet cleanup")

    # Count packets before cleanup for statistics
    _total_before = count_total_packets()

    # Perform the cleanup
    deleted_count = Packets.clean_old_packets()

    # Log results
    retention_days = Application.get_env(:aprs, :packet_retention_days, 30)
    Logger.info("APRS packet cleanup complete: removed #{deleted_count} packets older than #{retention_days} days")

    # Return deleted count
    deleted_count
  end

  defp count_total_packets do
    # Import modules needed for database operations

    Aprs.Repo.aggregate(Aprs.Packet, :count, :id)
  end

  defp schedule_cleanup do
    # Schedule next cleanup
    Process.send_after(self(), :cleanup, @cleanup_interval_ms)
  end
end
