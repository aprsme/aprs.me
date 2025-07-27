defmodule Aprsme.CleanupScheduler do
  @moduledoc """
  GenServer that schedules periodic packet cleanup jobs using Exq.

  This scheduler replaces the Oban cron functionality for packet cleanup.
  It runs every 6 hours by default and enqueues a cleanup job.
  """

  use GenServer

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    config = Application.get_env(:aprsme, :cleanup_scheduler, [])
    enabled = Keyword.get(config, :enabled, true)
    # 6 hours
    interval = Keyword.get(config, :interval, 6 * 60 * 60 * 1000)

    if enabled do
      Logger.info("Starting packet cleanup scheduler with #{interval}ms interval")
      schedule_next_cleanup(interval)
      {:ok, %{interval: interval}}
    else
      Logger.info("Packet cleanup scheduler disabled")
      {:ok, %{interval: nil}}
    end
  end

  @impl true
  def handle_info(:schedule_cleanup, %{interval: interval} = state) when is_integer(interval) do
    Logger.info("Scheduling packet cleanup job")

    # Enqueue cleanup job with Exq
    case Exq.enqueue(Exq, "maintenance", Aprsme.Workers.PacketCleanupWorker, []) do
      {:ok, _job_id} ->
        Logger.info("Packet cleanup job scheduled successfully")

      {:error, reason} ->
        Logger.error("Failed to schedule packet cleanup job: #{inspect(reason)}")
    end

    schedule_next_cleanup(interval)
    {:noreply, state}
  end

  @impl true
  def handle_info(:schedule_cleanup, state) do
    # Scheduler is disabled
    {:noreply, state}
  end

  defp schedule_next_cleanup(interval) do
    Process.send_after(self(), :schedule_cleanup, interval)
  end
end
