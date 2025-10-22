defmodule Aprsme.CleanupScheduler do
  @moduledoc """
  GenServer that schedules periodic packet cleanup tasks.

  This scheduler runs cleanup tasks directly every 6 hours by default
  without requiring external job queues.
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
    Logger.info("Running packet cleanup task")

    # Run cleanup directly in a supervised task
    Task.start(fn ->
      try do
        Aprsme.Workers.PacketCleanupWorker.perform(%{})
      rescue
        error ->
          Logger.error("Packet cleanup task failed: #{inspect(error)}")
      end
    end)

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
