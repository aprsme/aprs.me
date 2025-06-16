defmodule Aprs.Workers.Scheduler do
  @moduledoc """
  Scheduler for Oban jobs.

  This module is responsible for scheduling recurring jobs in the application.
  It should be started as part of the application supervision tree.
  """

  use GenServer

  alias Aprs.Workers.PacketCleanupWorker

  require Logger

  # Start the scheduler on application boot
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{})
  end

  @impl true
  def init(state) do
    # Schedule initial jobs
    schedule_initial_jobs()

    # Return with state
    {:ok, state}
  end

  @impl true
  def handle_info(:schedule_daily_jobs, state) do
    # Schedule daily cleanup job
    schedule_daily_jobs()

    # Schedule this function to run again tomorrow
    schedule_next_day_scheduler()

    {:noreply, state}
  end

  # Schedule jobs when the application starts
  defp schedule_initial_jobs do
    Logger.info("Scheduling initial Oban jobs")

    # Schedule the packet cleanup job
    {:ok, _job} = PacketCleanupWorker.schedule_cleanup()

    # Schedule the daily scheduler to run at midnight
    schedule_next_day_scheduler()
  end

  # Schedule jobs that should run daily
  defp schedule_daily_jobs do
    Logger.info("Scheduling daily Oban jobs")

    # Schedule the packet cleanup job
    {:ok, _job} = PacketCleanupWorker.schedule_cleanup()
  end

  # Schedule the next day's job scheduler to run at midnight UTC
  defp schedule_next_day_scheduler do
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

    seconds_until_midnight = DateTime.diff(next_midnight, now)

    # Schedule this function to run at midnight
    Process.send_after(self(), :schedule_daily_jobs, seconds_until_midnight * 1000)
  end
end
