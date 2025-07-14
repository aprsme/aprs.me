defmodule Aprsme.SystemMonitor do
  @moduledoc """
  Monitors system metrics to help with adaptive performance tuning.
  """
  use GenServer

  require Logger

  @check_interval 5_000
  @min_batch_size 50
  @max_batch_size 500
  @default_batch_size 100

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def get_recommended_batch_size do
    GenServer.call(__MODULE__, :get_batch_size)
  catch
    :exit, {:noproc, _} -> @default_batch_size
  end

  def get_metrics do
    GenServer.call(__MODULE__, :get_metrics)
  catch
    :exit, {:noproc, _} -> default_metrics()
  end

  @impl true
  def init(_opts) do
    schedule_check()

    state = %{
      metrics: default_metrics(),
      batch_size: @default_batch_size,
      history: []
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:get_batch_size, _from, state) do
    {:reply, state.batch_size, state}
  end

  @impl true
  def handle_call(:get_metrics, _from, state) do
    {:reply, state.metrics, state}
  end

  @impl true
  def handle_info(:check_system, state) do
    metrics = collect_metrics()
    new_batch_size = calculate_optimal_batch_size(metrics, state)

    # Keep history for trend analysis (last 12 data points = 1 minute)
    history = Enum.take([metrics | state.history], 12)

    # Emit telemetry events for LiveDashboard
    emit_telemetry_events(metrics, new_batch_size)

    new_state = %{state | metrics: metrics, batch_size: new_batch_size, history: history}

    schedule_check()
    {:noreply, new_state}
  end

  defp schedule_check do
    Process.send_after(self(), :check_system, @check_interval)
  end

  defp collect_metrics do
    # Memory metrics
    memory_info = :erlang.memory()
    total_memory = memory_info[:total]
    process_memory = memory_info[:processes]
    binary_memory = memory_info[:binary]

    # CPU metrics
    scheduler_count = :erlang.system_info(:schedulers_online)

    # Parse load averages more robustly
    load_values =
      ~c"uptime | awk -F'load average:' '{print $2}'"
      |> :os.cmd()
      |> to_string()
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&parse_float/1)

    # Ensure we have 3 values, defaulting to 0.0 if missing
    {load1, load5, load15} =
      case load_values do
        [l1, l5, l15 | _] -> {l1, l5, l15}
        [l1, l5] -> {l1, l5, 0.0}
        [l1] -> {l1, 0.0, 0.0}
        [] -> {0.0, 0.0, 0.0}
      end

    # Process metrics
    process_count = :erlang.system_info(:process_count)

    # Database pool metrics
    db_pool_status = get_db_pool_status()

    # Calculate memory pressure (0.0 to 1.0)
    memory_pressure = calculate_memory_pressure(memory_info)

    # Calculate CPU pressure (0.0 to 1.0)
    cpu_pressure = min(1.0, load1 / scheduler_count)

    %{
      memory: %{
        total: total_memory,
        process: process_memory,
        binary: binary_memory,
        pressure: memory_pressure
      },
      cpu: %{
        load1: load1,
        load5: load5,
        load15: load15,
        schedulers: scheduler_count,
        pressure: cpu_pressure
      },
      processes: %{
        count: process_count,
        pressure: min(1.0, process_count / 50_000)
      },
      db_pool: db_pool_status,
      timestamp: DateTime.utc_now()
    }
  end

  defp parse_float(str) do
    case Float.parse(str) do
      {float, _} -> float
      :error -> 0.0
    end
  end

  defp calculate_memory_pressure(memory_info) do
    # Get app memory for pressure calculation
    app_memory = memory_info[:total]

    # Assume 4GB available memory as baseline
    available_memory = 4 * 1024 * 1024 * 1024

    # Calculate pressure based on usage
    min(1.0, app_memory / available_memory)
  end

  defp get_db_pool_status do
    pool_config = Aprsme.Repo.config()[:pool_size] || 10

    # Get pool telemetry if available
    :telemetry.execute([:aprsme, :repo, :pool], %{}, %{})

    %{
      size: pool_config,
      # Would need actual telemetry
      available: pool_config,
      # Placeholder
      pressure: 0.3
    }
  rescue
    _ -> %{size: 10, available: 7, pressure: 0.3}
  end

  defp calculate_optimal_batch_size(metrics, state) do
    # Base factors
    memory_factor = 1.0 - metrics.memory.pressure
    cpu_factor = 1.0 - metrics.cpu.pressure
    db_factor = 1.0 - metrics.db_pool.pressure

    # Historical trend analysis
    trend_factor = calculate_trend_factor(state.history)

    # Weighted combination
    combined_factor =
      memory_factor * 0.4 +
        cpu_factor * 0.3 +
        db_factor * 0.2 +
        trend_factor * 0.1

    # Calculate new batch size
    target_size =
      @min_batch_size +
        round(combined_factor * (@max_batch_size - @min_batch_size))

    # Apply smoothing to avoid rapid changes
    current_size = state.batch_size
    step = round((target_size - current_size) * 0.3)
    new_size = current_size + step

    # Ensure within bounds
    new_size
    |> max(@min_batch_size)
    |> min(@max_batch_size)
  end

  defp calculate_trend_factor(history) when length(history) < 3, do: 0.5

  defp calculate_trend_factor(history) do
    # Analyze recent pressure trends
    recent_pressures =
      history
      |> Enum.take(3)
      |> Enum.map(fn m ->
        (m.memory.pressure + m.cpu.pressure + m.db_pool.pressure) / 3
      end)

    case recent_pressures do
      [p1, p2, p3] when p1 > p2 and p2 > p3 ->
        # Pressure increasing, reduce batch size
        0.2

      [p1, p2, p3] when p1 < p2 and p2 < p3 ->
        # Pressure decreasing, increase batch size
        0.8

      _ ->
        # Stable
        0.5
    end
  end

  defp default_metrics do
    %{
      memory: %{total: 0, process: 0, binary: 0, pressure: 0.5},
      cpu: %{load1: 1.0, load5: 1.0, load15: 1.0, schedulers: 1, pressure: 0.5},
      processes: %{count: 1000, pressure: 0.5},
      db_pool: %{size: 10, available: 7, pressure: 0.3},
      timestamp: DateTime.utc_now()
    }
  end

  defp emit_telemetry_events(metrics, batch_size) do
    # Memory metrics
    :telemetry.execute(
      [:aprsme, :system, :memory],
      %{
        total: metrics.memory.total,
        process: metrics.memory.process,
        binary: metrics.memory.binary,
        pressure: metrics.memory.pressure
      },
      %{}
    )

    # CPU metrics
    :telemetry.execute(
      [:aprsme, :system, :cpu],
      %{
        load1: metrics.cpu.load1,
        load5: metrics.cpu.load5,
        load15: metrics.cpu.load15,
        pressure: metrics.cpu.pressure
      },
      %{schedulers: metrics.cpu.schedulers}
    )

    # Process metrics
    :telemetry.execute(
      [:aprsme, :system, :processes],
      %{
        count: metrics.processes.count,
        pressure: metrics.processes.pressure
      },
      %{}
    )

    # Database pool metrics
    :telemetry.execute(
      [:aprsme, :system, :db_pool],
      %{
        size: metrics.db_pool.size,
        available: metrics.db_pool.available,
        pressure: metrics.db_pool.pressure
      },
      %{}
    )

    # Batch size metrics
    :telemetry.execute(
      [:aprsme, :system, :batch_size],
      %{
        current: batch_size,
        min: @min_batch_size,
        max: @max_batch_size
      },
      %{}
    )
  end
end
