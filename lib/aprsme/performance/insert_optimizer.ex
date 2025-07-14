defmodule Aprsme.Performance.InsertOptimizer do
  @moduledoc """
  Optimizations specifically for INSERT performance.

  This module contains strategies to improve packet insertion performance:
  - Optimized batch sizing based on system load
  - Reduced index maintenance overhead
  - Streamlined packet preparation
  - Connection pool optimizations
  """

  use GenServer

  require Logger

  # Configuration for INSERT optimization
  @base_batch_size 100
  @max_batch_size 500
  @min_batch_size 50
  # 30 seconds
  @optimization_check_interval 30_000

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Get the optimal batch size for current system conditions.
  """
  def get_optimal_batch_size do
    GenServer.call(__MODULE__, :get_batch_size)
  catch
    :exit, {:noproc, _} -> @base_batch_size
  end

  @doc """
  Get INSERT optimization settings.
  """
  def get_insert_options do
    GenServer.call(__MODULE__, :get_insert_options)
  catch
    :exit, {:noproc, _} -> default_insert_options()
  end

  @doc """
  Record INSERT performance metrics for optimization.
  """
  def record_insert_metrics(batch_size, duration_ms, success_count) do
    GenServer.cast(__MODULE__, {:record_metrics, batch_size, duration_ms, success_count})
  end

  @impl true
  def init(_opts) do
    schedule_optimization_check()

    state = %{
      current_batch_size: @base_batch_size,
      insert_options: default_insert_options(),
      performance_history: [],
      last_optimization: System.monotonic_time(:millisecond)
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:get_batch_size, _from, state) do
    {:reply, state.current_batch_size, state}
  end

  @impl true
  def handle_call(:get_insert_options, _from, state) do
    {:reply, state.insert_options, state}
  end

  @impl true
  def handle_cast({:record_metrics, batch_size, duration_ms, success_count}, state) do
    # Calculate throughput (packets per second)
    throughput = if duration_ms > 0, do: success_count / (duration_ms / 1000), else: 0

    metric = %{
      batch_size: batch_size,
      duration_ms: duration_ms,
      success_count: success_count,
      throughput: throughput,
      timestamp: System.monotonic_time(:millisecond)
    }

    # Keep last 20 metrics for analysis
    new_history = Enum.take([metric | state.performance_history], 20)

    {:noreply, %{state | performance_history: new_history}}
  end

  @impl true
  def handle_info(:optimize_settings, state) do
    new_state = optimize_based_on_performance(state)
    schedule_optimization_check()
    {:noreply, new_state}
  end

  defp schedule_optimization_check do
    Process.send_after(self(), :optimize_settings, @optimization_check_interval)
  end

  defp optimize_based_on_performance(state) do
    if length(state.performance_history) >= 3 do
      # Analyze recent performance
      recent_metrics = Enum.take(state.performance_history, 5)
      total_throughput = recent_metrics |> Enum.map(& &1.throughput) |> Enum.sum()
      avg_throughput = total_throughput / length(recent_metrics)
      total_duration = recent_metrics |> Enum.map(& &1.duration_ms) |> Enum.sum()
      avg_duration = total_duration / length(recent_metrics)

      # Adjust batch size based on performance
      new_batch_size = calculate_optimal_batch_size(avg_throughput, avg_duration, state.current_batch_size)

      # Adjust INSERT options based on system load
      new_insert_options = calculate_insert_options(avg_duration)

      Logger.debug(
        "INSERT optimization: batch_size=#{new_batch_size}, avg_throughput=#{Float.round(avg_throughput, 2)} pps"
      )

      # Emit telemetry for monitoring
      :telemetry.execute([:aprsme, :insert_optimizer, :batch_size], %{value: new_batch_size}, %{})
      :telemetry.execute([:aprsme, :insert_optimizer, :throughput], %{value: avg_throughput}, %{})
      :telemetry.execute([:aprsme, :insert_optimizer, :duration], %{value: avg_duration}, %{})

      if new_batch_size != state.current_batch_size do
        :telemetry.execute([:aprsme, :insert_optimizer, :optimizations], %{count: 1}, %{})
      end

      %{
        state
        | current_batch_size: new_batch_size,
          insert_options: new_insert_options,
          last_optimization: System.monotonic_time(:millisecond)
      }
    else
      state
    end
  end

  defp calculate_optimal_batch_size(avg_throughput, avg_duration, current_batch_size) do
    cond do
      # If throughput is low and duration is high, reduce batch size
      avg_throughput < 50 and avg_duration > 5000 ->
        max(@min_batch_size, round(current_batch_size * 0.8))

      # If throughput is good and duration is acceptable, increase batch size
      avg_throughput > 200 and avg_duration < 2000 ->
        min(@max_batch_size, round(current_batch_size * 1.2))

      # Otherwise keep current size
      true ->
        current_batch_size
    end
  end

  defp calculate_insert_options(avg_duration) do
    base_options = default_insert_options()

    # If INSERTs are taking too long, use more aggressive optimizations
    if avg_duration > 3000 do
      Map.merge(base_options, %{
        returning: false,
        on_conflict: :nothing,
        timeout: 30_000
      })
    else
      base_options
    end
  end

  defp default_insert_options do
    %{
      # Don't return IDs unless needed
      returning: false,
      # Skip conflicts instead of raising
      on_conflict: :nothing,
      # 15 second timeout
      timeout: 15_000
    }
  end
end
