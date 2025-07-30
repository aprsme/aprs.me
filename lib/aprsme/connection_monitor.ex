defmodule Aprsme.ConnectionMonitor do
  @moduledoc """
  Monitors system load and LiveView connections to implement connection draining
  when load is imbalanced across cluster nodes.
  """
  use GenServer

  require Logger

  @check_interval to_timeout(second: 30)
  # Start draining if CPU usage > 70%
  @cpu_threshold 0.7
  # Drain if this node has 2x more connections than average
  @connection_imbalance_ratio 2.0
  # Drain 10% of connections when triggered
  @drain_percentage 0.1

  defstruct [
    :node_stats,
    :local_connections,
    :draining,
    :last_check
  ]

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    # Only start if clustering is enabled
    if cluster_enabled?() do
      schedule_check()

      {:ok,
       %__MODULE__{
         node_stats: %{},
         local_connections: 0,
         draining: false,
         last_check: System.monotonic_time(:second)
       }}
    else
      :ignore
    end
  end

  @doc """
  Register a new LiveView connection
  """
  def register_connection do
    if cluster_enabled?() do
      GenServer.cast(__MODULE__, :register_connection)
    end
  end

  @doc """
  Unregister a LiveView connection
  """
  def unregister_connection do
    if cluster_enabled?() do
      GenServer.cast(__MODULE__, :unregister_connection)
    end
  end

  @doc """
  Check if this node should accept new connections
  """
  def accepting_connections? do
    if cluster_enabled?() do
      GenServer.call(__MODULE__, :accepting_connections?)
    else
      true
    end
  end

  @doc """
  Get current node statistics
  """
  def get_stats do
    if cluster_enabled?() do
      GenServer.call(__MODULE__, :get_stats)
    else
      %{connections: 0, cpu: 0.0, memory: 0.0}
    end
  end

  @impl true
  def handle_cast(:register_connection, state) do
    {:noreply, %{state | local_connections: state.local_connections + 1}}
  end

  @impl true
  def handle_cast(:unregister_connection, state) do
    {:noreply, %{state | local_connections: max(0, state.local_connections - 1)}}
  end

  @impl true
  def handle_call(:accepting_connections?, _from, state) do
    {:reply, not state.draining, state}
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    stats = %{
      connections: state.local_connections,
      cpu: get_cpu_usage(),
      memory: get_memory_usage(),
      draining: state.draining
    }

    {:reply, stats, state}
  end

  @impl true
  def handle_info(:check_load, state) do
    new_state =
      state
      |> gather_cluster_stats()
      |> analyze_load()
      |> maybe_trigger_draining()

    schedule_check()
    {:noreply, new_state}
  end

  defp gather_cluster_stats(state) do
    # Get stats from all nodes
    nodes = [Node.self() | Node.list()]

    node_stats =
      Enum.reduce(nodes, %{}, fn node, acc ->
        stats = :rpc.call(node, __MODULE__, :get_stats, [])

        case stats do
          {:badrpc, _} -> acc
          stats -> Map.put(acc, node, stats)
        end
      end)

    %{state | node_stats: node_stats}
  end

  defp analyze_load(state) do
    local_stats = Map.get(state.node_stats, Node.self(), %{})
    cpu_usage = Map.get(local_stats, :cpu, 0.0)

    # Calculate average connections across cluster
    total_connections = Enum.sum(for {_, stats} <- state.node_stats, do: Map.get(stats, :connections, 0))
    node_count = map_size(state.node_stats)
    avg_connections = if node_count > 0, do: total_connections / node_count, else: 0

    # Determine if we should be draining
    should_drain =
      cond do
        # High CPU usage
        cpu_usage > @cpu_threshold ->
          Logger.info("Node #{Node.self()} CPU usage high: #{Float.round(cpu_usage * 100, 1)}%")
          true

        # Too many connections compared to average
        node_count > 1 and state.local_connections > avg_connections * @connection_imbalance_ratio ->
          Logger.info(
            "Node #{Node.self()} has #{state.local_connections} connections, avg: #{Float.round(avg_connections, 1)}"
          )

          true

        # Otherwise, stop draining if we were
        true ->
          false
      end

    %{state | draining: should_drain}
  end

  defp maybe_trigger_draining(%{draining: true, local_connections: connections} = state) when connections > 0 do
    # Calculate how many connections to drain
    to_drain = round(connections * @drain_percentage)
    # Drain 1-10 connections at a time
    to_drain = max(1, min(to_drain, 10))

    Logger.info("Draining #{to_drain} connections from node #{Node.self()}")

    # Broadcast drain event to LiveViews
    Phoenix.PubSub.broadcast(
      Aprsme.PubSub,
      "connection:drain:#{Node.self()}",
      {:drain_connections, to_drain}
    )

    state
  end

  defp maybe_trigger_draining(state), do: state

  defp get_cpu_usage do
    # Get CPU usage from scheduler utilization
    # Use Erlang's cpu_sup if available, otherwise estimate from scheduler utilization
    case :cpu_sup.util() do
      util when is_number(util) ->
        util / 100.0

      _ ->
        # Fallback to scheduler wall time
        :scheduler_wall_time_all
        |> :erlang.statistics()
        |> Enum.map(fn {_, active, total} ->
          if total > 0, do: active / total, else: 0.0
        end)
        |> Enum.sum()
        |> Kernel./(System.schedulers_online())
    end
  rescue
    _ -> 0.0
  end

  defp get_memory_usage do
    # Get memory usage as a percentage
    mem_data = :erlang.memory()
    total = Keyword.get(mem_data, :total, 0)
    system = Keyword.get(mem_data, :system, 0)

    if system > 0 do
      total / system
    else
      0.0
    end
  rescue
    _ -> 0.0
  end

  defp schedule_check do
    Process.send_after(self(), :check_load, @check_interval)
  end

  defp cluster_enabled? do
    Application.get_env(:aprsme, :cluster_enabled, false)
  end
end
