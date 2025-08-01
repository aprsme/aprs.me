defmodule Aprsme.Cluster.LeaderElection do
  @moduledoc """
  Manages leader election for APRS-IS connection using distributed Erlang.
  Only the elected leader will maintain the APRS-IS connection.
  """
  use GenServer

  require Logger

  @election_key {:aprs_is_leader, __MODULE__}
  @check_interval 5_000

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def is_leader? do
    GenServer.call(__MODULE__, :is_leader?)
  end

  def current_leader do
    GenServer.call(__MODULE__, :current_leader)
  end

  @doc """
  Gets APRS-IS status from across the entire cluster.
  Returns the status from whichever node has an active connection.
  """
  def get_cluster_aprs_status do
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    if cluster_enabled do
      get_cluster_wide_status()
    else
      # Non-clustered mode - just return local status
      Aprsme.Is.get_status()
    end
  end

  @impl true
  def init(_opts) do
    Logger.info("Starting leader election process")

    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    if cluster_enabled do
      Logger.info("Clustering enabled - waiting for cluster formation before leader election")
      # Wait longer for cluster to form, then check periodically
      Process.send_after(self(), :check_cluster_and_elect, 2_000)
    else
      Logger.info("Clustering disabled - proceeding with immediate leader election")
      # Non-clustered mode - elect immediately
      Process.send_after(self(), :attempt_election, 100)
    end

    # Schedule periodic checks
    Process.send_after(self(), :check_leadership, @check_interval)

    {:ok, %{is_leader: false, leader_node: nil, cluster_enabled: cluster_enabled}}
  end

  @impl true
  def handle_info(:check_cluster_and_elect, state) do
    connected_nodes = Node.list()

    if length(connected_nodes) > 0 do
      Logger.info("Cluster formed with #{length(connected_nodes)} other nodes: #{inspect(connected_nodes)}")
      Logger.info("Proceeding with leader election")
      Process.send_after(self(), :attempt_election, 100)
      {:noreply, state}
    else
      Logger.info("Cluster not yet formed - waiting...")
      # Check again in 2 seconds
      Process.send_after(self(), :check_cluster_and_elect, 2_000)
      {:noreply, state}
    end
  end

  @impl true
  def handle_info(:attempt_election, state) do
    # First, try to clean up any stale registrations
    cleanup_stale_registrations()

    case :global.register_name(@election_key, self(), &resolve_conflict/3) do
      :yes ->
        Logger.info("Elected as APRS-IS connection leader on node #{node()}")
        notify_leadership_change(true)
        {:noreply, %{state | is_leader: true, leader_node: node()}}

      :no ->
        leader_pid = :global.whereis_name(@election_key)
        leader_node = if leader_pid != :undefined and is_pid(leader_pid), do: node(leader_pid)
        Logger.info("Not elected as leader. Current leader is on node #{inspect(leader_node)}")
        {:noreply, %{state | is_leader: false, leader_node: leader_node}}
    end
  end

  @impl true
  def handle_info(:check_leadership, state) do
    # Re-attempt election if we're not leader
    if not state.is_leader do
      Process.send_after(self(), :attempt_election, 100)
    end

    # Schedule next check
    Process.send_after(self(), :check_leadership, @check_interval)

    {:noreply, state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("LeaderElection received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def handle_call(:is_leader?, _from, state) do
    {:reply, state.is_leader, state}
  end

  @impl true
  def handle_call(:current_leader, _from, state) do
    {:reply, state.leader_node, state}
  end

  @impl true
  def terminate(reason, state) do
    if state.is_leader do
      Logger.info("Leader stepping down due to: #{inspect(reason)}")
      :global.unregister_name(@election_key)
      notify_leadership_change(false)
    end

    :ok
  end

  # Conflict resolution - prefer the process on the lexicographically lower node
  defp resolve_conflict(_name, pid1, pid2) do
    node1 = node(pid1)
    node2 = node(pid2)

    Logger.info("Resolving leader conflict between #{node1} and #{node2}")

    # Choose based on node name ordering for deterministic results
    if node1 <= node2 do
      pid1
    else
      pid2
    end
  end

  defp cleanup_stale_registrations do
    case :global.whereis_name(@election_key) do
      :undefined ->
        # No registration exists
        :ok

      pid when is_pid(pid) ->
        pid_node = node(pid)
        connected_nodes = [node() | Node.list()]

        # Check if the PID's node is still connected
        if pid_node in connected_nodes do
          # Node is connected, try to check if process is alive
          try do
            if pid_node == node() do
              # Local PID - use Process.alive?
              if Process.alive?(pid) do
                :ok
              else
                Logger.info("Cleaning up stale leader registration for dead local process #{inspect(pid)}")
                :global.unregister_name(@election_key)
              end
            else
              # Remote PID - use RPC to check if alive
              case :rpc.call(pid_node, Process, :alive?, [pid]) do
                true ->
                  :ok

                false ->
                  Logger.info("Cleaning up stale leader registration for dead remote process #{inspect(pid)}")
                  :global.unregister_name(@election_key)

                {:badrpc, _reason} ->
                  Logger.info("Cleaning up stale leader registration for unreachable process #{inspect(pid)}")
                  :global.unregister_name(@election_key)
              end
            end
          rescue
            _error ->
              Logger.info("Cleaning up stale leader registration for problematic process #{inspect(pid)}")
              :global.unregister_name(@election_key)
          end
        else
          # Node is disconnected - clean up the registration
          Logger.info("Cleaning up stale leader registration for disconnected node #{pid_node}")
          :global.unregister_name(@election_key)
        end
    end
  end

  defp get_cluster_wide_status do
    all_nodes = [node() | Node.list()]

    # Check each node for APRS-IS connection status
    connected_statuses =
      all_nodes
      |> Enum.map(&get_node_status/1)
      |> Enum.filter(fn status -> status.connected end)

    case connected_statuses do
      [status | _] ->
        # At least one node is connected - return its status
        # Add cluster info to indicate this is cluster-wide status
        Map.put(status, :cluster_info, %{
          total_nodes: length(all_nodes),
          connected_nodes: length(connected_statuses),
          leader_node: get_leader_node_name(),
          all_nodes: all_nodes |> Enum.map(&to_string/1) |> Enum.sort()
        })

      [] ->
        # No nodes are connected - return local status but mark as cluster-wide
        local_status = Aprsme.Is.get_status()

        Map.put(local_status, :cluster_info, %{
          total_nodes: length(all_nodes),
          connected_nodes: 0,
          leader_node: get_leader_node_name(),
          all_nodes: all_nodes |> Enum.map(&to_string/1) |> Enum.sort()
        })
    end
  end

  defp get_node_status(node_name) do
    if node_name == node() do
      # Local node - call directly
      Aprsme.Is.get_status()
    else
      # Remote node - use RPC
      case :rpc.call(node_name, Aprsme.Is, :get_status, [], 5000) do
        {:badrpc, _reason} ->
          # Node unreachable - return disconnected status
          %{connected: false, server: "unreachable", port: 0}

        status when is_map(status) ->
          status

        _ ->
          %{connected: false, server: "error", port: 0}
      end
    end
  rescue
    _error ->
      %{connected: false, server: "error", port: 0}
  end

  defp get_leader_node_name do
    case :global.whereis_name(@election_key) do
      :undefined -> "none"
      pid when is_pid(pid) -> pid |> node() |> to_string()
    end
  end

  defp notify_leadership_change(became_leader) do
    Phoenix.PubSub.broadcast(
      Aprsme.PubSub,
      "cluster:leadership",
      {:leadership_change, node(), became_leader}
    )
  end
end
