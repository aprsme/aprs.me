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
        leader_node = if leader_pid, do: node(leader_pid)
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
        # Check if the registered process is alive and on a connected node
        if Process.alive?(pid) and node(pid) in [node() | Node.list()] do
          # Process is alive and on a connected node - leave it alone
          :ok
        else
          # Process is dead or on a disconnected node - unregister it
          Logger.info("Cleaning up stale leader registration for dead/disconnected process #{inspect(pid)}")
          :global.unregister_name(@election_key)
        end
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
