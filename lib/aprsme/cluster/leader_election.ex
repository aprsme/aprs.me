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

    # Schedule initial election
    Process.send_after(self(), :attempt_election, 100)

    # Schedule periodic checks
    Process.send_after(self(), :check_leadership, @check_interval)

    {:ok, %{is_leader: false, leader_node: nil}}
  end

  @impl true
  def handle_info(:attempt_election, state) do
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

  # Conflict resolution - prefer the process that's been running longer
  defp resolve_conflict(_name, pid1, pid2) do
    info1 = Process.info(pid1, [:registered_name, :current_function])
    info2 = Process.info(pid2, [:registered_name, :current_function])

    Logger.debug("Resolving leader conflict between #{inspect(info1)} and #{inspect(info2)}")

    # Keep the first registered process
    pid1
  end

  defp notify_leadership_change(became_leader) do
    Phoenix.PubSub.broadcast(
      Aprsme.PubSub,
      "cluster:leadership",
      {:leadership_change, node(), became_leader}
    )
  end
end
