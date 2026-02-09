defmodule Aprsme.Cluster.ConnectionManager do
  @moduledoc """
  Manages the APRS-IS connection based on cluster leadership.
  Only starts the connection when this node is elected as leader.
  """
  use GenServer

  alias Aprsme.Cluster.LeaderElection
  alias Aprsme.Is.IsSupervisor

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    # Subscribe to leadership changes
    Phoenix.PubSub.subscribe(Aprsme.PubSub, "cluster:leadership")

    # Check initial leadership state
    delay = Application.get_env(:aprsme, :connection_manager_init_delay, 1000)
    Process.send_after(self(), :check_initial_state, delay)

    {:ok, %{connection_started: false}}
  end

  @impl true
  def handle_info(:check_initial_state, state) do
    if leader_check() do
      Logger.info("This node is the leader, starting APRS-IS connection")
      start_aprs_connection()
      {:noreply, %{state | connection_started: true}}
    else
      Logger.info("This node is not the leader, APRS-IS connection will not be started")
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:leadership_change, node, became_leader}, state) do
    cond do
      became_leader and node == node() and not state.connection_started ->
        Logger.info("This node became the leader, starting APRS-IS connection")
        start_aprs_connection()
        {:noreply, %{state | connection_started: true}}

      not became_leader and node == node() and state.connection_started ->
        Logger.info("This node lost leadership, stopping APRS-IS connection")
        stop_aprs_connection()
        {:noreply, %{state | connection_started: false}}

      true ->
        {:noreply, state}
    end
  end

  defp leader_check do
    LeaderElection.leader?()
  catch
    :exit, _ -> false
  end

  defp start_aprs_connection do
    # Start the APRS-IS connection supervisor if not already started
    case DynamicSupervisor.start_child(
           Aprsme.DynamicSupervisor,
           {IsSupervisor, []}
         ) do
      {:ok, _pid} ->
        Logger.info("APRS-IS connection started successfully")

      {:error, {:already_started, _pid}} ->
        Logger.info("APRS-IS connection was already running")

      {:error, reason} ->
        Logger.error("Failed to start APRS-IS connection: #{inspect(reason)}")
    end
  end

  defp stop_aprs_connection do
    # Find and terminate the APRS-IS connection supervisor
    children = DynamicSupervisor.which_children(Aprsme.DynamicSupervisor)

    Enum.each(children, fn
      {_, pid, _, [IsSupervisor]} when is_pid(pid) ->
        Logger.info("Stopping APRS-IS connection supervisor")
        DynamicSupervisor.terminate_child(Aprsme.DynamicSupervisor, pid)

      _ ->
        :ok
    end)
  end
end
