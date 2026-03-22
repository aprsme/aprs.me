defmodule Aprsme.StreamingPacketsPubSub do
  @moduledoc """
  Global PubSub system for streaming APRS packets to subscribers based on geographic bounds.

  This module provides efficient real-time packet distribution with geographic filtering,
  allowing multiple GenServers to subscribe to packet streams filtered by lat/long bounds.
  """

  use GenServer

  require Logger

  @table_name :streaming_packets_subscribers

  # Client API

  @doc """
  Starts the StreamingPacketsPubSub GenServer.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Subscribe to packets within the specified geographic bounds.

  ## Parameters
    - pid: The process to receive packet notifications
    - bounds: Map with :north, :south, :east, :west keys defining the geographic area

  ## Returns
    - :ok
  """
  def subscribe_to_bounds(pid, bounds) do
    with_server({:error, :not_running}, fn ->
      GenServer.call(__MODULE__, {:subscribe, pid, bounds})
    end)
  end

  @doc """
  Unsubscribe from packet notifications.
  """
  def unsubscribe(pid) do
    with_server(:ok, fn ->
      GenServer.call(__MODULE__, {:unsubscribe, pid})
    end)
  end

  @doc """
  Broadcast a packet to all subscribers whose bounds contain the packet's location.
  """
  def broadcast_packet(packet) do
    with_server(:ok, fn ->
      GenServer.cast(__MODULE__, {:broadcast, packet})
    end)
  end

  @doc """
  List all active subscribers and their bounds.
  """
  def list_subscribers do
    with_server([], fn ->
      GenServer.call(__MODULE__, :list_subscribers)
    end)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    # Create ETS table for fast lookups
    :ets.new(@table_name, [:set, :protected, :named_table, read_concurrency: true])

    # Monitor subscribers for cleanup
    Process.flag(:trap_exit, true)

    # pid => monitor_ref — track monitors to demonitor on unsubscribe/update
    {:ok, %{monitors: %{}}}
  end

  @impl true
  def handle_call({:subscribe, pid, bounds}, _from, state) do
    # Validate bounds
    if valid_bounds?(bounds) do
      # Demonitor old ref if this pid was already subscribed (bounds update)
      state = demonitor_if_exists(state, pid)

      # Monitor the subscriber
      ref = Process.monitor(pid)

      # Store in ETS for fast lookup
      :ets.insert(@table_name, {pid, bounds})

      {:reply, :ok, put_in(state.monitors[pid], ref)}
    else
      {:reply, {:error, :invalid_bounds}, state}
    end
  end

  @impl true
  def handle_call({:unsubscribe, pid}, _from, state) do
    state = demonitor_if_exists(state, pid)
    :ets.delete(@table_name, pid)
    {:reply, :ok, state}
  end

  @impl true
  def handle_call(:list_subscribers, _from, state) do
    subscribers = :ets.tab2list(@table_name)
    {:reply, subscribers, state}
  end

  @impl true
  def handle_cast({:broadcast, packet}, state) do
    # Get packet coordinates
    lat = packet[:latitude] || packet[:lat]
    lon = packet[:longitude] || packet[:lon] || packet[:lng]

    if lat && lon do
      # Get all subscribers — table is small (one entry per LiveView client)
      subscribers = :ets.tab2list(@table_name)

      if test_env?() do
        send_to_matching_subscribers(subscribers, lat, lon, packet, self())
      else
        # Send to matching subscribers using BroadcastTaskSupervisor
        # Collect dead pids to clean up
        server_pid = self()

        Aprsme.BroadcastTaskSupervisor.async_execute(fn ->
          send_to_matching_subscribers(subscribers, lat, lon, packet, server_pid)
        end)
      end
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    # Clean up subscriber when process dies
    :ets.delete(@table_name, pid)
    {:noreply, %{state | monitors: Map.delete(state.monitors, pid)}}
  end

  # Private functions

  defp demonitor_if_exists(state, pid) do
    case Map.pop(state.monitors, pid) do
      {nil, _monitors} ->
        state

      {ref, monitors} ->
        Process.demonitor(ref, [:flush])
        %{state | monitors: monitors}
    end
  end

  defp valid_bounds?(%{north: n, south: s, east: e, west: w}) do
    all_bounds_numeric?(n, s, e, w) and
      n >= s and
      valid_latitude_range?(n) and
      valid_latitude_range?(s) and
      valid_longitude_range?(e) and
      valid_longitude_range?(w)
  end

  defp valid_bounds?(_), do: false

  defp all_bounds_numeric?(n, s, e, w) do
    is_number(n) and is_number(s) and is_number(e) and is_number(w)
  end

  defp valid_latitude_range?(lat) do
    lat >= -90 and lat <= 90
  end

  defp valid_longitude_range?(lon) do
    lon >= -180 and lon <= 180
  end

  defp packet_in_bounds?(lat, lon, %{north: n, south: s, east: e, west: w}) do
    lat_in_bounds = lat >= s and lat <= n

    # Handle longitude wrap-around at international date line
    lon_in_bounds =
      if w > e do
        # Bounds cross the date line
        lon >= w or lon <= e
      else
        # Normal bounds
        lon >= w and lon <= e
      end

    lat_in_bounds and lon_in_bounds
  end

  defp send_to_matching_subscribers(subscribers, lat, lon, packet, _server_pid) do
    # send/2 to a dead PID is a no-op in Erlang (does not crash).
    # Process.monitor :DOWN messages handle cleanup, so no alive? check needed.
    subscribers
    |> Stream.filter(fn {_pid, bounds} -> packet_in_bounds?(lat, lon, bounds) end)
    |> Enum.each(fn {pid, _bounds} ->
      send(pid, {:streaming_packet, packet})
    end)
  end

  defp test_env? do
    Application.get_env(:aprsme, :env) == :test
  end

  defp with_server(default, fun) do
    if Process.whereis(__MODULE__) do
      fun.()
    else
      default
    end
  end
end
