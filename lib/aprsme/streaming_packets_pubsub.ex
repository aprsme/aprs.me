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
    GenServer.call(__MODULE__, {:subscribe, pid, bounds})
  end

  @doc """
  Unsubscribe from packet notifications.
  """
  def unsubscribe(pid) do
    GenServer.call(__MODULE__, {:unsubscribe, pid})
  end

  @doc """
  Broadcast a packet to all subscribers whose bounds contain the packet's location.
  """
  def broadcast_packet(packet) do
    GenServer.cast(__MODULE__, {:broadcast, packet})
  end

  @doc """
  List all active subscribers and their bounds.
  """
  def list_subscribers do
    GenServer.call(__MODULE__, :list_subscribers)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    # Create ETS table for fast lookups
    :ets.new(@table_name, [:set, :protected, :named_table])

    # Monitor subscribers for cleanup
    Process.flag(:trap_exit, true)

    {:ok, %{}}
  end

  @impl true
  def handle_call({:subscribe, pid, bounds}, _from, state) do
    # Validate bounds
    if valid_bounds?(bounds) do
      # Monitor the subscriber
      Process.monitor(pid)

      # Store in ETS for fast lookup
      :ets.insert(@table_name, {pid, bounds})

      {:reply, :ok, state}
    else
      {:reply, {:error, :invalid_bounds}, state}
    end
  end

  @impl true
  def handle_call({:unsubscribe, pid}, _from, state) do
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
      # Find all subscribers whose bounds contain this packet
      subscribers =
        :ets.select(@table_name, [
          {
            {:"$1", :"$2"},
            [],
            [{{:"$1", :"$2"}}]
          }
        ])

      # Send to matching subscribers using BroadcastTaskSupervisor
      # Collect dead pids to clean up
      server_pid = self()

      Aprsme.BroadcastTaskSupervisor.async_execute(fn ->
        send_to_matching_subscribers(subscribers, lat, lon, packet, server_pid)
      end)
    end

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    # Clean up subscriber when process dies
    :ets.delete(@table_name, pid)
    {:noreply, state}
  end

  @impl true
  def handle_info({:cleanup_dead_subscribers, pids}, state) do
    # Clean up dead subscribers from the GenServer process
    Enum.each(pids, fn pid ->
      :ets.delete(@table_name, pid)
    end)

    {:noreply, state}
  end

  # Private functions

  defp valid_bounds?(%{north: n, south: s, east: e, west: w}) do
    is_number(n) and is_number(s) and is_number(e) and is_number(w) and
      n >= s and
      n >= -90 and n <= 90 and
      s >= -90 and s <= 90 and
      e >= -180 and e <= 180 and
      w >= -180 and w <= 180
  end

  defp valid_bounds?(_), do: false

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

  defp send_to_matching_subscribers(subscribers, lat, lon, packet, server_pid) do
    dead_pids =
      subscribers
      |> Stream.filter(fn {_pid, bounds} -> packet_in_bounds?(lat, lon, bounds) end)
      |> Enum.reduce([], fn {pid, _bounds}, acc ->
        send_to_subscriber_if_alive(pid, packet, acc)
      end)

    # Send dead pids back to GenServer for cleanup
    send(server_pid, {:cleanup_dead_subscribers, dead_pids})
  end

  defp send_to_subscriber_if_alive(pid, packet, acc) do
    if Process.alive?(pid) do
      send(pid, {:streaming_packet, packet})
      acc
    else
      # Collect dead pid for cleanup
      [pid | acc]
    end
  end
end
