defmodule Aprs.PacketReplay do
  @moduledoc """
  Handles replaying of historical APRS packets alongside live packets.

  This module provides functionality to:
  1. Start a replay session for historical packets
  2. Stream historical packets to a client with timing similar to original reception
  3. Merge historical packet streams with live packets
  4. Control replay parameters (speed, filters, etc.)
  """

  use GenServer

  alias Aprs.Packets
  alias AprsWeb.Endpoint

  require Logger

  @default_replay_window_minutes 60
  @default_replay_speed 5.0

  @type state :: %{
          user_id: String.t(),
          replay_topic: String.t(),
          replay_speed: float(),
          start_time: DateTime.t(),
          end_time: DateTime.t(),
          region: String.t() | nil,
          bounds: map(),
          callsign: String.t() | nil,
          with_position: boolean(),
          limit: pos_integer(),
          paused: boolean(),
          packets_sent: non_neg_integer(),
          replay_started_at: DateTime.t(),
          replay_timer: reference() | nil,
          last_packet_time: DateTime.t() | nil
        }

  # Client API

  @doc """
  Starts a packet replay session for a specific user/connection.

  ## Options
    * `:user_id` - Unique identifier for the user/session (required)
    * `:bounds` - Bounding box for visible map area [min_lon, min_lat, max_lon, max_lat] (required)
    * `:callsign` - Filter packets by callsign (optional)
    * `:start_time` - Start time for replay (default: 60 minutes ago)
    * `:end_time` - End time for replay (default: now)
    * `:replay_speed` - Speed multiplier for replay (default: 5.0)
    * `:limit` - Maximum number of packets to replay (default: 5000)
    * `:with_position` - Only include packets with position data (default: true)
  """
  @spec start_replay(keyword()) :: {:ok, pid()} | {:error, any()}
  def start_replay(opts) do
    user_id = Keyword.fetch!(opts, :user_id)

    # Ensure bounds are provided for map area filtering
    bounds = Keyword.get(opts, :bounds)

    if !bounds do
      raise ArgumentError, "Map bounds are required for packet replay"
    end

    # Convert user_id to process name
    name = via_tuple(user_id)

    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Stops an active replay session for a user.
  """
  @spec stop_replay(String.t()) :: :ok
  def stop_replay(user_id) do
    name = via_tuple(user_id)

    if GenServer.whereis(name) do
      GenServer.stop(name)
    else
      {:error, :not_found}
    end
  end

  @doc """
  Pauses an active replay session.
  """
  @spec pause_replay(String.t()) :: :ok | :already_paused
  def pause_replay(user_id) do
    name = via_tuple(user_id)
    GenServer.call(name, :pause)
  end

  @doc """
  Resumes a paused replay session.
  """
  @spec resume_replay(String.t()) :: :ok
  def resume_replay(user_id) do
    name = via_tuple(user_id)
    GenServer.call(name, :resume)
  end

  @doc """
  Changes the replay speed.
  """
  @spec set_replay_speed(String.t(), number()) :: :ok
  def set_replay_speed(user_id, speed) when is_number(speed) and speed > 0 do
    name = via_tuple(user_id)
    GenServer.call(name, {:set_speed, speed})
  end

  @doc """
  Updates replay filters (region, bounds, callsign, etc.)
  """
  @spec update_filters(String.t(), keyword()) :: :ok
  def update_filters(user_id, filters) when is_list(filters) do
    name = via_tuple(user_id)
    GenServer.call(name, {:update_filters, filters})
  end

  @doc """
  Gets information about the current replay session.
  """
  @spec get_replay_info(String.t()) :: map()
  def get_replay_info(user_id) do
    name = via_tuple(user_id)
    GenServer.call(name, :get_info)
  end

  # Server implementation

  @impl true
  @spec init(keyword()) :: {:ok, state()}
  def init(opts) do
    user_id = Keyword.fetch!(opts, :user_id)

    # Default time range - always use last hour at most
    now = DateTime.utc_now()
    window_minutes = @default_replay_window_minutes

    # Force start time to be at most 1 hour ago
    default_start_time = DateTime.add(now, -window_minutes * 60, :second)
    user_start_time = Keyword.get(opts, :start_time)

    # If user provided a start time, make sure it's not older than 1 hour
    effective_start_time =
      if user_start_time && DateTime.before?(user_start_time, default_start_time) do
        default_start_time
      else
        user_start_time || default_start_time
      end

    # Set up initial state
    state = %{
      user_id: user_id,
      replay_topic: "replay:#{user_id}",
      replay_speed: Keyword.get(opts, :replay_speed, @default_replay_speed),
      start_time: effective_start_time,
      end_time: Keyword.get(opts, :end_time, now),
      # Not using region - using bounds instead
      region: nil,
      bounds: Keyword.fetch!(opts, :bounds),
      callsign: Keyword.get(opts, :callsign),
      with_position: Keyword.get(opts, :with_position, true),
      limit: Keyword.get(opts, :limit, 5000),
      paused: false,
      packets_sent: 0,
      replay_started_at: now,
      replay_timer: nil,
      last_packet_time: nil
    }

    # Start the replay immediately
    send(self(), :start_replay)

    {:ok, state}
  end

  @impl true
  @spec handle_info(any(), state()) :: {:noreply, state()} | {:stop, :normal, state()}
  def handle_info(:start_replay, state) do
    # Fetch historical packets based on filters
    # Always filter by bounds (visible map area) and limit to packets with position
    replay_opts = [
      start_time: state.start_time,
      end_time: state.end_time,
      limit: state.limit,
      callsign: state.callsign,
      with_position: true,
      bounds: state.bounds
    ]

    # Log the start of replay with map bounds
    Logger.info("Starting packet replay for user #{state.user_id} in map area #{inspect(state.bounds)} for the last hour")

    # Send notification to client that replay is starting
    Endpoint.broadcast(state.replay_topic, "replay_started", %{
      total_packets: Packets.get_historical_packet_count(Map.new(replay_opts)),
      start_time: state.start_time,
      end_time: state.end_time,
      replay_speed: state.replay_speed,
      bounds: state.bounds
    })

    # Get packets and start streaming
    replay_opts_map =
      replay_opts
      |> Keyword.put(:playback_speed, state.replay_speed)
      |> Map.new()

    stream = Packets.stream_packets_for_replay(replay_opts_map)

    # Schedule the first packet
    case stream |> Stream.take(1) |> Enum.to_list() do
      [{delay, packet}] ->
        # Convert delay to milliseconds
        delay_ms = trunc(delay * 1000)
        timer = Process.send_after(self(), {:send_packet, packet, stream}, delay_ms)

        {:noreply, %{state | replay_timer: timer, last_packet_time: packet.received_at}}

      [] ->
        # No packets found, end replay immediately
        Endpoint.broadcast(state.replay_topic, "replay_complete", %{
          packets_sent: 0,
          message: "No matching packets found for replay"
        })

        {:stop, :normal, state}
    end
  end

  @impl true
  def handle_info({:send_packet, packet, stream}, %{paused: true} = state) do
    # If paused, reschedule the current packet
    timer = Process.send_after(self(), {:send_packet, packet, stream}, 1000)
    {:noreply, %{state | replay_timer: timer}}
  end

  @impl true
  def handle_info({:send_packet, packet, stream}, state) do
    # Send the packet to the client
    Endpoint.broadcast(state.replay_topic, "historical_packet", %{
      packet: sanitize_packet_for_transport(packet),
      timestamp: packet.received_at,
      is_historical: true
    })

    # Update counter
    new_packets_sent = state.packets_sent + 1

    # Schedule the next packet
    case stream |> Stream.take(1) |> Enum.to_list() do
      [{delay, next_packet}] ->
        # Convert delay to milliseconds
        delay_ms = trunc(delay * 1000)
        timer = Process.send_after(self(), {:send_packet, next_packet, stream}, delay_ms)

        {:noreply,
         %{state | replay_timer: timer, packets_sent: new_packets_sent, last_packet_time: next_packet.received_at}}

      [] ->
        # No more packets, end replay
        Endpoint.broadcast(state.replay_topic, "replay_complete", %{
          packets_sent: new_packets_sent,
          message: "Replay complete"
        })

        {:stop, :normal, state}
    end
  end

  @impl true
  @spec handle_call(any(), {pid(), any()}, state()) :: {:reply, any(), state()}
  def handle_call(:pause, _from, state) do
    if state.replay_timer do
      Process.cancel_timer(state.replay_timer)
    end

    Endpoint.broadcast(state.replay_topic, "replay_paused", %{
      packets_sent: state.packets_sent,
      last_packet_time: state.last_packet_time
    })

    {:reply, :ok, %{state | paused: true, replay_timer: nil}}
  end

  @impl true
  def handle_call(:resume, _from, %{paused: true} = state) do
    # Force the next packet to be sent soon
    send(self(), {:continue_replay})

    Endpoint.broadcast(state.replay_topic, "replay_resumed", %{
      packets_sent: state.packets_sent,
      last_packet_time: state.last_packet_time
    })

    {:reply, :ok, %{state | paused: false}}
  end

  @impl true
  def handle_call(:resume, _from, state) do
    # Already running, just acknowledge
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:set_speed, speed}, _from, state) do
    # Update speed and notify client
    Endpoint.broadcast(state.replay_topic, "replay_speed_changed", %{
      replay_speed: speed
    })

    {:reply, :ok, %{state | replay_speed: speed}}
  end

  @impl true
  def handle_call({:update_filters, filters}, _from, state) do
    # Update filters - this requires restarting the replay
    if state.replay_timer do
      Process.cancel_timer(state.replay_timer)
    end

    # Update state with new filters
    new_state =
      Enum.reduce(filters, state, fn {key, value}, acc ->
        Map.put(acc, key, value)
      end)

    # Always ensure we're filtering by map bounds
    new_state =
      if not Map.has_key?(new_state, :bounds) or is_nil(new_state.bounds) do
        # Keep existing bounds if none provided
        new_state
      else
        # Validate new bounds if provided
        case new_state.bounds do
          [min_lon, min_lat, max_lon, max_lat]
          when is_number(min_lon) and is_number(min_lat) and
                 is_number(max_lon) and is_number(max_lat) ->
            new_state

          _ ->
            # Invalid bounds format, keep old bounds
            %{new_state | bounds: state.bounds}
        end
      end

    # Restart replay with new filters
    send(self(), :start_replay)

    {:reply, :ok, %{new_state | replay_timer: nil, packets_sent: 0}}
  end

  @impl true
  def handle_call(:get_info, _from, state) do
    info = %{
      user_id: state.user_id,
      replay_speed: state.replay_speed,
      start_time: state.start_time,
      end_time: state.end_time,
      bounds: state.bounds,
      callsign: state.callsign,
      with_position: state.with_position,
      packets_sent: state.packets_sent,
      paused: state.paused,
      replay_started_at: state.replay_started_at,
      last_packet_time: state.last_packet_time
    }

    {:reply, info, state}
  end

  @impl true
  @spec terminate(any(), state()) :: :ok
  def terminate(_reason, state) do
    # Clean up any timers
    if state.replay_timer do
      Process.cancel_timer(state.replay_timer)
    end

    # Notify client that replay has ended
    Endpoint.broadcast(state.replay_topic, "replay_stopped", %{
      packets_sent: state.packets_sent,
      message: "Replay stopped"
    })

    :ok
  end

  # Helper functions

  @spec via_tuple(String.t()) :: {:via, Registry, {atom(), String.t()}}
  defp via_tuple(user_id) do
    {:via, Registry, {Aprs.ReplayRegistry, "replay:#{user_id}"}}
  end

  @spec sanitize_packet_for_transport(any()) :: map()
  defp sanitize_packet_for_transport(packet) do
    # Convert to map and ensure all fields are JSON-safe
    packet
    |> Map.from_struct()
    |> Map.drop([:__meta__, :__struct__])
    |> sanitize_map_values()
  end

  @spec sanitize_map_values(map()) :: map()
  defp sanitize_map_values(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {k, v}, acc ->
      Map.put(acc, k, sanitize_value(v))
    end)
  end

  defp sanitize_value(value) when is_map(value) do
    sanitize_map_values(value)
  end

  defp sanitize_value(value) when is_list(value) do
    Enum.map(value, &sanitize_value/1)
  end

  defp sanitize_value(%DateTime{} = dt) do
    DateTime.to_iso8601(dt)
  end

  defp sanitize_value(%NaiveDateTime{} = dt) do
    NaiveDateTime.to_iso8601(dt)
  end

  defp sanitize_value(%Decimal{} = d) do
    Decimal.to_float(d)
  end

  defp sanitize_value(value) do
    value
  end
end
