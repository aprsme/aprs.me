defmodule AprsmeWeb.MapLive.PacketProcessor do
  @moduledoc """
  Handles real-time packet processing, filtering, and display logic.
  """

  import Phoenix.Component, only: [assign: 3]

  alias Aprsme.GeoUtils
  alias AprsmeWeb.Live.Shared.BoundsUtils
  alias AprsmeWeb.Live.Shared.CoordinateUtils
  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.MapLive.DataBuilder
  alias AprsmeWeb.MapLive.DisplayManager
  alias Phoenix.LiveView
  alias Phoenix.LiveView.Socket

  @max_visible_packets 1000

  @doc """
  Process a packet for display on the map.
  Pushes events directly to the socket (used for single-packet path).
  """
  @spec process_packet_for_display(map(), Socket.t()) :: {:noreply, Socket.t()}
  def process_packet_for_display(packet, socket) do
    {lat, lon, _data_extended} = CoordinateUtils.get_coordinates(packet)
    callsign_key = SharedPacketUtils.get_callsign_key(packet)

    # Handle packet visibility logic
    socket = handle_packet_visibility(packet, lat, lon, callsign_key, socket)

    # Update last update timestamp for real-time display in map sidebar
    socket = assign(socket, :last_update_at, DateTime.utc_now())

    {:noreply, socket}
  end

  @doc """
  Process a packet for batch display. Updates socket state but does NOT push
  events. Returns `{socket, marker_data | nil}` where marker_data is the data
  that should be included in a batched push_event, or nil if no marker needed.
  """
  @spec process_packet_for_marker_data(map(), Socket.t()) :: {Socket.t(), map() | nil}
  def process_packet_for_marker_data(packet, socket) do
    {lat, lon, _data_extended} = CoordinateUtils.get_coordinates(packet)
    callsign_key = SharedPacketUtils.get_callsign_key(packet)

    batch_visibility_action(packet, lat, lon, callsign_key, socket)
  end

  @doc """
  Handle packet visibility logic based on bounds and existing markers.
  """
  @spec handle_packet_visibility(map(), number() | nil, number() | nil, binary(), Socket.t()) :: Socket.t()
  def handle_packet_visibility(packet, lat, lon, callsign_key, socket) do
    visibility_action(packet, lat, lon, callsign_key, socket)
  end

  defp visibility_action(packet, lat, lon, callsign_key, socket) when not is_nil(lat) and not is_nil(lon) do
    in_bounds = within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
    has_marker = Map.has_key?(socket.assigns.visible_packets, callsign_key)
    dispatch_visibility({in_bounds, has_marker}, packet, lat, lon, callsign_key, socket)
  end

  defp visibility_action(_packet, _lat, _lon, _callsign_key, socket), do: socket

  defp dispatch_visibility({false, true}, _packet, _lat, _lon, callsign_key, socket) do
    remove_marker_from_map(callsign_key, socket)
  end

  defp dispatch_visibility({true, false}, packet, lat, lon, _callsign_key, socket) do
    handle_valid_postgres_packet(packet, lat, lon, socket)
  end

  defp dispatch_visibility({true, true}, packet, lat, lon, callsign_key, socket) do
    existing_packet = socket.assigns.visible_packets[callsign_key]
    {existing_lat, existing_lon, _} = CoordinateUtils.get_coordinates(existing_packet)

    if is_number(existing_lat) and is_number(existing_lon) and
         GeoUtils.significant_movement?(existing_lat, existing_lon, lat, lon, 50) do
      handle_valid_postgres_packet(packet, lat, lon, socket)
    else
      new_visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, packet)
      assign(socket, :visible_packets, new_visible_packets)
    end
  end

  defp dispatch_visibility(_, _packet, _lat, _lon, _callsign_key, socket), do: socket

  @doc """
  Handle valid postgres packet by adding it to visible packets and displaying it.
  """
  @spec handle_valid_postgres_packet(map(), number() | nil, number() | nil, Socket.t()) :: Socket.t()
  def handle_valid_postgres_packet(packet, _lat, _lon, socket) do
    {socket, marker_data} = prepare_packet_state(packet, socket)

    cond do
      socket.assigns.map_zoom <= 8 ->
        send_heat_map_for_current_bounds(socket)

      marker_data ->
        send_marker_with_popup_check(socket, marker_data)

      true ->
        socket
    end
  end

  @doc """
  Remove marker from map and update visible packets.
  """
  @spec remove_marker_from_map(binary(), Socket.t()) :: Socket.t()
  def remove_marker_from_map(callsign_key, socket) do
    socket = LiveView.push_event(socket, "remove_marker", %{id: callsign_key})
    new_visible_packets = Map.delete(socket.assigns.visible_packets, callsign_key)
    assign(socket, :visible_packets, new_visible_packets)
  end

  # Batch-mode visibility: same logic as visibility_action but returns
  # {socket, marker_data | nil} without pushing events
  defp batch_visibility_action(packet, lat, lon, callsign_key, socket) when not is_nil(lat) and not is_nil(lon) do
    in_bounds = within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
    has_marker = Map.has_key?(socket.assigns.visible_packets, callsign_key)
    batch_dispatch({in_bounds, has_marker}, packet, lat, lon, callsign_key, socket)
  end

  defp batch_visibility_action(_packet, _lat, _lon, _callsign_key, socket) do
    {socket, nil}
  end

  # Out of bounds but has marker — remove it (still pushes remove_marker since
  # removals are rare and don't benefit from batching)
  defp batch_dispatch({false, true}, _packet, _lat, _lon, callsign_key, socket) do
    socket = remove_marker_from_map(callsign_key, socket)
    {socket, nil}
  end

  # In bounds, no existing marker — add it
  defp batch_dispatch({true, false}, packet, _lat, _lon, _callsign_key, socket) do
    prepare_packet_state(packet, socket)
  end

  # In bounds, existing marker — update if significant movement
  defp batch_dispatch({true, true}, packet, lat, lon, callsign_key, socket) do
    existing_packet = socket.assigns.visible_packets[callsign_key]
    {existing_lat, existing_lon, _} = CoordinateUtils.get_coordinates(existing_packet)

    if is_number(existing_lat) and is_number(existing_lon) and
         GeoUtils.significant_movement?(existing_lat, existing_lon, lat, lon, 50) do
      prepare_packet_state(packet, socket)
    else
      new_visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, packet)
      {assign(socket, :visible_packets, new_visible_packets), nil}
    end
  end

  defp batch_dispatch(_, _packet, _lat, _lon, _callsign_key, socket) do
    {socket, nil}
  end

  # Shared: updates visible_packets state and builds marker data.
  # Returns {socket, marker_data | nil}. Does NOT push events.
  # When an existing visible packet for the same callsign is replaced,
  # the marker_data includes a "convert_from" key with the old callsign_key
  # so the JS can convert the old marker to a historical dot via O(1) lookup.
  defp prepare_packet_state(packet, socket) do
    callsign_key = SharedPacketUtils.get_callsign_key(packet)

    # Check if we're replacing an existing visible packet for this callsign
    had_existing = Map.has_key?(socket.assigns.visible_packets, callsign_key)

    new_visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, packet)

    new_visible_packets =
      if map_size(new_visible_packets) > @max_visible_packets do
        SharedPacketUtils.prune_oldest_packets(new_visible_packets, @max_visible_packets)
      else
        new_visible_packets
      end

    socket = assign(socket, :visible_packets, new_visible_packets)

    marker_data =
      if socket.assigns.map_zoom > 8 do
        data = DataBuilder.build_packet_data(packet, true, get_locale(socket))

        if data && had_existing do
          Map.put(data, "convert_from", callsign_key)
        else
          data
        end
      end

    {socket, marker_data}
  end

  # Private helper functions

  defp within_bounds?(coords, bounds) do
    BoundsUtils.within_bounds?(coords, bounds)
  end

  defp get_locale(socket) do
    Map.get(socket.assigns, :locale, "en")
  end

  defp send_heat_map_for_current_bounds(socket) do
    DisplayManager.send_heat_map_for_current_bounds(socket)
  end

  defp send_marker_with_popup_check(socket, marker_data) do
    # Strip convert_from — the single-packet JS handler uses its own scan logic
    marker_data = Map.delete(marker_data, "convert_from")

    if socket.assigns.station_popup_open do
      LiveView.push_event(socket, "new_packet", Map.put(marker_data, :openPopup, false))
    else
      LiveView.push_event(socket, "new_packet", marker_data)
    end
  end
end
