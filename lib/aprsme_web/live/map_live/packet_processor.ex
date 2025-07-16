defmodule AprsmeWeb.MapLive.PacketProcessor do
  @moduledoc """
  Handles real-time packet processing, filtering, and display logic.
  """

  import Phoenix.Component, only: [assign: 3]

  alias Aprsme.GeoUtils
  alias AprsmeWeb.Live.Shared.BoundsUtils
  alias AprsmeWeb.Live.Shared.CoordinateUtils
  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.MapLive.PacketUtils
  alias Phoenix.LiveView
  alias Phoenix.LiveView.Socket

  @max_visible_packets 1000
  @max_all_packets 2000

  @doc """
  Process a packet for display on the map.
  """
  @spec process_packet_for_display(map(), Socket.t()) :: {:noreply, Socket.t()}
  def process_packet_for_display(packet, socket) do
    {lat, lon, _data_extended} = CoordinateUtils.get_coordinates(packet)
    callsign_key = SharedPacketUtils.get_callsign_key(packet)

    # Update all_packets with memory limit
    all_packets = Map.put(socket.assigns.all_packets, callsign_key, packet)

    all_packets =
      if map_size(all_packets) > @max_all_packets do
        SharedPacketUtils.prune_oldest_packets(all_packets, @max_all_packets)
      else
        all_packets
      end

    socket = assign(socket, :all_packets, all_packets)

    # Handle packet visibility logic
    socket = handle_packet_visibility(packet, lat, lon, callsign_key, socket)
    {:noreply, socket}
  end

  @doc """
  Handle packet visibility logic based on bounds and existing markers.
  """
  @spec handle_packet_visibility(map(), number() | nil, number() | nil, binary(), Socket.t()) :: Socket.t()
  def handle_packet_visibility(packet, lat, lon, callsign_key, socket) do
    cond do
      should_remove_marker?(lat, lon, callsign_key, socket) ->
        remove_marker_from_map(callsign_key, socket)

      should_add_marker?(lat, lon, callsign_key, socket) ->
        handle_valid_postgres_packet(packet, lat, lon, socket)

      should_update_marker?(lat, lon, callsign_key, socket) ->
        # Marker exists and is within bounds - check if there's significant movement
        existing_packet = socket.assigns.visible_packets[callsign_key]
        {existing_lat, existing_lon, _} = CoordinateUtils.get_coordinates(existing_packet)

        # Check if we have valid existing coordinates
        if is_number(existing_lat) and is_number(existing_lon) and
             GeoUtils.significant_movement?(existing_lat, existing_lon, lat, lon, 15) do
          # Significant movement detected (more than 15 meters), update the marker
          handle_valid_postgres_packet(packet, lat, lon, socket)
        else
          # Just GPS drift or invalid coordinates, update the packet data but don't send visual update
          new_visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, packet)
          assign(socket, :visible_packets, new_visible_packets)
        end

      true ->
        socket
    end
  end

  @doc """
  Handle valid postgres packet by adding it to visible packets and displaying it.
  """
  @spec handle_valid_postgres_packet(map(), number() | nil, number() | nil, Socket.t()) :: Socket.t()
  def handle_valid_postgres_packet(packet, _lat, _lon, socket) do
    # Add the packet to visible_packets and push a marker immediately
    callsign_key = SharedPacketUtils.get_callsign_key(packet)

    new_visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, packet)

    # Enforce memory limits
    new_visible_packets =
      if map_size(new_visible_packets) > @max_visible_packets do
        SharedPacketUtils.prune_oldest_packets(new_visible_packets, @max_visible_packets)
      else
        new_visible_packets
      end

    socket = assign(socket, :visible_packets, new_visible_packets)

    # Check zoom level to decide how to display the packet
    if socket.assigns.map_zoom <= 8 do
      # We're in heat map mode - update the heat map with all current data
      send_heat_map_for_current_bounds(socket)
    else
      # We're in marker mode - send individual marker
      marker_data = PacketUtils.build_packet_data(packet, true, get_locale(socket))

      if marker_data do
        # Only show new packet popup if no station popup is currently open
        if socket.assigns.station_popup_open do
          # Send without opening popup to avoid interrupting user
          LiveView.push_event(socket, "new_packet", Map.put(marker_data, :openPopup, false))
        else
          LiveView.push_event(socket, "new_packet", marker_data)
        end
      else
        socket
      end
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

  # Private helper functions

  defp should_remove_marker?(lat, lon, callsign_key, socket) do
    !is_nil(lat) and !is_nil(lon) and
      Map.has_key?(socket.assigns.visible_packets, callsign_key) and
      not within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
  end

  defp should_add_marker?(lat, lon, callsign_key, socket) do
    !is_nil(lat) and !is_nil(lon) and
      not Map.has_key?(socket.assigns.visible_packets, callsign_key) and
      within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
  end

  defp should_update_marker?(lat, lon, callsign_key, socket) do
    !is_nil(lat) and !is_nil(lon) and
      Map.has_key?(socket.assigns.visible_packets, callsign_key) and
      within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
  end

  # Use shared bounds utility
  defp within_bounds?(coords, bounds) do
    BoundsUtils.within_bounds?(coords, bounds)
  end

  # Helper to get locale from socket
  defp get_locale(socket) do
    Map.get(socket.assigns, :locale, "en")
  end

  # Placeholder for heat map function - this should be moved to DisplayManager
  defp send_heat_map_for_current_bounds(socket) do
    # This function should be moved to DisplayManager module
    socket
  end
end
