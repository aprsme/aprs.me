defmodule AprsmeWeb.MapLive.DisplayManager do
  @moduledoc """
  Handles heat maps, markers, and zoom threshold display logic.
  """

  alias Aprsme.Packets.Clustering
  alias AprsmeWeb.Live.Shared.BoundsUtils
  alias AprsmeWeb.MapLive.DataBuilder
  alias Phoenix.LiveView
  alias Phoenix.LiveView.Socket

  # Zoom level at or below which the map switches to heat map mode
  @heat_map_max_zoom 8

  @doc """
  Handle zoom threshold crossing between heat map and marker modes.
  When tracking a callsign at low zoom, show trail line instead of heat map.
  """
  @spec handle_zoom_threshold_crossing(Socket.t(), integer()) :: Socket.t()
  def handle_zoom_threshold_crossing(socket, zoom) do
    tracked_callsign = socket.assigns.tracked_callsign || ""

    if zoom <= @heat_map_max_zoom do
      # Switching to low zoom mode
      socket = LiveView.push_event(socket, "clear_all_markers", %{})

      # If tracking a callsign, show trail line; otherwise show heat map
      if tracked_callsign == "" do
        send_heat_map_for_current_bounds(socket)
      else
        send_trail_line_for_tracked_callsign(socket)
      end
    else
      # Switching to markers
      trigger_marker_display(socket)
    end
  end

  @doc """
  Check if zoom level crosses the threshold between heat map and marker modes.
  """
  @spec crossing_zoom_threshold?(integer(), integer()) :: boolean()
  def crossing_zoom_threshold?(old_zoom, new_zoom) do
    (old_zoom <= @heat_map_max_zoom and new_zoom > @heat_map_max_zoom) or
      (old_zoom > @heat_map_max_zoom and new_zoom <= @heat_map_max_zoom)
  end

  @doc """
  Send heat map data for current bounds.
  """
  @spec send_heat_map_for_current_bounds(Socket.t()) :: Socket.t()
  def send_heat_map_for_current_bounds(socket) do
    # Get all packets within current bounds
    all_packets =
      Map.values(socket.assigns.visible_packets) ++
        Map.values(socket.assigns.historical_packets)

    # Filter by bounds
    filtered_packets =
      all_packets
      |> BoundsUtils.filter_packets_by_bounds(socket.assigns.map_bounds)
      |> Enum.uniq_by(fn packet ->
        Map.get(packet, :id) || Map.get(packet, "id")
      end)

    send_heat_map_for_packets(socket, filtered_packets)
  end

  @doc """
  Send heat map data for specific packets.
  """
  @spec send_heat_map_for_packets(Socket.t(), list()) :: Socket.t()
  def send_heat_map_for_packets(socket, packets) do
    # Get clustering data
    case Clustering.cluster_packets(packets, socket.assigns.map_zoom) do
      {:heat_map, heat_points} ->
        LiveView.push_event(socket, "show_heat_map", %{heat_points: heat_points})

      {:raw_packets, _packets} ->
        # Shouldn't happen at zoom <= 8, but handle it anyway
        socket
    end
  end

  @doc """
  Send trail line data for tracked callsign.
  Connects all position points chronologically, regardless of distance.
  """
  @spec send_trail_line_for_tracked_callsign(Socket.t()) :: Socket.t()
  def send_trail_line_for_tracked_callsign(socket) do
    callsign = socket.assigns.tracked_callsign
    threshold = socket.assigns.packet_age_threshold

    packets =
      socket.assigns.visible_packets
      |> Map.values()
      |> Enum.filter(fn packet ->
        sender = Map.get(packet, :sender) || Map.get(packet, "sender")
        received_at = Map.get(packet, :received_at) || Map.get(packet, "received_at")

        String.upcase(sender) == String.upcase(callsign) &&
          DateTime.compare(received_at, threshold) != :lt
      end)
      |> Enum.sort_by(
        fn packet ->
          received_at = Map.get(packet, :received_at) || Map.get(packet, "received_at")
          DateTime.to_unix(received_at, :microsecond)
        end,
        :asc
      )

    if Enum.empty?(packets) do
      socket
    else
      points =
        Enum.map(packets, fn packet ->
          lat = Map.get(packet, :lat) || Map.get(packet, "lat")
          lon = Map.get(packet, :lon) || Map.get(packet, "lon")
          received_at = Map.get(packet, :received_at) || Map.get(packet, "received_at")

          %{
            lat: Aprsme.EncodingUtils.to_float(lat) || 0.0,
            lng: Aprsme.EncodingUtils.to_float(lon) || 0.0,
            timestamp: DateTime.to_iso8601(received_at)
          }
        end)

      LiveView.push_event(socket, "show_trail_line", %{
        callsign: callsign,
        points: points
      })
    end
  end

  @doc """
  Send heat map data for filtered packets.
  """
  @spec send_heat_map_data(Socket.t(), map()) :: Socket.t()
  def send_heat_map_data(socket, filtered_packets) do
    # Convert map of packets to list
    packet_list = Map.values(filtered_packets)
    send_heat_map_for_packets(socket, packet_list)
  end

  @doc """
  Trigger marker display mode.
  """
  @spec trigger_marker_display(Socket.t()) :: Socket.t()
  def trigger_marker_display(socket) do
    # Clear heat map and show markers
    socket = LiveView.push_event(socket, "show_markers", %{})

    # Re-send all visible packets as markers
    visible_packets_list = DataBuilder.build_packet_data_list_from_map(socket.assigns.visible_packets, true, socket)

    socket = add_markers_if_any(socket, visible_packets_list)

    # Trigger historical packet reload for markers
    start_progressive_historical_loading(socket)
  end

  @doc """
  Add markers to map if any exist.
  """
  @spec add_markers_if_any(Socket.t(), list()) :: Socket.t()
  def add_markers_if_any(socket, []), do: socket

  def add_markers_if_any(socket, markers) do
    LiveView.push_event(socket, "add_markers", %{markers: markers})
  end

  @doc """
  Remove multiple markers from map.
  """
  @spec remove_markers_batch(Socket.t(), list()) :: Socket.t()
  def remove_markers_batch(socket, []), do: socket

  def remove_markers_batch(socket, marker_ids) do
    LiveView.push_event(socket, "remove_markers_batch", %{ids: marker_ids})
  end

  # Placeholder for historical loading function - this should be moved to HistoricalLoader
  defp start_progressive_historical_loading(socket) do
    # This function should be moved to HistoricalLoader module
    socket
  end
end
