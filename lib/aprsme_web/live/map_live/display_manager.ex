defmodule AprsmeWeb.MapLive.DisplayManager do
  @moduledoc """
  Handles heat maps, markers, and zoom threshold display logic.
  """

  alias Aprsme.Packets.Clustering
  alias AprsmeWeb.MapLive.BoundsManager
  alias AprsmeWeb.MapLive.DataBuilder
  alias Phoenix.LiveView
  alias Phoenix.LiveView.Socket

  @doc """
  Handle zoom threshold crossing between heat map and marker modes.
  """
  @spec handle_zoom_threshold_crossing(Socket.t(), integer()) :: Socket.t()
  def handle_zoom_threshold_crossing(socket, zoom) do
    if zoom <= 8 do
      # Switching to heat map
      socket
      |> LiveView.push_event("clear_all_markers", %{})
      |> send_heat_map_for_current_bounds()
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
    (old_zoom <= 8 and new_zoom > 8) or (old_zoom > 8 and new_zoom <= 8)
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
      |> BoundsManager.filter_packets_by_bounds(socket.assigns.map_bounds)
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
    Enum.reduce(marker_ids, socket, fn id, acc ->
      LiveView.push_event(acc, "remove_marker", %{id: id})
    end)
  end

  # Placeholder for historical loading function - this should be moved to HistoricalLoader
  defp start_progressive_historical_loading(socket) do
    # This function should be moved to HistoricalLoader module
    socket
  end
end
