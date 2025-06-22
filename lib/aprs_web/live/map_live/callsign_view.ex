defmodule AprsWeb.MapLive.CallsignView do
  @moduledoc false
  use AprsWeb, :live_view

  alias Aprs.EncodingUtils
  alias Aprs.Packets
  alias AprsWeb.Endpoint
  alias AprsWeb.MapLive.MapHelpers
  alias AprsWeb.MapLive.PacketUtils

  @default_center %{lat: 39.0, lng: -98.0}
  @default_zoom 4
  @default_replay_speed 1.0

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    # Normalize callsign to uppercase
    normalized_callsign = String.upcase(callsign)

    # Calculate one hour ago for packet age filtering
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    socket =
      assign(socket,
        callsign: normalized_callsign,
        packets: [],
        page_title: "APRS Map - #{normalized_callsign}",
        # Track visible packets by callsign
        visible_packets: %{},
        # Default bounds - will be updated based on packet locations
        map_bounds: %{
          north: 49.0,
          south: 24.0,
          east: -66.0,
          west: -125.0
        },
        map_center: @default_center,
        map_zoom: @default_zoom,
        default_center: @default_center,
        default_zoom: @default_zoom,
        # Replay controls
        replay_active: false,
        replay_speed: @default_replay_speed,
        replay_paused: false,
        replay_packets: [],
        replay_index: 0,
        replay_timer_ref: nil,
        replay_start_time: nil,
        replay_end_time: nil,
        # Map of packet IDs to packet data for historical packets
        historical_packets: %{},
        # Timestamp for filtering out old packets
        packet_age_threshold: one_hour_ago,
        # Flag to indicate if map is ready for replay
        map_ready: false,
        # Flag to prevent multiple replay starts
        replay_started: false,
        # Pending geolocation to zoom to after map is ready
        pending_geolocation: nil,
        # Last known position for auto-zoom
        last_known_position: nil,
        # Flag to track if packets have been loaded
        packets_loaded: false,
        # Latest symbol table ID and code
        latest_symbol_table_id: "/",
        latest_symbol_code: ">"
      )

    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")

      # Don't load packets here - wait for map_ready event
      # socket = load_callsign_packets(socket, normalized_callsign)

      # Schedule regular cleanup of old packets from the map
      Process.send_after(self(), :cleanup_old_packets, 60_000)
      # Auto-start replay after a short delay
      Process.send_after(self(), :auto_start_replay, 2000)

      {:ok, socket}
    else
      {:ok, socket}
    end
  end

  def handle_event("bounds_changed", %{"bounds" => bounds}, socket) do
    handle_bounds_update(bounds, socket)
  end

  def handle_event("update_bounds", %{"bounds" => bounds}, socket) do
    handle_bounds_update(bounds, socket)
  end

  def handle_event("locate_me", _params, socket) do
    socket = push_event(socket, "request_geolocation", %{})
    {:noreply, socket}
  end

  def handle_event("set_location", %{"lat" => lat, "lng" => lng}, socket) do
    lat_float =
      cond do
        is_binary(lat) -> String.to_float(lat)
        is_integer(lat) -> lat / 1.0
        true -> lat
      end

    lng_float =
      cond do
        is_binary(lng) -> String.to_float(lng)
        is_integer(lng) -> lng / 1.0
        true -> lng
      end

    socket =
      socket
      |> assign(map_center: %{lat: lat_float, lng: lng_float}, map_zoom: 12)
      |> push_event("zoom_to_location", %{lat: lat_float, lng: lng_float, zoom: 12})

    {:noreply, socket}
  end

  def handle_event("pause_replay", _params, socket) do
    if socket.assigns.replay_timer_ref do
      Process.cancel_timer(socket.assigns.replay_timer_ref)
    end

    socket =
      assign(socket,
        replay_paused: not socket.assigns.replay_paused,
        replay_timer_ref: nil
      )

    # Resume replay if unpausing
    socket =
      if socket.assigns.replay_paused do
        socket
      else
        Process.send_after(self(), :replay_next_packet, 100)
        socket
      end

    {:noreply, socket}
  end

  def handle_event("adjust_replay_speed", %{"speed" => speed}, socket) do
    speed_float = to_float(speed)
    {:noreply, assign(socket, replay_speed: speed_float)}
  end

  @impl true
  def handle_event("marker_clicked", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("map_ready", _params, socket) do
    socket = assign(socket, map_ready: true)

    # Load callsign packets now that the map is ready
    socket =
      if socket.assigns.packets_loaded do
        socket
      else
        socket
        |> load_callsign_packets(socket.assigns.callsign)
        |> assign(packets_loaded: true)
      end

    # Auto-start replay if it hasn't been started yet
    socket =
      if socket.assigns.replay_started do
        socket
      else
        socket = start_historical_replay(socket)
        assign(socket, replay_started: true, replay_active: true)
      end

    # If we have a pending geolocation, zoom to it after a delay
    socket =
      if socket.assigns.pending_geolocation do
        location = socket.assigns.pending_geolocation
        Process.send_after(self(), {:zoom_to_location, location.lat, location.lng, 12}, 1500)
        socket
      else
        # If we have a last known position, zoom to it after a delay
        if socket.assigns.last_known_position do
          pos = socket.assigns.last_known_position
          Process.send_after(self(), {:zoom_to_location, pos.lat, pos.lng, 12}, 1500)
          socket
        else
          socket
        end
      end

    {:noreply, socket}
  end

  defp handle_bounds_update(bounds, socket) do
    # Convert string keys to atom keys and parse values
    normalized_bounds = %{
      north: to_float(bounds["north"]),
      south: to_float(bounds["south"]),
      east: to_float(bounds["east"]),
      west: to_float(bounds["west"])
    }

    # Remove out-of-bounds visible packets
    new_visible_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_k, packet} -> MapHelpers.within_bounds?(packet, normalized_bounds) end)
      |> Map.new()

    markers_to_remove =
      socket.assigns.visible_packets
      |> Enum.reject(fn {_k, packet} -> MapHelpers.within_bounds?(packet, normalized_bounds) end)
      |> Enum.map(fn {k, _} -> k end)

    socket =
      if markers_to_remove == [] do
        socket
      else
        Enum.reduce(markers_to_remove, socket, fn k, acc ->
          push_event(acc, "remove_marker", %{id: k})
        end)
      end

    socket = assign(socket, map_bounds: normalized_bounds, visible_packets: new_visible_packets)
    {:noreply, socket}
  end

  # Helper function to convert string or float to float
  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_integer(value), do: value * 1.0

  defp to_float(value) when is_binary(value) do
    case Float.parse(value) do
      {float_val, _} -> float_val
      :error -> raise ArgumentError, "Invalid float string: #{value}"
    end
  end

  @impl true
  def handle_info({:zoom_to_location, lat, lng, zoom}, socket) do
    socket = push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: zoom})
    {:noreply, socket}
  end

  def handle_info({:delayed_zoom, %{lat: lat, lng: lng}}, socket) do
    socket = push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: 12})
    {:noreply, socket}
  end

  def handle_info(:auto_start_replay, socket) do
    if not socket.assigns.replay_started and socket.assigns.map_ready do
      socket = start_historical_replay(socket)
      {:noreply, assign(socket, replay_started: true, replay_active: true)}
    else
      if not socket.assigns.map_ready do
        Process.send_after(self(), :auto_start_replay, 1000)
      end

      {:noreply, socket}
    end
  end

  def handle_info(:replay_next_packet, socket), do: handle_replay_next_packet(socket)
  def handle_info(:cleanup_old_packets, socket), do: handle_cleanup_old_packets(socket)

  def handle_info(%Phoenix.Socket.Broadcast{topic: "aprs_messages", event: "packet", payload: payload} = msg, socket) do
    handle_aprs_packet_broadcast(msg, payload, socket)
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  defp handle_aprs_packet_broadcast(_msg, payload, socket) do
    sanitized_packet = EncodingUtils.sanitize_packet(payload)
    sanitized_packet = Map.put_new(sanitized_packet, :received_at, DateTime.utc_now())
    {lat, lng, _} = MapHelpers.get_coordinates(sanitized_packet)

    callsign_key =
      "#{sanitized_packet.base_callsign}#{if sanitized_packet.ssid, do: "-#{sanitized_packet.ssid}", else: ""}"

    if MapHelpers.has_position_data?(sanitized_packet) and
         packet_matches_callsign?(sanitized_packet, socket.assigns.callsign) and
         MapHelpers.within_bounds?(%{lat: lat, lon: lng}, socket.assigns.map_bounds) and
         packet_within_time_threshold?(
           sanitized_packet,
           socket.assigns.packet_age_threshold
         ) do
      packet_data = build_packet_data(sanitized_packet)

      # Remove any previous marker for this callsign
      socket =
        if Map.has_key?(socket.assigns.visible_packets, callsign_key) do
          push_event(socket, "remove_marker", %{id: callsign_key})
        else
          socket
        end

      visible_packets = %{callsign_key => sanitized_packet}

      last_known_position =
        if lat && lng, do: %{lat: lat, lng: lng}, else: socket.assigns.last_known_position

      socket =
        socket
        |> push_event("new_packet", packet_data)
        |> assign(
          visible_packets: visible_packets,
          last_known_position: last_known_position
        )

      {:noreply, socket}
    else
      # Remove marker if it exists and is now out of bounds or expired
      if Map.has_key?(socket.assigns.visible_packets, callsign_key) do
        socket = push_event(socket, "remove_marker", %{id: callsign_key})
        visible_packets = %{}
        {:noreply, assign(socket, visible_packets: visible_packets)}
      else
        {:noreply, socket}
      end
    end
  end

  defp handle_replay_next_packet(socket) do
    if should_continue_replay?(socket) do
      packet = Enum.at(socket.assigns.replay_packets, socket.assigns.replay_index)
      handle_replay_packet(packet, socket)
    else
      {:noreply, socket}
    end
  end

  defp should_continue_replay?(socket) do
    socket.assigns.replay_active and
      not socket.assigns.replay_paused and
      socket.assigns.replay_index < length(socket.assigns.replay_packets)
  end

  defp handle_replay_packet(nil, socket) do
    socket =
      assign(socket,
        replay_active: false,
        replay_paused: false,
        replay_timer_ref: nil,
        replay_index: 0
      )

    {:noreply, socket}
  end

  defp handle_replay_packet(packet, socket) do
    case build_packet_data(packet) do
      nil -> handle_invalid_replay_packet(socket)
      packet_data -> handle_valid_replay_packet(packet, packet_data, socket)
    end
  end

  defp handle_invalid_replay_packet(socket) do
    timer_ref = Process.send_after(self(), :replay_next_packet, 10)

    socket =
      assign(socket,
        replay_index: socket.assigns.replay_index + 1,
        replay_timer_ref: timer_ref
      )

    {:noreply, socket}
  end

  defp handle_valid_replay_packet(packet, packet_data, socket) do
    historical_packets = Map.put(socket.assigns.historical_packets, packet_data["id"], packet)
    socket = push_event(socket, "historical_packet", Map.put(packet_data, :historical, true))
    delay = trunc(1000 / socket.assigns.replay_speed)
    timer_ref = Process.send_after(self(), :replay_next_packet, delay)

    socket =
      assign(socket,
        replay_index: socket.assigns.replay_index + 1,
        replay_timer_ref: timer_ref,
        historical_packets: historical_packets
      )

    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <link
      rel="stylesheet"
      href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
      integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="
      crossorigin=""
    />
    <link
      rel="stylesheet"
      href="https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css"
    />
    <link
      rel="stylesheet"
      href="https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css"
    />
    <script
      src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
      integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="
      crossorigin=""
    >
    </script>
    <script src="https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js">
    </script>

    <style>
      #aprs-map {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        right: 0 !important;
        bottom: 0 !important;
        width: 100vw !important;
        height: 100vh !important;
        z-index: 1 !important;
        min-width: 100vw !important;
        min-height: 100vh !important;
        max-width: 100vw !important;
        max-height: 100vh !important;
      }

      /* Ensure the map container has a defined height */
      .phx-main {
        position: relative;
        height: 100vh;
        overflow: hidden;
      }

      /* Fix for potential layout issues */
      body, html {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }

      .callsign-header {
        position: absolute;
        top: 10px;
        left: 10px;
        z-index: 1000;
        background: rgba(255, 255, 255, 0.95);
        border: 2px solid rgba(0,0,0,0.2);
        border-radius: 6px;
        padding: 8px 12px;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        color: #333;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        min-width: 200px;
      }

      .callsign-title {
        font-size: 16px;
        font-weight: bold;
        margin-bottom: 4px;
      }

      .aprs-symbol-info {
        font-size: 11px;
        color: #666;
        margin-bottom: 2px;
      }

      .nav-links {
        display: flex;
        gap: 12px;
        font-size: 12px;
      }

      .nav-link {
        color: #007cba;
        text-decoration: none;
        font-weight: 500;
        padding: 2px 4px;
        border-radius: 3px;
        transition: background-color 0.2s;
      }

      .nav-link:hover {
        background-color: rgba(0, 124, 186, 0.1);
        text-decoration: none;
      }

      .locate-button {
        position: absolute;
        left: 10px;
        top: 120px;
        z-index: 1000;
        background: white;
        border: 2px solid rgba(0,0,0,0.2);
        border-radius: 4px;
        padding: 5px;
        cursor: pointer;
      }

      .locate-button:hover {
        background: #f4f4f4;
      }

      .replay-controls {
        position: absolute;
        top: 10px;
        right: 10px;
        z-index: 1000;
        background: rgba(255, 255, 255, 0.95);
        border: 2px solid rgba(0,0,0,0.2);
        border-radius: 6px;
        padding: 8px;
        display: flex;
        gap: 8px;
        align-items: center;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        font-size: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      }

      .replay-button {
        background: #007cba;
        color: white;
        border: none;
        border-radius: 4px;
        padding: 4px 8px;
        cursor: pointer;
        font-size: 11px;
      }

      .replay-button:hover {
        background: #005a87;
      }

      .replay-button.active {
        background: #dc3545;
      }

      .speed-control {
        display: flex;
        align-items: center;
        gap: 4px;
      }

      .speed-control input {
        width: 60px;
        padding: 2px 4px;
        border: 1px solid #ccc;
        border-radius: 2px;
        font-size: 11px;
      }

      .aprs-marker {
        background: transparent !important;
        border: none !important;
      }

      .historical-marker {
        opacity: 0.7;
      }

      .aprs-popup {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        font-size: 12px;
        line-height: 1.4;
        max-width: 200px;
      }

      .aprs-callsign {
        font-size: 14px;
        font-weight: bold;
        margin-bottom: 4px;
        color: #333;
      }

      .aprs-comment {
        font-size: 11px;
        color: #444;
        margin: 4px 0;
        font-style: italic;
      }

      .aprs-coords {
        font-size: 10px;
        color: #888;
        margin-top: 4px;
        font-family: monospace;
      }

      .aprs-time {
        font-size: 10px;
        color: #999;
        margin-top: 2px;
      }

      .empty-state {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 1000;
        background: rgba(255, 255, 255, 0.95);
        border: 2px solid rgba(0,0,0,0.1);
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        color: #666;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        max-width: 300px;
      }

      .empty-state h3 {
        margin: 0 0 8px 0;
        color: #333;
        font-size: 18px;
      }

      .empty-state p {
        margin: 0;
        font-size: 14px;
        line-height: 1.4;
      }
    </style>

    <div style="position: relative; width: 100vw; height: 100vh;">
      <div class="callsign-header">
        <div class="callsign-title">{@callsign}</div>

        <div class="nav-links">
          <a href="/" class="nav-link">← Back to Map</a>
          <a href="/packets" class="nav-link">All Packets</a>
          <a href={"/packets/#{String.downcase(@callsign)}"} class="nav-link">{@callsign} Packets</a>
        </div>
      </div>

      <%= if map_size(@visible_packets) == 0 and not @replay_active do %>
        <div class="empty-state">
          <h3>Loading Historical Data</h3>
          <p>
            Loading packet history for {@callsign}...
          </p>
        </div>
      <% end %>

      <div
        id="aprs-map"
        phx-hook="APRSMap"
        phx-update="ignore"
        data-center={Jason.encode!(@map_center || @default_center)}
        data-zoom={@map_zoom || @default_zoom}
      >
      </div>
    </div>
    """
  end

  defp handle_cleanup_old_packets(socket) do
    # Schedule next cleanup
    Process.send_after(self(), :cleanup_old_packets, 60_000)

    # Update packet age threshold
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)
    socket = assign(socket, packet_age_threshold: one_hour_ago)

    # Remove expired packets from visible_packets
    expired_keys =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_key, packet} ->
        not packet_within_time_threshold?(packet, one_hour_ago)
      end)
      |> Enum.map(fn {key, _} -> key end)

    # Only update the client if there are expired markers
    socket =
      if expired_keys == [] do
        socket
      else
        Enum.reduce(expired_keys, socket, fn key, acc ->
          push_event(acc, "remove_marker", %{id: key})
        end)
      end

    # Use Map.drop/2 for better performance
    updated_visible_packets = Map.drop(socket.assigns.visible_packets, expired_keys)
    socket = assign(socket, visible_packets: updated_visible_packets)

    {:noreply, socket}
  end

  defp packet_within_time_threshold?(packet, threshold) do
    received_at = Map.get(packet, :received_at, DateTime.utc_now())

    case received_at do
      %DateTime{} -> DateTime.compare(received_at, threshold) != :lt
      # If no timestamp, assume it's current
      _ -> true
    end
  end

  defp start_historical_replay(socket) do
    # Fetch historical packets for the specific callsign from the last hour
    now = DateTime.utc_now()
    one_hour_ago = DateTime.add(now, -3600, :second)

    packets =
      fetch_historical_packets_for_callsign(
        socket.assigns.callsign,
        one_hour_ago,
        now
      )

    if Enum.empty?(packets) do
      # No historical packets found
      socket
    else
      # Clear any previous historical packets from the map
      socket = push_event(socket, "clear_historical_packets", %{})

      # Sort packets by inserted_at to identify the most recent
      # Convert NaiveDateTime to DateTime if needed for proper comparison
      sorted_packets =
        Enum.sort_by(
          packets,
          fn packet ->
            case packet.inserted_at do
              %NaiveDateTime{} = naive_dt ->
                DateTime.from_naive!(naive_dt, "Etc/UTC")

              %DateTime{} = dt ->
                dt

              _other ->
                DateTime.utc_now()
            end
          end,
          {:desc, DateTime}
        )

      # Filter out packets with unchanged positions (only keep if lat/lon changed)
      unique_position_packets = filter_unique_positions(sorted_packets)

      # Build packet data for all positions, marking which is the most recent
      packet_data_list =
        unique_position_packets
        |> Enum.with_index()
        |> Enum.map(fn {packet, index} ->
          case build_packet_data(packet) do
            nil ->
              nil

            packet_data ->
              # Generate a unique ID for this historical packet
              packet_id =
                "hist_#{if Map.has_key?(packet, :id), do: packet.id, else: System.unique_integer([:positive])}_#{index}"

              is_most_recent = index == 0
              callsign = socket.assigns.callsign

              packet_data
              |> Map.put("id", packet_id)
              |> Map.put("is_historical", true)
              |> Map.put("is_most_recent_for_callsign", is_most_recent)
              |> Map.put("callsign_group", callsign)
              |> Map.put(
                "timestamp",
                case packet.inserted_at do
                  %NaiveDateTime{} = naive_dt ->
                    DateTime.to_unix(DateTime.from_naive!(naive_dt, "Etc/UTC"), :millisecond)

                  %DateTime{} = dt ->
                    DateTime.to_unix(dt, :millisecond)

                  _other ->
                    DateTime.to_unix(DateTime.utc_now(), :millisecond)
                end
              )
          end
        end)
        |> Enum.filter(& &1)

      # Send all historical packets at once
      socket = push_event(socket, "add_historical_packets", %{packets: packet_data_list})

      # Store historical packets in assigns for reference
      historical_packets_map =
        packet_data_list
        |> Enum.zip(unique_position_packets)
        |> Enum.reduce(%{}, fn {packet_data, packet}, acc ->
          Map.put(acc, packet_data["id"], packet)
        end)

      assign(socket,
        historical_packets: historical_packets_map,
        replay_packets: [],
        replay_index: 0,
        replay_start_time: one_hour_ago,
        replay_end_time: now
      )
    end
  end

  defp fetch_historical_packets_for_callsign(callsign, start_time, end_time) do
    Packets.get_packets_for_replay(%{
      callsign: callsign,
      start_time: start_time,
      end_time: end_time,
      limit: 1000
    })
  end

  # Filter packets to only include those with unique positions (lat/lon changed)
  @spec filter_unique_positions([struct()]) :: [struct()]
  defp filter_unique_positions(packets) do
    packets
    |> Enum.reduce([], fn packet, acc ->
      {lat, lng, _} = MapHelpers.get_coordinates(packet)
      process_packet_position(packet, acc, lat, lng)
    end)
    |> Enum.reverse()
  end

  defp process_packet_position(packet, acc, lat, lng) when not is_nil(lat) and not is_nil(lng) do
    check_position_uniqueness(packet, acc)
  end

  defp process_packet_position(_packet, acc, _lat, _lng), do: acc

  defp check_position_uniqueness(packet, []), do: [packet]

  defp check_position_uniqueness(packet, [last_packet | _] = acc) do
    if position_changed?(packet, last_packet) do
      [packet | acc]
    else
      acc
    end
  end

  # Check if position changed significantly between two packets (more than ~1 meter)
  @spec position_changed?(struct(), struct()) :: boolean()
  defp position_changed?(packet1, packet2) do
    {lat1, lng1, _} = MapHelpers.get_coordinates(packet1)
    {lat2, lng2, _} = MapHelpers.get_coordinates(packet2)

    abs(lat1 - lat2) > 0.00001 || abs(lng1 - lng2) > 0.00001
  end

  defp build_packet_data(packet) do
    {lat, lng, _} = MapHelpers.get_coordinates(packet)
    callsign = Map.get(packet, :base_callsign, Map.get(packet, "base_callsign", ""))

    if lat != nil and lng != nil and callsign != "" and callsign != nil do
      build_packet_map(packet, lat, lng, packet.data_extended)
    end
  end

  defp build_packet_map(packet, lat, lng, data_extended) do
    data_extended = data_extended || %{}
    callsign = PacketUtils.generate_callsign(packet)
    {symbol_table_id, symbol_code} = PacketUtils.get_symbol_info(packet)
    timestamp = PacketUtils.get_timestamp(packet)
    comment = PacketUtils.get_packet_field(packet, :comment, "")

    popup = """
    <div class=\"aprs-popup\">
      <div class=\"aprs-callsign\"><strong><a href=\"/#{callsign}\">#{callsign}</a></strong></div>
      <div class=\"aprs-symbol-info\">Symbol: #{symbol_table_id}#{symbol_code}</div>
      #{if comment == "", do: "", else: "<div class=\\\"aprs-comment\\\">#{comment}</div>"}
      <div class=\"aprs-coords\">#{Float.round(PacketUtils.to_float(lat), 4)}, #{Float.round(PacketUtils.to_float(lng), 4)}</div>
      <div class=\"aprs-time\">#{timestamp}</div>
    </div>
    """

    %{
      "id" => callsign,
      "callsign" => callsign,
      "base_callsign" => PacketUtils.get_packet_field(packet, :base_callsign, ""),
      "ssid" => PacketUtils.get_packet_field(packet, :ssid, 0),
      "lat" => PacketUtils.to_float(lat),
      "lng" => PacketUtils.to_float(lng),
      "data_type" => to_string(PacketUtils.get_packet_field(packet, :data_type, "unknown")),
      "path" => PacketUtils.get_packet_field(packet, :path, ""),
      "comment" => comment,
      "data_extended" => PacketUtils.convert_tuples_to_strings(data_extended || %{}),
      "symbol_table_id" => symbol_table_id,
      "symbol_code" => symbol_code,
      "symbol_description" => "Symbol: #{symbol_table_id}#{symbol_code}",
      "timestamp" => timestamp,
      "popup" => popup
    }
  end

  defp packet_matches_callsign?(packet, target_callsign) do
    normalized_packet = normalize_packet_callsign(packet)
    normalized_target = normalize_target_callsign(target_callsign)
    exact_or_base_match?(normalized_packet, normalized_target, packet)
  end

  defp normalize_packet_callsign(packet) do
    base_callsign = packet[:base_callsign] || packet.base_callsign || ""
    ssid = packet[:ssid] || packet.ssid

    case_result =
      case ssid do
        nil -> base_callsign
        "" -> base_callsign
        "0" -> base_callsign
        _ -> "#{base_callsign}-#{ssid}"
      end

    case_result
    |> String.upcase()
    |> String.trim()
  end

  defp normalize_target_callsign(target_callsign) do
    target_callsign
    |> String.upcase()
    |> String.trim()
  end

  defp exact_or_base_match?(normalized_packet, normalized_target, packet) do
    cond do
      normalized_packet == normalized_target ->
        true

      not String.contains?(normalized_target, "-") ->
        base_callsign = packet[:base_callsign] || packet.base_callsign || ""
        String.upcase(String.trim(base_callsign)) == normalized_target

      true ->
        false
    end
  end

  defp load_callsign_packets(socket, callsign) do
    latest_packet =
      %{callsign: callsign}
      |> Packets.get_recent_packets()
      |> Enum.filter(&MapHelpers.has_position_data?/1)
      |> Enum.sort_by(& &1.received_at, {:desc, DateTime})
      |> List.first()

    last_known_position = extract_last_known_position(latest_packet)
    latest_symbol_table_id = extract_latest_symbol_table_id(latest_packet)
    latest_symbol_code = extract_latest_symbol_code(latest_packet)
    visible_packets = build_visible_packets(latest_packet)
    socket = maybe_push_latest_marker(socket, latest_packet)

    assign(socket,
      last_known_position: last_known_position,
      visible_packets: visible_packets,
      latest_symbol_table_id: latest_symbol_table_id,
      latest_symbol_code: latest_symbol_code
    )
  end

  defp extract_last_known_position(nil), do: nil

  defp extract_last_known_position(packet) do
    {lat, lng, _} = MapHelpers.get_coordinates(packet)
    if lat && lng, do: %{lat: lat, lng: lng}
  end

  defp extract_latest_symbol_table_id(%{data_extended: %{symbol_table_id: id}}) when is_binary(id), do: id

  defp extract_latest_symbol_table_id(_), do: "/"

  defp extract_latest_symbol_code(%{data_extended: %{symbol_code: code}}) when is_binary(code), do: code

  defp extract_latest_symbol_code(_), do: ">"

  defp build_visible_packets(nil), do: %{}

  defp build_visible_packets(packet) do
    callsign_key =
      "#{packet.base_callsign}#{if packet.ssid, do: "-#{packet.ssid}", else: ""}"

    %{callsign_key => packet}
  end

  defp maybe_push_latest_marker(socket, nil), do: socket

  defp maybe_push_latest_marker(socket, packet) do
    packet_data = build_packet_data(packet)
    if packet_data, do: push_event(socket, "add_markers", %{markers: [packet_data]}), else: socket
  end
end
