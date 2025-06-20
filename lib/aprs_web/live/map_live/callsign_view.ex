defmodule AprsWeb.MapLive.CallsignView do
  use AprsWeb, :live_view

  alias Aprs.EncodingUtils
  alias Aprs.Packets
  alias AprsWeb.Endpoint
  alias Parser.Types.MicE

  @default_center %{lat: 39.0, lng: -98.0}
  @default_zoom 4
  @default_replay_speed 1.0

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
    {:noreply, assign(socket, replay_speed: to_float(speed))}
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

    socket = assign(socket, map_bounds: normalized_bounds)
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

  def handle_info({:zoom_to_location, lat, lng, zoom}, socket) do
    socket = push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: zoom})
    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    case msg do
      {:delayed_zoom, %{lat: lat, lng: lng}} ->
        socket = push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: 12})
        {:noreply, socket}

      :auto_start_replay ->
        # Auto-start replay if it hasn't been started yet and map is ready
        if not socket.assigns.replay_started and socket.assigns.map_ready do
          socket = start_historical_replay(socket)
          {:noreply, assign(socket, replay_started: true, replay_active: true)}
        else
          # If map isn't ready yet, try again in a bit
          if not socket.assigns.map_ready do
            Process.send_after(self(), :auto_start_replay, 1000)
          end

          {:noreply, socket}
        end

      :replay_next_packet ->
        handle_replay_next_packet(socket)

      :cleanup_old_packets ->
        # Clean up packets older than 1 hour from the map display
        handle_cleanup_old_packets(socket)

      %{event: "packet", payload: payload} ->
        # Sanitize the packet to prevent encoding errors
        sanitized_packet = EncodingUtils.sanitize_packet(payload)

        # Add received timestamp if not present
        sanitized_packet = Map.put_new(sanitized_packet, :received_at, DateTime.utc_now())

        # Check if this packet is from our target callsign
        if packet_matches_callsign?(sanitized_packet, socket.assigns.callsign) and
             has_position_data?(sanitized_packet) and
             packet_within_time_threshold?(sanitized_packet, socket.assigns.packet_age_threshold) do
          # Convert to a simple map structure for JSON encoding
          packet_data = build_packet_data(sanitized_packet)

          # Only push if we have valid packet data
          if packet_data do
            # Generate a unique key for this packet
            callsign_key =
              "#{sanitized_packet.base_callsign}#{if sanitized_packet.ssid, do: "-#{sanitized_packet.ssid}", else: ""}"

            # Update visible packets tracking
            visible_packets =
              Map.put(socket.assigns.visible_packets, callsign_key, sanitized_packet)

            # Update last known position
            {lat, lng} = get_coordinates(sanitized_packet)

            last_known_position =
              if lat && lng, do: %{lat: lat, lng: lng}, else: socket.assigns.last_known_position

            # Push the packet to the client-side JavaScript
            socket =
              socket
              |> push_event("new_packet", packet_data)
              |> assign(
                visible_packets: visible_packets,
                last_known_position: last_known_position
              )

            # If this is the first packet and map is ready, zoom to it
            # Or if this is a new position that's significantly different, update zoom
            socket =
              cond do
                map_empty?(socket) and socket.assigns.map_ready and last_known_position ->
                  push_event(socket, "zoom_to_location", %{
                    lat: last_known_position.lat,
                    lng: last_known_position.lng,
                    zoom: 12
                  })

                should_update_zoom?(socket.assigns.last_known_position, last_known_position) and
                    socket.assigns.map_ready ->
                  push_event(socket, "zoom_to_location", %{
                    lat: last_known_position.lat,
                    lng: last_known_position.lng,
                    zoom: 12
                  })

                true ->
                  socket
              end

            {:noreply, socket}
          else
            # Invalid packet data, skip it
            {:noreply, socket}
          end
        else
          # Ignore packets that don't match our callsign or don't have position data
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
    historical_packets = Map.put(socket.assigns.historical_packets, packet_data.id, packet)
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
        <div class="callsign-title">📡 {@callsign}</div>
        <div class="aprs-symbol-info">
          <span>Symbol Table: <code>{@latest_symbol_table_id}</code></span>
          <span style="margin-left:8px;">Symbol Code: <code>{@latest_symbol_code}</code></span>
        </div>
        <div class="nav-links">
          <a href="/" class="nav-link">← Back to Map</a>
          <a href="/packets" class="nav-link">All Packets</a>
          <a href={"/packets/#{String.downcase(@callsign)}"} class="nav-link">{@callsign} Packets</a>
        </div>
      </div>

      <%= if @replay_active do %>
        <div class="replay-controls">
          <button class="replay-button" phx-click="pause_replay">
            {if @replay_paused, do: "Resume", else: "Pause"}
          </button>

          <div class="speed-control">
            <label>Speed:</label>
            <input
              type="number"
              min="0.1"
              max="10"
              step="0.1"
              value={@replay_speed}
              phx-change="adjust_replay_speed"
              name="speed"
            />x
          </div>
        </div>
      <% end %>

      <button class="locate-button" phx-click="locate_me" title="Find my location">
        🎯
      </button>

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
    updated_visible_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_key, packet} ->
        packet_within_time_threshold?(packet, one_hour_ago)
      end)
      |> Map.new()

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

    socket =
      assign(socket,
        replay_packets: packets,
        replay_index: 0,
        replay_start_time: one_hour_ago,
        replay_end_time: now,
        historical_packets: %{}
      )

    # Start replay immediately if we have packets
    if length(packets) > 0 do
      Process.send_after(self(), :replay_next_packet, 100)
    end

    socket
  end

  defp fetch_historical_packets_for_callsign(callsign, start_time, end_time) do
    Packets.get_packets_for_replay(%{
      callsign: callsign,
      start_time: start_time,
      end_time: end_time,
      limit: 1000
    })
  end

  defp has_position_data?(packet) do
    case packet.data_extended do
      %MicE{} ->
        true

      %{latitude: lat, longitude: lon} when not is_nil(lat) and not is_nil(lon) ->
        true

      _ ->
        lat = Map.get(packet, :lat) || Map.get(packet, "lat")
        lon = Map.get(packet, :lon) || Map.get(packet, "lon")
        not is_nil(lat) and not is_nil(lon)
    end
  end

  defp get_coordinates(%{data_extended: %MicE{} = mic_e}), do: get_coordinates_from_mic_e(mic_e)
  defp get_coordinates(%{data_extended: %{latitude: lat, longitude: lon}}), do: {lat, lon}
  defp get_coordinates(_), do: {nil, nil}

  defp get_coordinates_from_mic_e(mic_e) do
    lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0 + mic_e.lat_fractional / 6000.0
    lat = if mic_e.lat_direction == :south, do: -lat, else: lat
    lng = mic_e.lon_degrees + mic_e.lon_minutes / 60.0 + mic_e.lon_fractional / 6000.0
    lng = if mic_e.lon_direction == :west, do: -lng, else: lng
    if lat >= -90 && lat <= 90 && lng >= -180 && lng <= 180, do: {lat, lng}, else: {nil, nil}
  end

  defp build_packet_data(packet) do
    {lat, lng} = get_coordinates(packet)

    if lat && lng do
      callsign = "#{packet.base_callsign}#{if packet.ssid, do: "-#{packet.ssid}", else: ""}"

      symbol_table_id = get_symbol_table_id(packet.data_extended)
      symbol_code = get_symbol_code(packet.data_extended)
      comment = get_comment(packet.data_extended)

      %{
        id: "#{callsign}_#{:os.system_time(:millisecond)}",
        callsign: callsign,
        lat: lat,
        lng: lng,
        symbol_table_id: symbol_table_id,
        symbol_code: symbol_code,
        comment: comment,
        received_at: packet.received_at || DateTime.utc_now(),
        data_extended: build_data_extended(packet.data_extended)
      }
    end
  end

  defp get_symbol_table_id(data) do
    cond do
      is_map(data) && (Map.get(data, :symbol_table_id) || Map.get(data, "symbol_table_id")) ->
        Map.get(data, :symbol_table_id) || Map.get(data, "symbol_table_id")

      is_map(data) && Map.has_key?(data, :packet) &&
          (Map.get(data.packet, :symbol_table_id) || Map.get(data.packet, "symbol_table_id")) ->
        Map.get(data.packet, :symbol_table_id) || Map.get(data.packet, "symbol_table_id")

      is_map(data) && Map.has_key?(data, "packet") &&
          (Map.get(data["packet"], :symbol_table_id) || Map.get(data["packet"], "symbol_table_id")) ->
        Map.get(data["packet"], :symbol_table_id) || Map.get(data["packet"], "symbol_table_id")

      true ->
        "/"
    end
  end

  defp get_symbol_code(data) do
    cond do
      is_map(data) && (Map.get(data, :symbol_code) || Map.get(data, "symbol_code")) ->
        Map.get(data, :symbol_code) || Map.get(data, "symbol_code")

      is_map(data) && Map.has_key?(data, :packet) &&
          (Map.get(data.packet, :symbol_code) || Map.get(data.packet, "symbol_code")) ->
        Map.get(data.packet, :symbol_code) || Map.get(data.packet, "symbol_code")

      is_map(data) && Map.has_key?(data, "packet") &&
          (Map.get(data["packet"], :symbol_code) || Map.get(data["packet"], "symbol_code")) ->
        Map.get(data["packet"], :symbol_code) || Map.get(data["packet"], "symbol_code")

      true ->
        ">"
    end
  end

  defp get_comment(%{comment: comment}) when is_binary(comment), do: comment
  defp get_comment(_), do: ""

  defp build_data_extended(nil), do: nil

  defp build_data_extended(data_extended) do
    case data_extended do
      %MicE{} = mic_e ->
        %{
          type: "MicE",
          latitude: mic_e.lat_degrees + mic_e.lat_minutes / 60.0 + mic_e.lat_fractional / 6000.0,
          longitude: mic_e.lon_degrees + mic_e.lon_minutes / 60.0 + mic_e.lon_fractional / 6000.0,
          symbol_table_id: Map.get(mic_e, :symbol_table_id, "/"),
          symbol_code: Map.get(mic_e, :symbol_code, ">"),
          comment: Map.get(mic_e, :comment, "")
        }

      %{} = data ->
        # Convert struct to map if needed, filtering out private fields
        data
        |> Map.from_struct()
        |> Map.delete(:__meta__)

      _ ->
        nil
    end
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
    # Load recent packets (last hour) for this specific callsign
    recent_packets = Packets.get_recent_packets(%{callsign: callsign})

    # Find the most recent packet with position data for auto-zoom and symbol info
    latest_packet =
      recent_packets
      |> Enum.filter(&has_position_data?/1)
      |> Enum.sort_by(& &1.received_at, {:desc, DateTime})
      |> List.first()

    last_known_position =
      case latest_packet do
        nil ->
          nil

        packet ->
          {lat, lng} = get_coordinates(packet)
          if lat && lng, do: %{lat: lat, lng: lng}
      end

    latest_symbol_table_id =
      case latest_packet do
        %{data_extended: %{symbol_table_id: id}} when is_binary(id) -> id
        _ -> "/"
      end

    latest_symbol_code =
      case latest_packet do
        %{data_extended: %{symbol_code: code}} when is_binary(code) -> code
        _ -> ">"
      end

    # Convert packets to client-friendly format and send to map
    packet_data_list =
      recent_packets
      |> Stream.filter(&has_position_data?/1)
      |> Stream.map(&build_packet_data/1)
      |> Stream.filter(&(&1 != nil))
      |> Enum.to_list()

    # Build visible_packets map from the loaded packets
    visible_packets =
      recent_packets
      |> Stream.filter(&has_position_data?/1)
      |> Map.new(fn packet ->
        callsign_key = "#{packet.base_callsign}#{if packet.ssid, do: "-#{packet.ssid}", else: ""}"
        {callsign_key, packet}
      end)

    # Send packets to map if any exist
    socket =
      if length(packet_data_list) > 0 do
        push_event(socket, "add_markers", %{markers: packet_data_list})
      else
        socket
      end

    assign(socket,
      last_known_position: last_known_position,
      visible_packets: visible_packets,
      latest_symbol_table_id: latest_symbol_table_id,
      latest_symbol_code: latest_symbol_code
    )
  end

  defp map_empty?(socket) do
    map_size(socket.assigns.visible_packets) == 0
  end

  # Check if we should update zoom based on position change
  defp should_update_zoom?(nil, _new_pos), do: false
  defp should_update_zoom?(_old_pos, nil), do: false

  defp should_update_zoom?(old_pos, new_pos) do
    # Calculate distance between positions (rough approximation)
    lat_diff = abs(old_pos.lat - new_pos.lat)
    lng_diff = abs(old_pos.lng - new_pos.lng)

    # Update zoom if position changed by more than ~5km (approximately 0.05 degrees)
    lat_diff > 0.05 or lng_diff > 0.05
  end
end
