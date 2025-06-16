defmodule AprsWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsWeb, :live_view

  alias Aprs.EncodingUtils
  alias AprsWeb.Endpoint
  alias AprsWeb.Helpers.AprsSymbols
  alias Parser.Types.MicE

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5
  @ip_api_url "https://ip-api.com/json/"
  @finch_name Aprs.Finch
  @default_replay_speed 1000

  @impl true
  def mount(_params, _session, socket) do
    # Calculate one hour ago for packet age filtering
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    socket =
      assign(socket,
        packets: [],
        page_title: "APRS Map",
        # Track visible packets by callsign
        visible_packets: %{},
        # Default bounds for USA
        map_bounds: %{
          north: 49.0,
          south: 24.0,
          east: -66.0,
          west: -125.0
        },
        map_center: @default_center,
        map_zoom: @default_zoom,
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
        pending_geolocation: nil
      )

    if connected?(socket) do
      IO.puts("Socket is connected, attempting to get IP location")
      Endpoint.subscribe("aprs_messages")
      # Get IP-based location on initial load
      IO.puts("Connect info: #{inspect(socket.private[:connect_info])}")
      IO.puts("Peer data: #{inspect(socket.private[:connect_info][:peer_data])}")

      ip =
        case socket.private[:connect_info][:peer_data][:address] do
          {a, b, c, d} -> "#{a}.#{b}.#{c}.#{d}"
          {a, b, c, d, e, f, g, h} -> "#{a}:#{b}:#{c}:#{d}:#{e}:#{f}:#{g}:#{h}"
          _ -> nil
        end

      IO.puts("Extracted IP address: #{inspect(ip)}")

      if ip do
        IO.puts("Starting IP geolocation task for IP: #{ip}")
        # Start as a separate task and await the result
        Task.start(fn ->
          try do
            get_ip_location(ip)
          rescue
            error ->
              IO.puts("Error in IP geolocation task: #{inspect(error)}")
              IO.puts("Stacktrace: #{inspect(__STACKTRACE__)}")
              send(self(), {:ip_location, @default_center})
          end
        end)
      else
        IO.puts("No IP address found, skipping geolocation")
      end

      # Schedule regular cleanup of old packets from the map
      if connected?(socket), do: Process.send_after(self(), :cleanup_old_packets, 60_000)
      # Schedule initialization of replay after a short delay
      if connected?(socket), do: Process.send_after(self(), :initialize_replay, 2000)
    end

    {:ok, socket}
  end

  @impl true
  def handle_event("bounds_changed", %{"bounds" => bounds}, socket) do
    handle_bounds_update(bounds, socket)
  end

  @impl true
  def handle_event("update_bounds", %{"bounds" => bounds}, socket) do
    handle_bounds_update(bounds, socket)
  end

  @impl true
  def handle_event("locate_me", _params, socket) do
    # Send JavaScript command to request browser geolocation
    IO.puts("locate_me event received, requesting geolocation")
    {:noreply, push_event(socket, "request_geolocation", %{})}
  end

  @impl true
  def handle_event("set_location", %{"lat" => lat, "lng" => lng}, socket) do
    # Update map center and zoom when location is received
    IO.puts("set_location event received with lat=#{lat}, lng=#{lng}")

    # Ensure coordinates are floats
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

    IO.puts("Sending zoom_to_location event from set_location with lat=#{lat_float}, lng=#{lng_float}")

    socket =
      socket
      |> assign(map_center: %{lat: lat_float, lng: lng_float}, map_zoom: 12)
      |> push_event("zoom_to_location", %{lat: lat_float, lng: lng_float, zoom: 12})

    {:noreply, socket}
  end

  @impl true
  def handle_event("toggle_replay", _params, socket) do
    if socket.assigns.replay_active do
      # Stop replay
      if socket.assigns.replay_timer_ref, do: Process.cancel_timer(socket.assigns.replay_timer_ref)

      # Clear historical packets from the map
      socket =
        socket
        |> push_event("clear_historical_packets", %{})
        |> assign(
          replay_active: false,
          replay_timer_ref: nil,
          replay_paused: false,
          replay_packets: [],
          replay_index: 0,
          historical_packets: %{},
          replay_started: false
        )

      # Restart replay after a short delay
      Process.send_after(self(), :initialize_replay, 1000)

      {:noreply, socket}
    else
      # If not active, the user manually requested a replay restart
      {:noreply, start_historical_replay(socket)}
    end
  end

  @impl true
  def handle_event("pause_replay", _params, socket) do
    if socket.assigns.replay_active do
      if socket.assigns.replay_paused do
        # Resume replay
        timer_ref = Process.send_after(self(), :replay_next_packet, 1000)
        {:noreply, assign(socket, replay_paused: false, replay_timer_ref: timer_ref)}
      else
        # Pause replay
        if socket.assigns.replay_timer_ref, do: Process.cancel_timer(socket.assigns.replay_timer_ref)
        {:noreply, assign(socket, replay_paused: true, replay_timer_ref: nil)}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("adjust_replay_speed", %{"speed" => speed}, socket) do
    speed_float = to_float(speed)
    {:noreply, assign(socket, replay_speed: speed_float)}
  end

  @impl true
  def handle_event("map_ready", _params, socket) do
    IO.puts("Map ready event received")
    socket = assign(socket, map_ready: true)

    # If we have pending geolocation, zoom to it now
    socket =
      if socket.assigns.pending_geolocation do
        %{lat: lat, lng: lng} = socket.assigns.pending_geolocation
        IO.puts("Map ready - zooming to pending location: lat=#{lat}, lng=#{lng}")
        push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: 12})
      else
        IO.puts("Map ready - no pending geolocation")
        socket
      end

    {:noreply, socket}
  end

  defp handle_bounds_update(bounds, socket) do
    # Update the map bounds from the client
    map_bounds = %{
      north: bounds["north"],
      south: bounds["south"],
      east: bounds["east"],
      west: bounds["west"]
    }

    # Don't clear markers - let the client preserve those still in view
    # Recalculate packet count based on new bounds
    visible_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_callsign, packet} ->
        within_bounds?(packet, map_bounds)
      end)
      |> Map.new()

    # If replay is not active, update the replay packets based on the new bounds
    socket =
      if socket.assigns.replay_active do
        socket
      else
        # Clear any existing replay data when bounds change
        socket =
          assign(socket,
            replay_packets: [],
            replay_index: 0,
            historical_packets: %{},
            map_ready: true
          )

        # Don't automatically start replay on every bounds change
        # We'll handle this in initialize_replay to avoid too many restarts
        socket
      end

    {:noreply, assign(socket, map_bounds: map_bounds, visible_packets: visible_packets)}
  end

  @impl true
  def handle_info(msg, socket) do
    case msg do
      {:delayed_zoom, %{lat: lat, lng: lng}} ->
        IO.puts("Processing delayed zoom to lat=#{lat}, lng=#{lng}")
        socket = push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: 12})
        {:noreply, socket}

      {:ip_location, %{lat: lat, lng: lng}} ->
        # Log IP location received
        IO.puts("IP location received: lat=#{lat}, lng=#{lng}")

        # Ensure we're using numeric values for coordinates
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

        # Update map center first
        socket = assign(socket, map_center: %{lat: lat_float, lng: lng_float}, map_zoom: 12)

        # If map is ready, zoom to location immediately, otherwise store for later
        socket =
          if socket.assigns.map_ready do
            IO.puts("Map is ready, zooming to location immediately to lat=#{lat_float}, lng=#{lng_float}")
            push_event(socket, "zoom_to_location", %{lat: lat_float, lng: lng_float, zoom: 12})
          else
            IO.puts("Map not ready yet, storing location lat=#{lat_float}, lng=#{lng_float} for later")
            assign(socket, pending_geolocation: %{lat: lat_float, lng: lng_float})
          end

        {:noreply, socket}

      :initialize_replay ->
        # Only start replay if it hasn't been started yet
        if not socket.assigns.replay_started and socket.assigns.map_ready do
          socket = start_historical_replay(socket)
          {:noreply, assign(socket, replay_started: true)}
        else
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

        # Only process packets with position data that are within current map bounds
        # AND are not older than 1 hour
        if has_position_data?(sanitized_packet) &&
             within_bounds?(sanitized_packet, socket.assigns.map_bounds) &&
             packet_within_time_threshold?(sanitized_packet, socket.assigns.packet_age_threshold) do
          # Convert to a simple map structure for JSON encoding
          packet_data = build_packet_data(sanitized_packet)

          # Only push if we have valid packet data
          if packet_data do
            # Generate a unique key for this packet
            callsign_key =
              "#{sanitized_packet.base_callsign}#{if sanitized_packet.ssid, do: "-#{sanitized_packet.ssid}", else: ""}"

            # Update visible packets tracking
            visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, sanitized_packet)

            # Push the packet to the client-side JavaScript
            socket =
              socket
              |> push_event("new_packet", packet_data)
              |> assign(visible_packets: visible_packets)

            {:noreply, socket}
          else
            # Invalid packet data, skip it
            {:noreply, socket}
          end
        else
          # Ignore packets without position data, outside bounds, or too old
          {:noreply, socket}
        end
    end
  end

  # Handle replaying the next historical packet
  defp handle_replay_next_packet(socket) do
    %{
      replay_packets: packets,
      replay_index: index,
      replay_speed: speed,
      historical_packets: historical_packets
    } = socket.assigns

    if index < length(packets) do
      # Get the next packet and advance the index
      packet = Enum.at(packets, index)
      next_index = index + 1

      # Convert to a simple map structure for JSON encoding
      packet_data = build_packet_data(packet)

      # Only process packets with valid position data
      socket =
        if packet_data do
          # Add is_historical flag and timestamp
          packet_data =
            Map.merge(packet_data, %{
              "is_historical" => true,
              "timestamp" => if(Map.has_key?(packet, :received_at), do: DateTime.to_iso8601(packet.received_at))
            })

          # Generate a unique key for this packet
          packet_id = "hist_#{if Map.has_key?(packet, :id), do: packet.id, else: System.unique_integer([:positive])}"

          # Update historical packets tracking
          new_historical_packets = Map.put(historical_packets, packet_id, packet)

          # Send packet data to client
          socket
          |> push_event("historical_packet", packet_data)
          |> assign(
            replay_index: next_index,
            historical_packets: new_historical_packets
          )
        else
          # Skip packets without position data, just advance the index
          assign(socket, replay_index: next_index)
        end

      # Calculate delay for next packet (faster based on replay speed)
      delay_ms = trunc(1000 / speed)

      # Schedule the next packet
      timer_ref = Process.send_after(self(), :replay_next_packet, delay_ms)
      {:noreply, assign(socket, replay_timer_ref: timer_ref)}
    else
      # All packets replayed, start over with a short delay
      Process.send_after(self(), :initialize_replay, 10_000)

      # Set active to false but don't clear packets - will auto-restart
      socket =
        assign(socket,
          replay_active: false,
          replay_timer_ref: nil,
          replay_started: false
        )

      {:noreply, socket}
    end
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
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        width: 100%;
        height: 100vh;
        z-index: 1;
      }

      .locate-button {
        position: absolute;
        left: 10px;
        top: 80px;
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
        color: #1e40af;
        margin-bottom: 4px;
      }

      .aprs-symbol-info {
        color: #6b7280;
        font-style: italic;
        margin-bottom: 4px;
      }

      .aprs-comment {
        color: #374151;
        margin-bottom: 4px;
        word-wrap: break-word;
      }

      .aprs-coords {
        color: #6b7280;
        font-size: 11px;
        font-family: monospace;
      }

      /* High-DPI support for APRS symbols */
      @media (-webkit-min-device-pixel-ratio: 2), (min-resolution: 192dpi) {
        .aprs-marker div[style*="aprs-symbols-24-0.png"] {
          background-image: url('/aprs-symbols/aprs-symbols-24-0@2x.png') !important;
          background-size: 384px 144px !important;
        }

        .aprs-marker div[style*="aprs-symbols-24-1.png"] {
          background-image: url('/aprs-symbols/aprs-symbols-24-1@2x.png') !important;
          background-size: 384px 144px !important;
        }

        .aprs-marker div[style*="aprs-symbols-24-2.png"] {
          background-image: url('/aprs-symbols/aprs-symbols-24-2@2x.png') !important;
          background-size: 384px 144px !important;
        }
      }

      /* Leaflet popup improvements for APRS data */
      .leaflet-popup-content-wrapper {
        border-radius: 8px;
      }

      .leaflet-popup-content {
        margin: 8px 12px;
      }
    </style>

    <div
      id="aprs-map"
      phx-hook="APRSMap"
      phx-update="ignore"
      data-center={Jason.encode!(@map_center)}
      data-zoom={@map_zoom}
      data-lat={@map_center.lat}
      data-lng={@map_center.lng}
    >
    </div>

    <button class="locate-button" phx-click="locate_me" title="Find my location">
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="20"
        height="20"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        stroke-width="2"
        stroke-linecap="round"
        stroke-linejoin="round"
      >
        <circle cx="12" cy="12" r="10" />
        <line x1="12" y1="8" x2="12" y2="16" />
        <line x1="8" y1="12" x2="16" y2="12" />
      </svg>
    </button>
    """
  end

  # Handle cleanup of old packets
  defp handle_cleanup_old_packets(socket) do
    # Update the packet age threshold to current time minus one hour
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    # Filter out packets older than one hour
    visible_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_key, packet} ->
        packet_within_time_threshold?(packet, one_hour_ago)
      end)
      |> Map.new()

    # Push event to remove old markers from the map
    socket =
      socket
      |> push_event("refresh_markers", %{})
      |> assign(
        visible_packets: visible_packets,
        packet_age_threshold: one_hour_ago
      )

    # Schedule the next cleanup in 1 minute
    if connected?(socket), do: Process.send_after(self(), :cleanup_old_packets, 60_000)

    {:noreply, socket}
  end

  # Check if a packet is within the time threshold (not too old)
  defp packet_within_time_threshold?(packet, threshold) do
    case packet do
      %{received_at: received_at} when not is_nil(received_at) ->
        DateTime.compare(received_at, threshold) in [:gt, :eq]

      _ ->
        # If no timestamp, treat as current
        true
    end
  end

  # Helper functions

  # Fetch historical packets from the database
  # Helper function to start historical replay
  defp start_historical_replay(socket) do
    # Get time range for historical data
    now = DateTime.utc_now()
    one_hour_ago = DateTime.add(now, -60 * 60, :second)

    # Convert map bounds to the format expected by the database query
    bounds = [
      socket.assigns.map_bounds.west,
      socket.assigns.map_bounds.south,
      socket.assigns.map_bounds.east,
      socket.assigns.map_bounds.north
    ]

    # Fetch historical packets with position data within the current map bounds
    historical_packets = fetch_historical_packets(bounds, one_hour_ago, now)

    if Enum.empty?(historical_packets) do
      # No historical packets found - silently continue without replay
      socket
    else
      # Clear any previous historical packets from the map
      socket = push_event(socket, "clear_historical_packets", %{})

      # Start replay
      timer_ref = Process.send_after(self(), :replay_next_packet, 1000)

      assign(socket,
        replay_active: true,
        replay_packets: historical_packets,
        replay_index: 0,
        replay_timer_ref: timer_ref,
        replay_start_time: one_hour_ago,
        replay_end_time: now,
        historical_packets: %{},
        replay_started: true
      )
    end
  end

  # Fetch historical packets from the database
  defp fetch_historical_packets(bounds, start_time, end_time) do
    # Force start_time to be at most 1 hour ago
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    effective_start_time =
      if DateTime.before?(start_time, one_hour_ago),
        do: one_hour_ago,
        else: start_time

    # Use the Packets context to retrieve historical packets
    packets_params = %{
      bounds: bounds,
      start_time: effective_start_time,
      end_time: end_time,
      with_position: true,
      # Reasonable limit to prevent overwhelming the client
      limit: 1000
    }

    # Call the database through the Packets context
    packets = Aprs.Packets.get_packets_for_replay(packets_params)

    # Sort packets by received_at timestamp to ensure chronological replay
    Enum.sort_by(packets, fn packet -> packet.received_at end)
  end

  defp has_position_data?(packet) do
    case packet.data_extended do
      %MicE{} = mic_e ->
        # MicE packets have lat/lon in separate components
        is_number(mic_e.lat_degrees) && is_number(mic_e.lat_minutes) &&
          is_number(mic_e.lon_degrees) && is_number(mic_e.lon_minutes)

      %{latitude: lat, longitude: lon} ->
        # Regular position packets have decimal lat/lon
        is_number(lat) && is_number(lon)

      _ ->
        false
    end
  end

  defp within_bounds?(packet, bounds) do
    {lat, lng} = get_coordinates(packet)

    lat && lng &&
      lat >= bounds.south && lat <= bounds.north &&
      lng >= bounds.west && lng <= bounds.east
  end

  defp get_coordinates(packet) do
    case packet.data_extended do
      %MicE{} = mic_e ->
        # Convert MicE components to decimal degrees
        lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0 + mic_e.lat_fractional / 6000.0
        lat = if mic_e.lat_direction == :south, do: -lat, else: lat

        lng = mic_e.lon_degrees + mic_e.lon_minutes / 60.0 + mic_e.lon_fractional / 6000.0
        lng = if mic_e.lon_direction == :west, do: -lng, else: lng

        # Validate coordinates are within valid ranges
        if lat >= -90 && lat <= 90 && lng >= -180 && lng <= 180 do
          {lat, lng}
        else
          IO.puts("Invalid MicE coordinates: lat=#{lat}, lng=#{lng}")
          {nil, nil}
        end

      %{latitude: lat, longitude: lon} ->
        {lat, lon}

      _ ->
        {nil, nil}
    end
  end

  defp build_packet_data(packet) do
    data_extended = build_data_extended(packet.data_extended)
    {lat, lng} = get_coordinates(packet)

    # Only include packets with valid position data
    if lat && lng do
      # Get symbol information from data_extended
      symbol_table_id =
        get_in(data_extended, ["symbol_table_id"]) ||
          packet.symbol_table_id || "/"

      symbol_code =
        get_in(data_extended, ["symbol_code"]) ||
          packet.symbol_code || ">"

      # Validate symbol
      {final_table_id, final_symbol_code} =
        if AprsSymbols.valid_symbol?(symbol_table_id, symbol_code) do
          {symbol_table_id, symbol_code}
        else
          AprsSymbols.default_symbol()
        end

      # Generate callsign for display
      callsign =
        if packet.ssid && packet.ssid != "" do
          "#{packet.base_callsign}-#{packet.ssid}"
        else
          packet.base_callsign || ""
        end

      %{
        "id" => callsign,
        "callsign" => callsign,
        "base_callsign" => packet.base_callsign || "",
        "ssid" => packet.ssid || "",
        "lat" => lat,
        "lng" => lng,
        "symbol_table_id" => final_table_id,
        "symbol_code" => final_symbol_code,
        "symbol_description" => AprsSymbols.symbol_description(final_table_id, final_symbol_code),
        "data_type" => to_string(packet.data_type || "unknown"),
        "path" => packet.path || "",
        "comment" => get_in(data_extended, ["comment"]) || "",
        "data_extended" => data_extended || %{}
      }

      # Return nil for packets without position data
    end
  end

  # Get IP location from external service

  # Get location from IP using ip-api.com
  defp get_ip_location(nil), do: nil

  defp get_ip_location(ip) do
    url = "#{@ip_api_url}#{ip}"
    IO.puts("Fetching location for IP: #{ip} from URL: #{url}")

    # Add headers to make the request more likely to succeed
    request =
      Finch.build(:get, url, [
        {"User-Agent", "APRS.me/1.0"},
        {"Accept", "application/json"}
      ])

    # Add a small delay to ensure the client is connected
    Process.sleep(2000)

    IO.puts("Making HTTP request to IP API...")

    case Finch.request(request, @finch_name, receive_timeout: 10_000) do
      {:ok, %{status: 200, body: body}} ->
        IO.puts("IP API response received successfully")
        IO.puts("Response body: #{String.slice(body, 0, 200)}...")

        case Jason.decode(body) do
          {:ok, %{"status" => "success", "lat" => lat, "lon" => lng}} when is_number(lat) and is_number(lng) ->
            IO.puts("Valid coordinates found: lat=#{lat}, lng=#{lng}")

            if lat >= -90 and lat <= 90 and lng >= -180 and lng <= 180 do
              IO.puts("Sending IP location message with coordinates")
              # Force numeric values
              lat_float = lat / 1.0
              lng_float = lng / 1.0
              send(self(), {:ip_location, %{lat: lat_float, lng: lng_float}})
            else
              IO.puts("Coordinates out of range: lat=#{lat}, lng=#{lng}, using default center")
              send(self(), {:ip_location, @default_center})
            end

          {:ok, %{"status" => status}} ->
            IO.puts("IP API returned non-success status: #{status}")
            send(self(), {:ip_location, @default_center})

          {:ok, data} ->
            IO.puts("IP API returned unexpected format: #{inspect(data)}")
            send(self(), {:ip_location, @default_center})

          {:error, decode_error} ->
            IO.puts("Failed to decode JSON response: #{inspect(decode_error)}")
            IO.puts("Raw response: #{body}")
            send(self(), {:ip_location, @default_center})
        end

      {:ok, response} ->
        IO.puts("IP API request failed with status: #{response.status}")
        IO.puts("Response headers: #{inspect(response.headers)}")
        IO.puts("Response body: #{String.slice(response.body || "", 0, 200)}...")
        send(self(), {:ip_location, @default_center})

      {:error, %{reason: :timeout}} ->
        IO.puts("IP API request timed out after 10 seconds")
        send(self(), {:ip_location, @default_center})

      {:error, error} ->
        IO.puts("IP API request error: #{inspect(error)}")
        send(self(), {:ip_location, @default_center})
    end
  end

  defp build_data_extended(nil), do: nil

  defp build_data_extended(data_extended) do
    case data_extended do
      %MicE{} = mic_e ->
        # Convert MicE components to decimal degrees
        lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0 + mic_e.lat_fractional / 6000.0
        lat = if mic_e.lat_direction == :south, do: -lat, else: lat

        lng = mic_e.lon_degrees + mic_e.lon_minutes / 60.0 + mic_e.lon_fractional / 6000.0
        lng = if mic_e.lon_direction == :west, do: -lng, else: lng

        # Validate coordinates are within valid ranges
        if lat >= -90 && lat <= 90 && lng >= -180 && lng <= 180 do
          %{
            "latitude" => lat,
            "longitude" => lng,
            "comment" => mic_e.message || "",
            "symbol_table_id" => "/",
            "symbol_code" => ">",
            "aprs_messaging" => false
          }
        end

      _ ->
        %{
          "latitude" => data_extended[:latitude],
          "longitude" => data_extended[:longitude],
          "comment" => data_extended[:comment] || "",
          "symbol_table_id" => data_extended[:symbol_table_id] || "/",
          "symbol_code" => data_extended[:symbol_code] || ">",
          "aprs_messaging" => data_extended[:aprs_messaging] || false
        }
    end
  end

  # Helper function to convert string or float to float
  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_binary(value), do: String.to_float(value)
  defp to_float(value) when is_integer(value), do: value * 1.0
end
