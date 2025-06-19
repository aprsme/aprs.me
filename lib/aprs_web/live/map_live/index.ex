defmodule AprsWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsWeb, :live_view

  alias AprsWeb.Endpoint
  alias AprsWeb.Helpers.AprsSymbols
  alias Phoenix.LiveView.Socket

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5
  @ip_api_url "https://ip-api.com/json/"
  @finch_name Aprs.Finch
  @default_replay_speed 1000
  @debounce_interval 200

  @impl true
  def mount(_params, _session, socket) do
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    socket = assign_defaults(socket, one_hour_ago)
    socket = assign(socket, packet_buffer: [], buffer_timer: nil)

    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      Phoenix.PubSub.subscribe(Aprs.PubSub, "postgres:aprs_packets")
      maybe_start_geolocation(socket)
      schedule_timers()
    end

    {:ok, socket}
  end

  @spec assign_defaults(Socket.t(), DateTime.t()) :: Socket.t()
  defp assign_defaults(socket, one_hour_ago) do
    assign(socket,
      packets: [],
      page_title: "APRS Map",
      visible_packets: %{},
      map_bounds: %{
        north: 49.0,
        south: 24.0,
        east: -66.0,
        west: -125.0
      },
      map_center: @default_center,
      map_zoom: @default_zoom,
      replay_active: false,
      replay_speed: @default_replay_speed,
      replay_paused: false,
      replay_packets: [],
      replay_index: 0,
      replay_timer_ref: nil,
      replay_start_time: nil,
      replay_end_time: nil,
      historical_packets: %{},
      packet_age_threshold: one_hour_ago,
      map_ready: false,
      replay_started: false,
      pending_geolocation: nil,
      bounds_update_timer: nil,
      pending_bounds: nil
    )
  end

  @spec maybe_start_geolocation(Socket.t()) :: Socket.t()
  defp maybe_start_geolocation(socket) do
    if Application.get_env(:aprs, :disable_aprs_connection, false) != true do
      ip =
        case socket.private[:connect_info][:peer_data][:address] do
          {a, b, c, d} -> "#{a}.#{b}.#{c}.#{d}"
          {a, b, c, d, e, f, g, h} -> "#{a}:#{b}:#{c}:#{d}:#{e}:#{f}:#{g}:#{h}"
          _ -> nil
        end

      if ip && !String.starts_with?(ip, "127.") && !String.starts_with?(ip, "::1") do
        Task.start(fn ->
          try do
            get_ip_location(ip)
          rescue
            _error ->
              send(self(), {:ip_location, @default_center})
          end
        end)
      end
    end

    socket
  end

  defp schedule_timers do
    Process.send_after(self(), :cleanup_old_packets, 60_000)
    Process.send_after(self(), :initialize_replay, 2000)
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
    {:noreply, push_event(socket, "request_geolocation", %{})}
  end

  @impl true
  def handle_event("set_location", %{"lat" => lat, "lng" => lng}, socket) do
    # Update map center and zoom when location is received
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
      if socket.assigns.replay_timer_ref,
        do: Process.cancel_timer(socket.assigns.replay_timer_ref)

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
        if socket.assigns.replay_timer_ref,
          do: Process.cancel_timer(socket.assigns.replay_timer_ref)

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
  def handle_event("clear_and_reload_markers", _params, socket) do
    # Clear all markers on the client first
    socket = push_event(socket, "clear_markers", %{})

    # Filter visible packets to only include those within current bounds
    filtered_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_callsign, packet} ->
        within_bounds?(packet, socket.assigns.map_bounds) &&
          packet_within_time_threshold?(packet, socket.assigns.packet_age_threshold)
      end)
      |> Map.new()

    # Convert filtered packets to packet data for the map
    visible_packets_list =
      filtered_packets
      |> Enum.map(fn {_callsign, packet} -> build_packet_data(packet) end)
      # Remove any nil packet data
      |> Enum.filter(& &1)

    # Update the state to only include the filtered packets
    socket = assign(socket, visible_packets: filtered_packets)

    # Add the visible markers back to the map
    socket =
      if Enum.any?(visible_packets_list) do
        push_event(socket, "add_markers", %{markers: visible_packets_list})
      else
        socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("map_ready", _params, socket) do
    socket = assign(socket, map_ready: true)

    # If we have pending geolocation, zoom to it now
    socket =
      if socket.assigns.pending_geolocation do
        %{lat: lat, lng: lng} = socket.assigns.pending_geolocation
        push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: 12})
      else
        socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("marker_clicked", %{"id" => _id, "callsign" => _callsign, "lat" => _lat, "lng" => _lng}, socket) do
    {:noreply, socket}
  end

  @spec handle_bounds_update(map(), Socket.t()) :: {:noreply, Socket.t()}
  defp handle_bounds_update(bounds, socket) do
    # Update the map bounds from the client
    map_bounds = %{
      north: bounds["north"],
      south: bounds["south"],
      east: bounds["east"],
      west: bounds["west"]
    }

    # Validate bounds to prevent invalid coordinates
    if map_bounds.north > 90 or map_bounds.south < -90 or
         map_bounds.north <= map_bounds.south do
      # Invalid bounds, skip update
      {:noreply, socket}
    else
      # Cancel any pending bounds update timer
      if socket.assigns[:bounds_update_timer] do
        Process.cancel_timer(socket.assigns.bounds_update_timer)
      end

      # Set a debounced update timer to prevent excessive processing
      timer_ref = Process.send_after(self(), {:process_bounds_update, map_bounds}, 250)

      socket = assign(socket, bounds_update_timer: timer_ref, pending_bounds: map_bounds)
      {:noreply, socket}
    end
  end

  @spec process_bounds_update(map(), Socket.t()) :: Socket.t()
  defp process_bounds_update(map_bounds, socket) do
    # Remove out-of-bounds packets and markers immediately
    new_visible_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_k, packet} -> within_bounds?(packet, map_bounds) end)
      |> Map.new()

    packets_to_remove =
      socket.assigns.visible_packets
      |> Enum.reject(fn {_k, packet} -> within_bounds?(packet, map_bounds) end)
      |> Enum.map(fn {k, _} -> k end)

    socket =
      Enum.reduce(packets_to_remove, socket, fn k, acc ->
        push_event(acc, "remove_marker", %{id: k})
      end)

    assign(socket, map_bounds: map_bounds, visible_packets: new_visible_packets)
  end

  @impl true
  def handle_info(msg, socket) do
    case msg do
      {:process_bounds_update, map_bounds} ->
        # Process the debounced bounds update
        socket = process_bounds_update(map_bounds, socket)
        {:noreply, socket}

      {:delayed_zoom, %{lat: lat, lng: lng}} ->
        socket = push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: 12})
        {:noreply, socket}

      {:ip_location, %{lat: lat, lng: lng}} ->
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
            push_event(socket, "zoom_to_location", %{lat: lat_float, lng: lng_float, zoom: 12})
          else
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

      {:postgres_packet, packet} ->
        {lat, lon, _data_extended} = get_coordinates(packet)

        if is_nil(lat) or is_nil(lon) do
          {:noreply, socket}
        else
          if within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds) do
            # Add to buffer for debounced batch update
            buffer = [packet | socket.assigns.packet_buffer]
            socket = assign(socket, packet_buffer: buffer)
            # If no timer, start one
            socket =
              if socket.assigns.buffer_timer == nil do
                timer = Process.send_after(self(), :flush_packet_buffer, @debounce_interval)
                assign(socket, buffer_timer: timer)
              else
                socket
              end

            {:noreply, socket}
          else
            {:noreply, socket}
          end
        end

      :flush_packet_buffer ->
        packets = Enum.reverse(socket.assigns.packet_buffer)
        visible_packets = socket.assigns.visible_packets

        {new_visible_packets, events} =
          Enum.reduce(packets, {visible_packets, []}, fn packet, {vis, evs} ->
            packet_data = build_packet_data(packet)

            if packet_data do
              callsign_key =
                if Map.has_key?(packet, "id"),
                  do: to_string(packet["id"]),
                  else: System.unique_integer([:positive])

              {Map.put(vis, callsign_key, packet), [{:new_packet, packet_data} | evs]}
            else
              {vis, evs}
            end
          end)

        # Push all new packets in one event (or as a batch)
        socket =
          Enum.reduce(events, socket, fn {:new_packet, data}, acc ->
            push_event(acc, "new_packet", data)
          end)

        socket =
          assign(socket,
            visible_packets: new_visible_packets,
            packet_buffer: [],
            buffer_timer: nil
          )

        {:noreply, socket}

      %Phoenix.Socket.Broadcast{topic: "aprs_messages", event: "packet", payload: packet} ->
        handle_info({:postgres_packet, packet}, socket)
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
              "timestamp" =>
                if(Map.has_key?(packet, :received_at),
                  do: DateTime.to_iso8601(packet.received_at)
                )
            })

          # Generate a unique key for this packet
          packet_id =
            "hist_#{if Map.has_key?(packet, :id), do: packet.id, else: System.unique_integer([:positive])}"

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

    # Get packets that need to be removed (old or outside bounds)
    packets_to_remove =
      socket.assigns.visible_packets
      |> Enum.reject(fn {_key, packet} ->
        packet_within_time_threshold?(packet, one_hour_ago) &&
          within_bounds?(packet, socket.assigns.map_bounds)
      end)
      |> Enum.map(fn {callsign, _packet} -> callsign end)

    # Filter out packets older than one hour and outside bounds
    visible_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_key, packet} ->
        packet_within_time_threshold?(packet, one_hour_ago) &&
          within_bounds?(packet, socket.assigns.map_bounds)
      end)
      |> Map.new()

    # Only update if there are actual changes to prevent unnecessary events
    socket =
      if map_size(visible_packets) == map_size(socket.assigns.visible_packets) do
        # Just update the threshold without map changes
        assign(socket, packet_age_threshold: one_hour_ago)
        # Remove old and out-of-bounds markers from the map

        # Explicitly remove markers for packets that are no longer valid
      else
        socket =
          socket
          |> push_event("refresh_markers", %{})
          |> assign(
            visible_packets: visible_packets,
            packet_age_threshold: one_hour_ago
          )

        if Enum.any?(packets_to_remove) do
          Enum.reduce(packets_to_remove, socket, fn callsign, acc_socket ->
            push_event(acc_socket, "remove_marker", %{id: callsign})
          end)
        else
          socket
        end
      end

    # Schedule the next cleanup in 1 minute
    if connected?(socket), do: Process.send_after(self(), :cleanup_old_packets, 60_000)

    {:noreply, socket}
  end

  # Check if a packet is within the time threshold (not too old)
  @spec packet_within_time_threshold?(map(), DateTime.t()) :: boolean()
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
  @spec start_historical_replay(Socket.t()) :: Socket.t()
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
  @spec fetch_historical_packets(list(), DateTime.t(), DateTime.t()) :: [struct()]
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

  @spec within_bounds?(map() | struct(), map()) :: boolean()
  defp within_bounds?(packet, bounds) do
    {lat, lon, _data_extended} = get_coordinates(packet)

    # Basic validation
    if is_nil(lat) or is_nil(lon) do
      false
    else
      # Check latitude bounds (straightforward)
      lat_in_bounds = lat >= bounds.south && lat <= bounds.north

      # Check longitude bounds (handle potential wrapping)
      lng_in_bounds =
        if bounds.west <= bounds.east do
          # Normal case: bounds don't cross antimeridian
          lon >= bounds.west && lon <= bounds.east
        else
          # Bounds cross antimeridian (e.g., west=170, east=-170)
          lon >= bounds.west || lon <= bounds.east
        end

      lat_in_bounds && lng_in_bounds
    end
  end

  @spec get_coordinates(map() | struct()) :: {number() | nil, number() | nil}
  defp get_coordinates(packet) do
    # Safely get data_extended for both atom and string keys
    data_extended = Map.get(packet, :data_extended) || Map.get(packet, "data_extended")
    lat = Map.get(packet, :lat) || Map.get(packet, "lat")
    lon = Map.get(packet, :lon) || Map.get(packet, "lon")
    {lat, lon, data_extended}
  end

  @spec build_packet_data(map() | struct()) :: map() | nil
  defp build_packet_data(packet) do
    {lat, lon, data_extended} = get_coordinates(packet)
    # Only include packets with valid position data
    if lat && lon do
      build_packet_map(packet, lat, lon, data_extended)
    end
  end

  @spec build_packet_map(map() | struct(), number(), number(), map() | nil) :: map()
  defp build_packet_map(packet, lat, lon, data_extended) do
    {final_table_id, final_symbol_code} = get_validated_symbol(packet, data_extended)
    callsign = generate_callsign(packet)

    %{
      "id" => callsign,
      "callsign" => callsign,
      "base_callsign" => packet.base_callsign || "",
      "ssid" => packet.ssid || "",
      "lat" => lat,
      "lng" => lon,
      "symbol_table_id" => final_table_id,
      "symbol_code" => final_symbol_code,
      "symbol_description" => AprsSymbols.symbol_description(final_table_id, final_symbol_code),
      "data_type" => to_string(packet.data_type || "unknown"),
      "path" => packet.path || "",
      "comment" => get_in(data_extended, ["comment"]) || "",
      "data_extended" => data_extended || %{}
    }
  end

  @spec get_validated_symbol(map() | struct(), map() | nil) :: {String.t(), String.t()}
  defp get_validated_symbol(packet, data_extended) do
    symbol_table_id = get_symbol_table_id(packet, data_extended)
    symbol_code = get_symbol_code(packet, data_extended)

    if AprsSymbols.valid_symbol?(symbol_table_id, symbol_code) do
      {symbol_table_id, symbol_code}
    else
      AprsSymbols.default_symbol()
    end
  end

  @spec get_symbol_table_id(map() | struct(), map() | nil) :: String.t()
  defp get_symbol_table_id(packet, data_extended) do
    get_in(data_extended, ["symbol_table_id"]) || packet.symbol_table_id || "/"
  end

  @spec get_symbol_code(map() | struct(), map() | nil) :: String.t()
  defp get_symbol_code(packet, data_extended) do
    get_in(data_extended, ["symbol_code"]) || packet.symbol_code || ">"
  end

  @spec generate_callsign(map() | struct()) :: String.t()
  defp generate_callsign(packet) do
    if packet.ssid && packet.ssid != "" do
      "#{packet.base_callsign}-#{packet.ssid}"
    else
      packet.base_callsign || ""
    end
  end

  # Get IP location from external service

  # Get location from IP using ip-api.com
  @spec get_ip_location(String.t() | nil) :: {float(), float()} | nil
  defp get_ip_location(nil), do: nil

  @spec get_ip_location(String.t()) :: {float(), float()} | nil
  defp get_ip_location(ip) do
    url = "#{@ip_api_url}#{ip}"

    # Add headers to make the request more likely to succeed
    request =
      Finch.build(:get, url, [
        {"User-Agent", "APRS.me/1.0"},
        {"Accept", "application/json"}
      ])

    # Add a small delay to ensure the client is connected
    Process.sleep(2000)

    case Finch.request(request, @finch_name, receive_timeout: 10_000) do
      {:ok, %{status: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, %{"status" => "success", "lat" => lat, "lon" => lng}}
          when is_number(lat) and is_number(lng) ->
            if lat >= -90 and lat <= 90 and lng >= -180 and lng <= 180 do
              # Force numeric values
              lat_float = lat / 1.0
              lng_float = lng / 1.0
              send(self(), {:ip_location, %{lat: lat_float, lng: lng_float}})
            else
              send(self(), {:ip_location, @default_center})
            end

          {:ok, %{"status" => _status}} ->
            send(self(), {:ip_location, @default_center})

          {:ok, _data} ->
            send(self(), {:ip_location, @default_center})

          {:error, _decode_error} ->
            send(self(), {:ip_location, @default_center})
        end

      {:ok, _response} ->
        send(self(), {:ip_location, @default_center})

      {:error, %{reason: :timeout}} ->
        send(self(), {:ip_location, @default_center})

      {:error, _error} ->
        send(self(), {:ip_location, @default_center})
    end
  end

  # Helper function to convert string or float to float
  @spec to_float(number() | String.t()) :: float()
  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_integer(value), do: value * 1.0

  @spec to_float(String.t()) :: float()
  defp to_float(value) when is_binary(value) do
    case Float.parse(value) do
      {float_val, _} -> float_val
      :error -> 0.0
    end
  end

  @impl true
  def terminate(_reason, socket) do
    if socket.assigns.buffer_timer, do: Process.cancel_timer(socket.assigns.buffer_timer)
    # Clean up any pending bounds update timer
    if socket.assigns[:bounds_update_timer] do
      Process.cancel_timer(socket.assigns.bounds_update_timer)
    end

    # Clean up replay timer
    if socket.assigns[:replay_timer_ref] do
      Process.cancel_timer(socket.assigns.replay_timer_ref)
    end

    :ok
  end
end
