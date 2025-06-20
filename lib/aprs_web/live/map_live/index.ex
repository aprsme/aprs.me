defmodule AprsWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsWeb, :live_view

  alias AprsWeb.Endpoint
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
    socket = assign(socket, all_packets: %{})

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
    if geolocation_enabled?() do
      ip = extract_ip(socket)

      if valid_ip_for_geolocation?(ip) do
        start_geolocation_task(ip)
      end
    end

    socket
  end

  defp geolocation_enabled? do
    Application.get_env(:aprs, :disable_aprs_connection, false) != true
  end

  defp extract_ip(socket) do
    case socket.private[:connect_info][:peer_data][:address] do
      {a, b, c, d} -> "#{a}.#{b}.#{c}.#{d}"
      {a, b, c, d, e, f, g, h} -> "#{a}:#{b}:#{c}:#{d}:#{e}:#{f}:#{g}:#{h}"
      _ -> nil
    end
  end

  defp valid_ip_for_geolocation?(ip) do
    ip && !String.starts_with?(ip, "127.") && !String.starts_with?(ip, "::1")
  end

  defp start_geolocation_task(ip) do
    Task.start(fn ->
      try do
        get_ip_location(ip)
      rescue
        _error ->
          send(self(), {:ip_location, @default_center})
      end
    end)
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

    # Remove markers for out-of-bounds packets
    socket =
      if packets_to_remove == [] do
        socket
      else
        Enum.reduce(packets_to_remove, socket, fn k, acc ->
          push_event(acc, "remove_marker", %{id: k})
        end)
      end

    # Fetch the latest packets within the new bounds and push to client (historical only)
    bounds_list = [map_bounds.west, map_bounds.south, map_bounds.east, map_bounds.north]
    now = DateTime.utc_now()
    one_hour_ago = DateTime.add(now, -3600, :second)

    packets =
      Aprs.Packets.get_packets_for_replay(%{
        bounds: bounds_list,
        start_time: one_hour_ago,
        end_time: now,
        limit: 100
      })

    marker_data =
      packets
      |> Enum.map(&build_packet_data/1)
      |> Enum.filter(& &1)

    socket =
      if Enum.any?(marker_data) do
        push_event(socket, "add_markers", %{markers: marker_data})
      else
        socket
      end

    # Only update visible_packets with the historical packets for the new bounds
    assign(socket, map_bounds: map_bounds, visible_packets: new_visible_packets)
  end

  @impl true
  def handle_info({:process_bounds_update, map_bounds}, socket), do: handle_info_process_bounds_update(map_bounds, socket)

  def handle_info({:delayed_zoom, %{lat: lat, lng: lng}}, socket), do: handle_info_delayed_zoom(lat, lng, socket)

  def handle_info({:ip_location, %{lat: lat, lng: lng}}, socket), do: handle_info_ip_location(lat, lng, socket)

  def handle_info(:initialize_replay, socket), do: handle_info_initialize_replay(socket)
  def handle_info(:replay_next_packet, socket), do: handle_replay_next_packet(socket)
  def handle_info(:cleanup_old_packets, socket), do: handle_cleanup_old_packets(socket)

  def handle_info({:postgres_packet, packet}, socket), do: handle_info_postgres_packet(packet, socket)

  def handle_info(%Phoenix.Socket.Broadcast{topic: "aprs_messages", event: "packet", payload: packet}, socket),
    do: handle_info({:postgres_packet, packet}, socket)

  # Private handler functions for each message type

  defp handle_info_process_bounds_update(map_bounds, socket) do
    socket = process_bounds_update(map_bounds, socket)
    {:noreply, socket}
  end

  defp handle_info_delayed_zoom(lat, lng, socket) do
    socket = push_event(socket, "zoom_to_location", %{lat: lat, lng: lng, zoom: 12})
    {:noreply, socket}
  end

  defp handle_info_ip_location(lat, lng, socket) do
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

    socket = assign(socket, map_center: %{lat: lat_float, lng: lng_float}, map_zoom: 12)

    socket =
      if socket.assigns.map_ready do
        push_event(socket, "zoom_to_location", %{lat: lat_float, lng: lng_float, zoom: 12})
      else
        assign(socket, pending_geolocation: %{lat: lat_float, lng: lng_float})
      end

    {:noreply, socket}
  end

  defp handle_info_initialize_replay(socket) do
    if not socket.assigns.replay_started and socket.assigns.map_ready do
      socket = start_historical_replay(socket)
      {:noreply, assign(socket, replay_started: true)}
    else
      {:noreply, socket}
    end
  end

  defp handle_info_postgres_packet(packet, socket) do
    {lat, lon, _data_extended} = get_coordinates(packet)

    callsign_key =
      if Map.has_key?(packet, "id"),
        do: to_string(packet["id"]),
        else: System.unique_integer([:positive])

    all_packets = Map.put(socket.assigns.all_packets, callsign_key, packet)
    socket = assign(socket, all_packets: all_packets)

    # Remove marker if packet is out of bounds but present
    if !is_nil(lat) and !is_nil(lon) and
         Map.has_key?(socket.assigns.visible_packets, callsign_key) and
         not within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds) do
      socket = push_event(socket, "remove_marker", %{id: callsign_key})
      new_visible_packets = Map.delete(socket.assigns.visible_packets, callsign_key)
      {:noreply, assign(socket, visible_packets: new_visible_packets)}
      # Only add marker if it is within bounds and not already present
    else
      if is_nil(lat) or is_nil(lon) or Map.has_key?(socket.assigns.visible_packets, callsign_key) do
        {:noreply, socket}
      else
        if within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds) do
          handle_valid_postgres_packet(packet, lat, lon, socket)
        else
          {:noreply, socket}
        end
      end
    end
  end

  defp handle_valid_postgres_packet(packet, lat, lon, socket) do
    if within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds) do
      buffer = [packet | socket.assigns.packet_buffer]
      socket = assign(socket, packet_buffer: buffer)

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

  defp handle_info_flush_packet_buffer(socket) do
    packets = Enum.reverse(socket.assigns.packet_buffer)
    visible_packets = socket.assigns.visible_packets
    all_packets = socket.assigns.all_packets

    {new_visible_packets, events, new_all_packets} =
      process_packet_buffer_with_all(packets, visible_packets, all_packets)

    socket =
      Enum.reduce(events, socket, fn {:new_packet, data}, acc ->
        push_event(acc, "new_packet", data)
      end)

    # Highlight the latest packet (open its popup)
    if packets != [] do
      latest_packet = List.last(packets)

      callsign_key =
        if Map.has_key?(latest_packet, "id"),
          do: to_string(latest_packet["id"]),
          else: System.unique_integer([:positive])

      push_event(socket, "highlight_packet", %{id: callsign_key})
    end

    socket =
      assign(socket,
        visible_packets: new_visible_packets,
        packet_buffer: [],
        buffer_timer: nil,
        all_packets: new_all_packets
      )

    {:noreply, socket}
  end

  # New helper to process buffer and update all_packets
  defp process_packet_buffer_with_all([], visible_packets, all_packets), do: {visible_packets, [], all_packets}

  defp process_packet_buffer_with_all([packet | rest], visible_packets, all_packets) do
    {vis, evs, allp} = process_packet_buffer_with_all(rest, visible_packets, all_packets)

    callsign_key =
      if Map.has_key?(packet, "id"),
        do: to_string(packet["id"]),
        else: System.unique_integer([:positive])

    allp = Map.put(allp, callsign_key, packet)

    case build_packet_data(packet) do
      nil ->
        {vis, evs, allp}

      packet_data ->
        {Map.put(vis, callsign_key, packet), [{:new_packet, packet_data} | evs], allp}
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
      handle_next_replay_packet(socket, packets, index, speed, historical_packets)
    else
      handle_replay_end(socket)
    end
  end

  defp handle_next_replay_packet(socket, packets, index, speed, historical_packets) do
    packet = Enum.at(packets, index)
    next_index = index + 1
    packet_data = build_packet_data(packet)

    socket =
      if packet_data do
        packet_data =
          Map.merge(packet_data, %{
            "is_historical" => true,
            "timestamp" =>
              if(Map.has_key?(packet, :received_at),
                do: DateTime.to_iso8601(packet.received_at)
              )
          })

        packet_id =
          "hist_#{if Map.has_key?(packet, :id), do: packet.id, else: System.unique_integer([:positive])}"

        new_historical_packets = Map.put(historical_packets, packet_id, packet)

        socket
        |> push_event("historical_packet", packet_data)
        |> assign(
          replay_index: next_index,
          historical_packets: new_historical_packets
        )
      else
        assign(socket, replay_index: next_index)
      end

    delay_ms = trunc(1000 / speed)
    timer_ref = Process.send_after(self(), :replay_next_packet, delay_ms)
    {:noreply, assign(socket, replay_timer_ref: timer_ref)}
  end

  defp handle_replay_end(socket) do
    Process.send_after(self(), :initialize_replay, 10_000)

    socket =
      assign(socket,
        replay_active: false,
        replay_timer_ref: nil,
        replay_started: false
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

  # Clean up expired markers from visible_packets and client, but do not re-query the DB
  defp handle_cleanup_old_packets(socket) do
    # Schedule next cleanup
    Process.send_after(self(), :cleanup_old_packets, 60_000)

    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)
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
    assign(socket, visible_packets: updated_visible_packets)
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

  @spec get_coordinates(map() | struct()) :: {number() | nil, number() | nil, map() | nil}
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
    data_extended = data_extended || %{}
    callsign = generate_callsign(packet)

    symbol_table_id =
      Map.get(data_extended, :symbol_table_id) || Map.get(data_extended, "symbol_table_id") || "/"

    symbol_code =
      Map.get(data_extended, :symbol_code) || Map.get(data_extended, "symbol_code") || ">"

    symbol_description =
      Map.get(data_extended, :symbol_description) || Map.get(data_extended, "symbol_description") ||
        "Symbol: #{symbol_table_id}#{symbol_code}"

    timestamp =
      cond do
        Map.has_key?(packet, :received_at) && packet.received_at ->
          DateTime.to_iso8601(packet.received_at)

        Map.has_key?(packet, "received_at") && packet["received_at"] ->
          DateTime.to_iso8601(packet["received_at"])

        true ->
          ""
      end

    comment = Map.get(data_extended, :comment) || Map.get(data_extended, "comment") || ""

    popup = """
    <div class=\"aprs-popup\">
      <div class=\"aprs-callsign\"><strong><a href=\"/#{callsign}\">#{callsign}</a></strong></div>
      <div class=\"aprs-symbol-info\">#{symbol_description}</div>
      #{if comment == "", do: "", else: "<div class=\\\"aprs-comment\\\">#{comment}</div>"}
      <div class=\"aprs-coords\">#{Float.round(lat, 4)}, #{Float.round(lon, 4)}</div>
      <div class=\"aprs-time\">#{timestamp}</div>
    </div>
    """

    %{
      "id" => callsign,
      "callsign" => callsign,
      "base_callsign" => packet.base_callsign || "",
      "ssid" => packet.ssid || "",
      "lat" => lat,
      "lng" => lon,
      "data_type" => to_string(packet.data_type || "unknown"),
      "path" => packet.path || "",
      "comment" => comment,
      "data_extended" => data_extended || %{},
      "symbol_table_id" => symbol_table_id,
      "symbol_code" => symbol_code,
      "symbol_description" => symbol_description,
      "timestamp" => timestamp,
      "popup" => popup
    }
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

  defp get_ip_location(ip) do
    url = "#{@ip_api_url}#{ip}"

    request =
      Finch.build(:get, url, [
        {"User-Agent", "APRS.me/1.0"},
        {"Accept", "application/json"}
      ])

    Process.sleep(2000)

    case Finch.request(request, @finch_name, receive_timeout: 10_000) do
      {:ok, %{status: 200, body: body}} -> handle_ip_api_response(body)
      {:ok, _response} -> send_default_ip_location()
      {:error, %{reason: :timeout}} -> send_default_ip_location()
      {:error, _error} -> send_default_ip_location()
    end
  end

  defp handle_ip_api_response(body) do
    case Jason.decode(body) do
      {:ok, %{"status" => "success", "lat" => lat, "lon" => lng}}
      when is_number(lat) and is_number(lng) ->
        if lat >= -90 and lat <= 90 and lng >= -180 and lng <= 180 do
          lat_float = lat / 1.0
          lng_float = lng / 1.0
          send(self(), {:ip_location, %{lat: lat_float, lng: lng_float}})
        else
          send_default_ip_location()
        end

      {:ok, _} ->
        send_default_ip_location()

      {:error, _decode_error} ->
        send_default_ip_location()
    end
  end

  defp send_default_ip_location, do: send(self(), {:ip_location, @default_center})

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
