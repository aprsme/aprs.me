defmodule AprsmeWeb.MapLive.CallsignView do
  @moduledoc false
  use AprsmeWeb, :live_view

  import Phoenix.LiveView, only: [connected?: 1, push_event: 3]

  alias Aprsme.Packets
  alias AprsmeWeb.Endpoint
  alias AprsmeWeb.MapLive.MapHelpers
  alias AprsmeWeb.MapLive.PacketUtils

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
        # Map bounds - will be set when first bounds update is received
        map_bounds: nil,
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
        latest_symbol_code: ">",
        # Flag to track if latest marker was already pushed
        latest_marker_pushed: false,
        # Indicate this is a map page for proper styling
        map_page: true
      )

    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")

      # Don't load packets here - wait for map_ready event
      # socket = load_callsign_packets(socket, normalized_callsign)

      # No longer need scheduled cleanup - packets are cleaned up automatically when new ones arrive
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

    # Immediately start historical replay without waiting for bounds
    # If bounds aren't available, use a default world bounds
    socket =
      if is_nil(socket.assigns.map_bounds) do
        # Use a default global bounds to ensure we get all historical points
        default_bounds = %{
          north: 90.0,
          south: -90.0,
          east: 180.0,
          west: -180.0
        }

        socket = assign(socket, map_bounds: default_bounds)
        socket = start_historical_replay(socket)
        assign(socket, replay_started: true, replay_active: true)
      else
        # Bounds already available, use them
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

    # Clean up old packets first
    socket = cleanup_old_packets(socket)

    # Remove out-of-bounds visible packets
    new_visible_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_k, packet} -> MapHelpers.within_bounds?(packet, normalized_bounds) end)
      |> Map.new()

    packets_to_remove =
      socket.assigns.visible_packets
      |> Enum.reject(fn {_k, packet} -> MapHelpers.within_bounds?(packet, normalized_bounds) end)
      |> Enum.map(fn {k, _} -> k end)

    socket =
      if packets_to_remove == [] do
        socket
      else
        Enum.reduce(packets_to_remove, socket, fn k, acc ->
          push_event(acc, "remove_marker", %{id: k})
        end)
      end

    socket = assign(socket, map_bounds: normalized_bounds, visible_packets: new_visible_packets)

    # Start historical replay now that we have bounds (if not already started)
    socket =
      if socket.assigns.map_ready and not socket.assigns.replay_started and
           not is_nil(socket.assigns.map_bounds) do
        socket = start_historical_replay(socket)
        assign(socket, replay_started: true, replay_active: true)
      else
        socket
      end

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
    # We now handle this in map_ready event, but keep this for backward compatibility
    # and in case auto_start_replay is triggered from elsewhere
    if socket.assigns.map_ready and not socket.assigns.replay_started do
      # Update socket with bounds if needed
      socket =
        if is_nil(socket.assigns.map_bounds) do
          # Use a default global bounds if needed
          default_bounds = %{
            north: 90.0,
            south: -90.0,
            east: 180.0,
            west: -180.0
          }

          assign(socket, map_bounds: default_bounds)
        else
          socket
        end

      socket = start_historical_replay(socket)
      {:noreply, assign(socket, replay_started: true, replay_active: true)}
    else
      # Only retry if map isn't ready yet
      if not socket.assigns.map_ready do
        Process.send_after(self(), :auto_start_replay, 1000)
      end

      {:noreply, socket}
    end
  end

  def handle_info(:replay_next_packet, socket), do: handle_replay_next_packet(socket)

  def handle_info(%Phoenix.Socket.Broadcast{topic: "aprs_messages", event: "packet", payload: packet}, socket) do
    # Only process packets for the specific callsign being viewed
    packet_sender = Map.get(packet, :sender, "")

    if String.upcase(packet_sender) == String.upcase(socket.assigns.callsign) do
      handle_info_postgres_packet(packet, socket)
    else
      {:noreply, socket}
    end
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  defp handle_info_postgres_packet(packet, socket) do
    # Clean up old packets before adding the new one
    socket = cleanup_old_packets(socket)

    key = System.unique_integer([:positive])
    updated_visible_packets = Map.put(socket.assigns.visible_packets, key, packet)
    socket = assign(socket, visible_packets: updated_visible_packets)

    # Live packets are always the most recent for their callsign
    locale = Map.get(socket.assigns, :locale, "en")
    packet_data = PacketUtils.build_packet_data(packet, true, locale)

    socket =
      if packet_data,
        do: push_event(socket, "new_packet", packet_data),
        else: socket

    {:noreply, socket}
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

  defp handle_replay_packet(packet, socket) do
    locale = Map.get(socket.assigns, :locale, "en")

    case PacketUtils.build_packet_data(packet, false, locale) do
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
        left: 60px;
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

      .aprs-info-link {
        font-size: 11px;
        color: #007cba;
        text-decoration: none;
        font-weight: normal;
        margin-left: 8px;
        padding: 2px 4px;
        border-radius: 3px;
        transition: background-color 0.2s;
      }

      .aprs-info-link:hover {
        background-color: rgba(0, 124, 186, 0.1);
        text-decoration: none;
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

      /* Hides the empty state dialog when not needed */
      .empty-state.hidden {
        display: none;
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
          <.link navigate="/" class="nav-link">← Back to Map</.link>
          <.link navigate="/packets" class="nav-link">All Packets</.link>
          <.link navigate={"/packets/#{String.downcase(@callsign)}"} class="nav-link">
            {@callsign} Packets
          </.link>
        </div>
      </div>

      <div class={[
        "empty-state",
        if(map_size(@visible_packets) > 0 or @replay_active, do: "hidden")
      ]}>
        <h3>Loading Historical Data</h3>
        <p>
          Loading packet history for {@callsign}...
        </p>
      </div>

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

  defp cleanup_old_packets(socket, age_threshold_seconds \\ 3600) do
    # Update packet age threshold
    threshold_time = DateTime.add(DateTime.utc_now(), -age_threshold_seconds, :second)
    socket = assign(socket, packet_age_threshold: threshold_time)

    # Remove expired packets from visible_packets
    expired_keys =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_key, packet} ->
        not packet_within_time_threshold?(packet, threshold_time)
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
        now,
        socket.assigns.map_bounds
      )

    # Always fetch the latest packet with a position, regardless of age
    latest_packet = get_latest_packet_for_callsign(socket.assigns.callsign)

    # Sort packets by inserted_at to identify the most recent
    sorted_packets = sort_packets_by_inserted_at(packets)

    # Filter out packets with unchanged positions (only keep if lat/lon changed)
    unique_position_packets = filter_unique_positions(sorted_packets)

    # Build packet data for all positions, marking which is the most recent
    packet_data_list = build_historical_packet_data_list(unique_position_packets, socket.assigns.callsign)

    # Always push the latest position as a live marker (not historical)
    {socket, latest_marker_pushed} = push_latest_marker(socket, latest_packet)

    if Enum.empty?(packet_data_list) do
      # No historical packets found (but latest marker may have been pushed above)
      assign(socket, latest_marker_pushed: latest_marker_pushed)
    else
      # Clear any previous historical packets from the map
      socket = push_event(socket, "clear_historical_packets", %{})
      # Send all historical packets at once (excluding the latest position)
      filtered_trail = filter_trail_excluding_latest(packet_data_list, latest_packet)

      socket = push_event(socket, "add_historical_packets", %{packets: filtered_trail})
      # Store historical packets in assigns for reference
      historical_packets_map = build_historical_packets_map(filtered_trail, unique_position_packets)

      assign(socket,
        historical_packets: historical_packets_map,
        replay_packets: [],
        replay_index: 0,
        replay_start_time: one_hour_ago,
        replay_end_time: now,
        latest_marker_pushed: latest_marker_pushed
      )
    end
  end

  defp get_latest_packet_for_callsign(callsign) do
    %{callsign: callsign}
    |> Packets.get_recent_packets()
    |> Enum.filter(&MapHelpers.has_position_data?/1)
    |> Enum.sort_by(& &1.received_at, {:desc, DateTime})
    |> List.first()
  end

  defp sort_packets_by_inserted_at(packets) do
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
  end

  defp build_historical_packet_data_list(unique_position_packets, callsign) do
    unique_position_packets
    |> Enum.with_index()
    |> Enum.map(fn {packet, index} ->
      build_single_historical_packet_data(packet, index, callsign)
    end)
    |> Enum.filter(& &1)
  end

  defp build_single_historical_packet_data(packet, index, callsign) do
    # Note: We don't have access to locale here, so we'll use default "en"
    case PacketUtils.build_packet_data(packet, false, "en") do
      nil ->
        nil

      packet_data ->
        # Generate a unique ID for this historical packet
        packet_id =
          "hist_#{if Map.has_key?(packet, :id), do: packet.id, else: System.unique_integer([:positive])}_#{index}"

        packet_data
        |> Map.put("id", packet_id)
        |> Map.put("is_historical", true)
        |> Map.put("is_most_recent_for_callsign", false)
        |> Map.put("callsign_group", callsign)
        |> Map.put("timestamp", get_packet_timestamp(packet))
    end
  end

  defp get_packet_timestamp(packet) do
    case packet.inserted_at do
      %NaiveDateTime{} = naive_dt ->
        DateTime.to_unix(DateTime.from_naive!(naive_dt, "Etc/UTC"), :millisecond)

      %DateTime{} = dt ->
        DateTime.to_unix(dt, :millisecond)

      _other ->
        DateTime.to_unix(DateTime.utc_now(), :millisecond)
    end
  end

  defp push_latest_marker(socket, latest_packet) do
    if latest_packet do
      locale = Map.get(socket.assigns, :locale, "en")
      packet_data = PacketUtils.build_packet_data(latest_packet, true, locale)

      if packet_data,
        do: {push_event(socket, "new_packet", packet_data), true},
        else: {socket, false}
    else
      {socket, false}
    end
  end

  defp filter_trail_excluding_latest(packet_data_list, latest_packet) do
    case latest_packet do
      nil ->
        packet_data_list

      _ ->
        latest_latlon = latest_packet |> MapHelpers.get_coordinates() |> Tuple.to_list() |> Enum.take(2)

        Enum.reject(packet_data_list, fn pd ->
          pd_latlon = [pd["lat"], pd["lng"]]

          abs(Enum.at(pd_latlon, 0) - Enum.at(latest_latlon, 0)) < 0.00001 and
            abs(Enum.at(pd_latlon, 1) - Enum.at(latest_latlon, 1)) < 0.00001
        end)
    end
  end

  defp build_historical_packets_map(filtered_trail, unique_position_packets) do
    filtered_trail
    |> Enum.zip(unique_position_packets)
    |> Enum.reduce(%{}, fn {packet_data, packet}, acc ->
      Map.put(acc, packet_data["id"], packet)
    end)
  end

  defp fetch_historical_packets_for_callsign(callsign, start_time, end_time, bounds) do
    params = %{
      callsign: callsign,
      start_time: start_time,
      end_time: end_time,
      limit: 1000
    }

    params =
      if bounds do
        Map.put(params, :bounds, [bounds.west, bounds.south, bounds.east, bounds.north])
      else
        params
      end

    Packets.get_packets_for_replay(params)
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

    # Clean up old packets before assigning new visible packets
    socket = cleanup_old_packets(socket)

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
    # Only push if we haven't already pushed it during historical replay
    if Map.get(socket.assigns, :latest_marker_pushed, false) do
      socket
    else
      locale = Map.get(socket.assigns, :locale, "en")
      packet_data = PacketUtils.build_packet_data(packet, true, locale)
      if packet_data, do: push_event(socket, "new_packet", packet_data), else: socket
    end
  end
end
