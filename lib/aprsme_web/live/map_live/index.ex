defmodule AprsmeWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsmeWeb, :live_view

  import AprsmeWeb.Components.ErrorBoundary
  import AprsmeWeb.TimeHelpers, only: [time_ago_in_words: 1]
  import Phoenix.LiveView, only: [connected?: 1, push_event: 3, push_navigate: 2, push_patch: 2]

  alias Aprsme.GeoUtils
  alias Aprsme.Packets.Clustering
  alias AprsmeWeb.Endpoint
  alias AprsmeWeb.MapLive.MapHelpers
  alias AprsmeWeb.MapLive.PacketUtils
  alias AprsmeWeb.MapLive.PopupComponent
  alias AprsmeWeb.TimeUtils
  alias Phoenix.HTML.Safe
  alias Phoenix.LiveView.Socket

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5

  # Parse map state from URL parameters
  @spec parse_map_params(map()) :: {map(), integer()}
  defp parse_map_params(params) do
    # Parse latitude (lat parameter)
    lat =
      case Map.get(params, "lat") do
        nil ->
          @default_center.lat

        lat_str ->
          case Float.parse(lat_str) do
            {lat_val, _} when lat_val >= -90 and lat_val <= 90 -> lat_val
            _ -> @default_center.lat
          end
      end

    # Parse longitude (lng parameter)
    lng =
      case Map.get(params, "lng") do
        nil ->
          @default_center.lng

        lng_str ->
          case Float.parse(lng_str) do
            {lng_val, _} when lng_val >= -180 and lng_val <= 180 -> lng_val
            _ -> @default_center.lng
          end
      end

    # Parse zoom level (z parameter)
    zoom =
      case Map.get(params, "z") do
        nil ->
          @default_zoom

        zoom_str ->
          case Integer.parse(zoom_str) do
            {zoom_val, _} when zoom_val >= 1 and zoom_val <= 20 -> zoom_val
            _ -> @default_zoom
          end
      end

    map_center = %{lat: lat, lng: lng}
    {map_center, zoom}
  end

  @impl true
  def mount(params, session, socket) do
    require Logger

    if connected?(socket) do
      # Subscribe to packet updates
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "packets")
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "bad_packets")

      # Schedule periodic cleanup of old packets
      Process.send_after(self(), :cleanup_old_packets, 60_000)
    end

    # Get deployment timestamp from config (set during application startup)
    deployed_at = Aprsme.Release.deployed_at()

    # Show 24 hours for more symbol variety
    one_hour_ago = TimeUtils.one_day_ago()

    # Parse map state from URL parameters
    {url_center, url_zoom} = parse_map_params(params)

    # Check for IP geolocation in session

    # Check if URL params were explicitly provided (not just defaults)
    has_explicit_url_params = params["lat"] || params["lng"] || params["z"]

    {map_center, map_zoom, should_skip_initial_url_update} =
      case session["ip_geolocation"] do
        %{"lat" => lat, "lng" => lng} when is_number(lat) and is_number(lng) ->
          if has_explicit_url_params do
            # URL params explicitly provided - use them

            {url_center, url_zoom, false}
          else
            # No explicit URL params - use IP geolocation
            geo_center = %{lat: lat, lng: lng}
            {geo_center, 11, true}
          end

        _ ->
          # No geolocation available, use URL params or defaults
          # Skip initial URL update if no explicit params were provided
          {url_center, url_zoom, !has_explicit_url_params}
      end

    socket = assign_defaults(socket, one_hour_ago)

    # Initialize the flag to track if initial historical load is completed
    socket = assign(socket, initial_historical_completed: false)

    # Calculate initial bounds based on center and zoom level
    initial_bounds = calculate_bounds_from_center_and_zoom(map_center, map_zoom)

    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")
    end

    {:ok,
     assign(socket,
       map_ready: false,
       map_bounds: initial_bounds,
       map_center: map_center,
       map_zoom: map_zoom,
       should_skip_initial_url_update: should_skip_initial_url_update,
       visible_packets: %{},
       historical_packets: %{},
       overlay_callsign: "",
       trail_duration: "1",
       historical_hours: "1",
       packet_age_threshold: one_hour_ago,
       slideover_open: true,
       deployed_at: deployed_at,
       map_page: true,
       packet_buffer: [],
       buffer_timer: nil,
       all_packets: %{},
       station_popup_open: false,
       initial_bounds_loaded: false,
       needs_initial_historical_load: false
     )}
  end

  # Calculate approximate bounds based on center point and zoom level
  # This provides initial bounds for database queries before client sends actual bounds
  @spec calculate_bounds_from_center_and_zoom(map(), integer()) :: map()
  defp calculate_bounds_from_center_and_zoom(center, zoom) do
    # Approximate degrees per pixel at different zoom levels
    # These are rough estimates for initial bounds calculation
    degrees_per_pixel =
      case zoom do
        z when z >= 15 -> 0.000005
        z when z >= 12 -> 0.00005
        z when z >= 10 -> 0.0002
        z when z >= 8 -> 0.001
        z when z >= 6 -> 0.005
        z when z >= 4 -> 0.02
        _ -> 0.1
      end

    # Assume viewport is roughly 800x600 pixels
    # Half of 600px height
    lat_offset = degrees_per_pixel * 300
    # Half of 800px width
    lng_offset = degrees_per_pixel * 400

    %{
      north: center.lat + lat_offset,
      south: center.lat - lat_offset,
      east: center.lng + lng_offset,
      west: center.lng - lng_offset
    }
  end

  @spec assign_defaults(Socket.t(), DateTime.t()) :: Socket.t()
  defp assign_defaults(socket, one_hour_ago) do
    assign(socket,
      packets: [],
      page_title: "APRS Map",
      visible_packets: %{},
      station_popup_open: false,
      map_bounds: %{
        north: 49.0,
        south: 24.0,
        east: -66.0,
        west: -125.0
      },
      map_center: @default_center,
      map_zoom: @default_zoom,
      historical_packets: %{},
      packet_age_threshold: one_hour_ago,
      map_ready: false,
      historical_loaded: false,
      bounds_update_timer: nil,
      pending_bounds: nil,
      initial_bounds_loaded: false,
      # Overlay controls
      overlay_callsign: "",
      trail_duration: "1",
      historical_hours: "1",
      # Slideover state - will be set based on screen size
      slideover_open: true
    )
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
  def handle_event("clear_and_reload_markers", _params, socket) do
    # Only filter the current visible_packets, do not re-query the database
    filtered_packets =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_callsign, packet} ->
        within_bounds?(packet, socket.assigns.map_bounds) &&
          packet_within_time_threshold?(packet, socket.assigns.packet_age_threshold)
      end)
      |> Map.new()

    locale = Map.get(socket.assigns, :locale, "en")

    visible_packets_list =
      filtered_packets
      |> Enum.map(fn {_callsign, packet} -> PacketUtils.build_packet_data(packet, false, locale) end)
      |> Enum.filter(& &1)

    socket = assign(socket, visible_packets: filtered_packets)

    # Check zoom level to decide between heat map and markers
    socket =
      if socket.assigns.map_zoom <= 8 do
        # Use heat map for low zoom levels
        send_heat_map_data(socket, filtered_packets)
      else
        # Use regular markers for high zoom levels
        if Enum.any?(visible_packets_list) do
          socket
          |> push_event("show_markers", %{})
          |> push_event("add_markers", %{markers: visible_packets_list})
        else
          socket
        end
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("map_ready", _params, socket) do
    require Logger

    # Mark map as ready and that we need to load historical packets
    socket =
      socket
      |> assign(map_ready: true)
      |> assign(needs_initial_historical_load: true)

    # If we have non-default center coordinates (e.g., from geolocation), apply them now
    socket =
      if socket.assigns.map_center.lat == @default_center.lat and
           socket.assigns.map_center.lng == @default_center.lng do
        socket
      else
        push_event(socket, "zoom_to_location", %{
          lat: socket.assigns.map_center.lat,
          lng: socket.assigns.map_center.lng,
          zoom: socket.assigns.map_zoom
        })
      end

    # Wait for JavaScript to send the actual map bounds before loading historical packets
    # The calculated bounds might be too small/inaccurate
    Logger.debug("Map ready - waiting for JavaScript to send actual bounds before loading historical packets")

    {:noreply, socket}
  end

  @impl true
  def handle_event("marker_clicked", _params, socket) do
    # When a marker is clicked, mark that a station popup is open
    {:noreply, assign(socket, station_popup_open: true)}
  end

  @impl true
  def handle_event("update_callsign", %{"callsign" => callsign}, socket) do
    {:noreply, assign(socket, overlay_callsign: callsign)}
  end

  @impl true
  def handle_event("update_trail_duration", %{"trail_duration" => duration}, socket) do
    # Convert duration string to hours and calculate new threshold
    hours = String.to_integer(duration)
    new_threshold = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)

    socket = assign(socket, trail_duration: duration, packet_age_threshold: new_threshold)

    # Trigger cleanup to remove packets that are now outside the new duration
    send(self(), :cleanup_old_packets)

    {:noreply, socket}
  end

  @impl true
  def handle_event("update_historical_hours", %{"historical_hours" => hours}, socket) do
    socket = assign(socket, historical_hours: hours)

    # Trigger a reload of historical packets with the new time range
    if socket.assigns.map_ready do
      send(self(), :reload_historical_packets)
    end

    {:noreply, socket}
  end

  @impl true
  def handle_event("search_callsign", %{"callsign" => callsign}, socket) do
    trimmed_callsign = callsign |> String.trim() |> String.upcase()

    if trimmed_callsign == "" do
      {:noreply, socket}
    else
      # Navigate to the callsign-specific route
      {:noreply, push_navigate(socket, to: "/#{trimmed_callsign}")}
    end
  end

  @impl true
  def handle_event("toggle_slideover", _params, socket) do
    {:noreply, assign(socket, slideover_open: !socket.assigns.slideover_open)}
  end

  @impl true
  def handle_event("set_slideover_state", %{"open" => open}, socket) do
    {:noreply, assign(socket, slideover_open: open)}
  end

  @impl true
  def handle_event("geolocation_error", %{"error" => _error}, socket) do
    # Handle geolocation errors gracefully - just continue without geolocation
    {:noreply, socket}
  end

  @impl true
  def handle_event("request_geolocation", _params, socket) do
    # This event is handled by the JavaScript hook
    {:noreply, socket}
  end

  @impl true
  def handle_event("popup_closed", _params, socket) do
    # When any popup is closed, mark that no station popup is open
    {:noreply, assign(socket, station_popup_open: false)}
  end

  @impl true
  def handle_event("get_assigns", _params, socket) do
    send(self(), {:test_assigns, socket.assigns})
    {:noreply, socket}
  end

  @impl true
  def handle_event("update_map_state", %{"center" => center, "zoom" => zoom} = params, socket) do
    require Logger

    Logger.debug("update_map_state event received: center=#{inspect(center)}, zoom=#{zoom}")

    # Parse center coordinates
    lat =
      case center do
        %{"lat" => lat_val} -> lat_val
        _ -> socket.assigns.map_center.lat
      end

    lng =
      case center do
        %{"lng" => lng_val} -> lng_val
        _ -> socket.assigns.map_center.lng
      end

    # Validate and clamp values
    lat = max(-90.0, min(90.0, lat))
    lng = max(-180.0, min(180.0, lng))
    zoom = max(1, min(20, zoom))

    map_center = %{lat: lat, lng: lng}

    # Check if we're crossing the heat map/marker threshold
    old_zoom = socket.assigns.map_zoom
    crossing_threshold = (old_zoom <= 8 and zoom > 8) or (old_zoom > 8 and zoom <= 8)

    # Update socket state
    socket = assign(socket, map_center: map_center, map_zoom: zoom)

    # If crossing threshold, trigger appropriate display mode
    socket =
      if crossing_threshold do
        if zoom <= 8 do
          # Switching to heat map
          socket = push_event(socket, "clear_all_markers", %{})
          send_heat_map_for_current_bounds(socket)
        else
          # Switching to markers
          trigger_marker_display(socket)
        end
      else
        socket
      end

    # Update URL without page reload, but skip on initial load if requested
    socket =
      if socket.assigns[:should_skip_initial_url_update] && !socket.assigns[:initial_bounds_loaded] do
        # Skip URL update on initial load
        Logger.debug("Skipping URL update on initial load")
        # Clear the flag after first update
        assign(socket, should_skip_initial_url_update: false)
      else
        new_path = "/?lat=#{lat}&lng=#{lng}&z=#{zoom}"
        Logger.debug("Updating URL to: #{new_path}")
        push_patch(socket, to: new_path, replace: true)
      end

    # If bounds are included, also process bounds update
    socket =
      case Map.get(params, "bounds") do
        %{"north" => north, "south" => south, "east" => east, "west" => west} ->
          map_bounds = %{
            north: north,
            south: south,
            east: east,
            west: west
          }

          # Trigger bounds processing if bounds changed OR if this is the initial load OR if we need initial historical load
          if socket.assigns.map_bounds != map_bounds or
               !socket.assigns[:initial_bounds_loaded] or
               socket.assigns[:needs_initial_historical_load] do
            require Logger

            Logger.debug(
              "Sending bounds update (initial_load: #{!socket.assigns[:initial_bounds_loaded]}, needs_historical: #{socket.assigns[:needs_initial_historical_load]}): #{inspect(map_bounds)}"
            )

            send(self(), {:process_bounds_update, map_bounds})
          end

          socket

        _ ->
          socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event(
        "error_boundary_triggered",
        %{"message" => message, "stack" => stack, "component_id" => component_id},
        socket
      ) do
    # Log the error for monitoring
    require Logger

    Logger.error("Error boundary triggered in component #{component_id}: #{message}\n#{stack}")

    # You could also send this to an error tracking service here
    # ErrorTracker.report_error(message, stack, %{component: component_id, user_id: socket.assigns[:current_user_id]})

    {:noreply, socket}
  end

  @impl true
  def handle_params(params, _url, socket) do
    # Check if we should skip this update (e.g., when using IP geolocation on initial load)
    if Map.get(socket.assigns, :should_skip_initial_url_update, false) and
         not Map.get(socket.assigns, :map_ready, false) do
      # Skip the URL parameter update to preserve IP geolocation
      socket = assign(socket, should_skip_initial_url_update: false)
      {:noreply, socket}
    else
      # Parse new map state from URL parameters
      {map_center, map_zoom} = parse_map_params(params)

      # Update socket state
      socket = assign(socket, map_center: map_center, map_zoom: map_zoom)

      # If map is ready, update the client-side map
      socket =
        if socket.assigns.map_ready do
          push_event(socket, "zoom_to_location", %{
            lat: map_center.lat,
            lng: map_center.lng,
            zoom: map_zoom
          })
        else
          socket
        end

      {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:process_bounds_update, map_bounds}, socket), do: handle_info_process_bounds_update(map_bounds, socket)

  def handle_info(:initialize_replay, socket), do: handle_info_initialize_replay(socket)

  def handle_info(:cleanup_old_packets, socket), do: handle_cleanup_old_packets(socket)

  def handle_info(:reload_historical_packets, socket), do: handle_reload_historical_packets(socket)

  def handle_info({:postgres_packet, packet}, socket), do: handle_info_postgres_packet(packet, socket)

  def handle_info({:load_historical_batch, batch_offset}, socket) do
    socket = load_historical_batch(socket, batch_offset)
    {:noreply, socket}
  end

  def handle_info(%Phoenix.Socket.Broadcast{topic: "aprs_messages", event: "packet", payload: packet}, socket),
    do: handle_info({:postgres_packet, packet}, socket)

  # Private handler functions for each message type

  defp handle_info_process_bounds_update(map_bounds, socket) do
    require Logger

    # Check if we need to process this bounds update
    should_process =
      !socket.assigns[:initial_bounds_loaded] or
        socket.assigns[:needs_initial_historical_load] or
        not compare_bounds(map_bounds, socket.assigns.map_bounds)

    Logger.debug(
      "handle_info_process_bounds_update - should_process: #{should_process}, initial_bounds_loaded: #{socket.assigns[:initial_bounds_loaded]}, needs_initial_historical_load: #{socket.assigns[:needs_initial_historical_load]}"
    )

    if should_process do
      Logger.debug("Processing bounds update: #{inspect(map_bounds)}")
      socket = process_bounds_update(map_bounds, socket)
      socket = assign(socket, initial_bounds_loaded: true)
      {:noreply, socket}
    else
      Logger.debug("Skipping bounds update - no change detected")
      {:noreply, socket}
    end
  end

  defp handle_info_initialize_replay(socket) do
    if not socket.assigns.historical_loaded and socket.assigns.map_ready do
      # Only proceed if we have actual map bounds - don't use world bounds
      if socket.assigns.map_bounds do
        # Use progressive loading for better performance
        socket = start_progressive_historical_loading(socket)
        socket = assign(socket, historical_loaded: true)
        {:noreply, socket}
      else
        # Wait a bit longer for map bounds to be available
        # Increase delay to give client more time to send real bounds
        Process.send_after(self(), :initialize_replay, 500)
        {:noreply, socket}
      end
    else
      {:noreply, socket}
    end
  end

  defp handle_info_postgres_packet(packet, socket) do
    {lat, lon, _data_extended} = MapHelpers.get_coordinates(packet)
    callsign_key = get_callsign_key(packet)

    # Update all_packets
    all_packets = Map.put(socket.assigns.all_packets, callsign_key, packet)
    socket = assign(socket, all_packets: all_packets)

    # Handle packet visibility logic
    handle_packet_visibility(packet, lat, lon, callsign_key, socket)
  end

  defp get_callsign_key(packet) do
    if Map.has_key?(packet, "id"),
      do: to_string(packet["id"]),
      else: System.unique_integer([:positive])
  end

  defp handle_packet_visibility(packet, lat, lon, callsign_key, socket) do
    cond do
      should_remove_marker?(lat, lon, callsign_key, socket) ->
        remove_marker_from_map(callsign_key, socket)

      should_add_marker?(lat, lon, callsign_key, socket) ->
        handle_valid_postgres_packet(packet, lat, lon, socket)

      should_update_marker?(lat, lon, callsign_key, socket) ->
        # Marker exists and is within bounds - check if there's significant movement
        existing_packet = socket.assigns.visible_packets[callsign_key]
        {existing_lat, existing_lon, _} = MapHelpers.get_coordinates(existing_packet)

        # Check if we have valid existing coordinates
        if is_number(existing_lat) and is_number(existing_lon) and
             GeoUtils.significant_movement?(existing_lat, existing_lon, lat, lon, 15) do
          # Significant movement detected (more than 15 meters), update the marker
          handle_valid_postgres_packet(packet, lat, lon, socket)
        else
          # Just GPS drift or invalid coordinates, update the packet data but don't send visual update
          new_visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, packet)
          socket = assign(socket, visible_packets: new_visible_packets)
          {:noreply, socket}
        end

      true ->
        {:noreply, socket}
    end
  end

  defp should_remove_marker?(lat, lon, callsign_key, socket) do
    !is_nil(lat) and !is_nil(lon) and
      Map.has_key?(socket.assigns.visible_packets, callsign_key) and
      not MapHelpers.within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
  end

  defp should_add_marker?(lat, lon, callsign_key, socket) do
    !is_nil(lat) and !is_nil(lon) and
      not Map.has_key?(socket.assigns.visible_packets, callsign_key) and
      MapHelpers.within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
  end

  defp should_update_marker?(lat, lon, callsign_key, socket) do
    !is_nil(lat) and !is_nil(lon) and
      Map.has_key?(socket.assigns.visible_packets, callsign_key) and
      MapHelpers.within_bounds?(%{lat: lat, lon: lon}, socket.assigns.map_bounds)
  end

  defp remove_marker_from_map(callsign_key, socket) do
    socket = push_event(socket, "remove_marker", %{id: callsign_key})
    new_visible_packets = Map.delete(socket.assigns.visible_packets, callsign_key)
    {:noreply, assign(socket, visible_packets: new_visible_packets)}
  end

  defp handle_valid_postgres_packet(packet, _lat, _lon, socket) do
    # Add the packet to visible_packets and push a marker immediately
    callsign_key =
      if Map.has_key?(packet, "id"),
        do: to_string(packet["id"]),
        else: System.unique_integer([:positive])

    new_visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, packet)
    socket = assign(socket, visible_packets: new_visible_packets)

    # Check zoom level to decide how to display the packet
    socket =
      if socket.assigns.map_zoom <= 8 do
        # We're in heat map mode - update the heat map with all current data
        send_heat_map_for_current_bounds(socket)
      else
        # We're in marker mode - send individual marker
        locale = Map.get(socket.assigns, :locale, "en")
        marker_data = PacketUtils.build_packet_data(packet, true, locale)

        if marker_data do
          # Only show new packet popup if no station popup is currently open
          if socket.assigns.station_popup_open do
            # Send without opening popup to avoid interrupting user
            push_event(socket, "new_packet", Map.put(marker_data, :openPopup, false))
          else
            push_event(socket, "new_packet", marker_data)
          end
        else
          socket
        end
      end

    {:noreply, socket}
  end

  # Handle replaying the next historical packet

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
    <script
      src="https://cdnjs.cloudflare.com/ajax/libs/OverlappingMarkerSpiderfier-Leaflet/0.2.6/oms.min.js"
    >
    </script>

    <style>
      #aprs-map {
        position: absolute;
        top: 0;
        left: 0;
        bottom: 0;
        height: 100vh;
        z-index: 1;
        transition: right 0.3s ease-in-out;
      }

      /* Desktop styles */
      @media (min-width: 1024px) {
        #aprs-map.slideover-open {
          right: 352px;
        }

        #aprs-map.slideover-closed {
          right: 0;
        }
      }

      /* Mobile styles */
      @media (max-width: 1023px) {
        #aprs-map {
          right: 0 !important;
        }
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
        color: #374151;
        margin-bottom: 4px;
        word-wrap: break-word;
      }

      .aprs-coords {
        color: #6b7280;
        font-size: 11px;
        font-family: monospace;
      }

      .aprs-timestamp {
        color: #6b7280;
        font-size: 11px;
        font-family: monospace;
        padding-top: 4px;
      }

      /* Leaflet popup improvements for APRS data */
      .leaflet-popup-content-wrapper {
        border-radius: 8px;
      }

      .leaflet-popup-content {
        margin: 8px 12px;
      }
    </style>

    <.error_boundary id="map-error-boundary">
      <div
        id="aprs-map"
        class={if @slideover_open, do: "slideover-open", else: "slideover-closed"}
        phx-hook="APRSMap"
        phx-update="ignore"
        data-center={Jason.encode!(@map_center)}
        data-zoom={@map_zoom}
      >
      </div>
    </.error_boundary>

    <button
      class="locate-button"
      phx-click="locate_me"
      title={Gettext.gettext(AprsmeWeb.Gettext, "Find my location")}
    >
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="20"
        height="20"
        viewBox="0 0 24 24"
        fill="#374151"
        stroke="none"
      >
        <path d="M12 2L4.5 20.29l.71.71L12 18l6.79 3 .71-.71z" />
      </svg>
    </button>

    <!-- Slideover Toggle Button -->
    <button
      class={["slideover-toggle", if(@slideover_open, do: "slideover-open", else: "slideover-closed")]}
      phx-click="toggle_slideover"
      title={
        if @slideover_open,
          do: Gettext.gettext(AprsmeWeb.Gettext, "Hide controls"),
          else: Gettext.gettext(AprsmeWeb.Gettext, "Show controls")
      }
    >
      <%= if @slideover_open do %>
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
          <path d="m9 18 6-6-6-6" />
        </svg>
      <% else %>
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
          <path d="m15 18-6-6 6-6" />
        </svg>
      <% end %>
    </button>

    <!-- Mobile Backdrop -->
    <%= if @slideover_open do %>
      <div
        class="fixed inset-0 bg-black bg-opacity-50 z-[999] lg:hidden backdrop-blur-sm"
        phx-click="toggle_slideover"
      >
      </div>
    <% end %>

    <!-- Control Panel Overlay -->
    <div
      id="slideover-panel"
      class={[
        "slideover-panel",
        if(@slideover_open, do: "slideover-open", else: "slideover-closed")
      ]}
      phx-hook="ResponsiveSlideoverHook"
    >
      <!-- Header -->
      <div class="flex items-center justify-between p-6 border-b border-slate-200 bg-gradient-to-r from-indigo-600 to-purple-600 text-white">
        <div class="flex items-center space-x-2">
          <svg
            class="w-6 h-6"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
            />
          </svg>
          <h2 class="text-xl font-bold">APRS.me</h2>
        </div>
        
    <!-- Close button for mobile -->
        <button
          class="lg:hidden text-white hover:text-slate-200 transition-colors"
          phx-click="toggle_slideover"
          title={Gettext.gettext(AprsmeWeb.Gettext, "Close controls")}
        >
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="24"
            height="24"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            stroke-linecap="round"
            stroke-linejoin="round"
          >
            <path d="m18 6-12 12" />
            <path d="m6 6 12 12" />
          </svg>
        </button>
      </div>
      
    <!-- Content -->
      <div class="p-6 space-y-6 bg-slate-50 flex-1 overflow-y-auto">
        <!-- Callsign Search -->
        <div class="space-y-4">
          <label class="block text-sm font-semibold text-slate-700 flex items-center space-x-2">
            <svg class="w-4 h-4 text-indigo-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
              />
            </svg>
            <span>{gettext("Search Callsign")}</span>
          </label>
          <form phx-submit="search_callsign" class="flex space-x-2">
            <input
              type="text"
              name="callsign"
              value={@overlay_callsign}
              phx-change="update_callsign"
              placeholder={gettext("Enter callsign...")}
              class="flex-1 px-4 py-3 border border-slate-300 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm uppercase placeholder-slate-400 transition-all duration-200 hover:border-slate-400"
            />
            <button
              type="submit"
              class="px-6 py-3 bg-gradient-to-r from-indigo-600 to-purple-600 text-white rounded-xl hover:from-indigo-700 hover:to-purple-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 transition-all duration-200 text-sm font-semibold shadow-lg hover:shadow-xl"
            >
              {gettext("Search")}
            </button>
          </form>
        </div>
        
    <!-- Trail Duration -->
        <div class="space-y-4">
          <label class="block text-sm font-semibold text-slate-700 flex items-center space-x-2">
            <svg
              class="w-4 h-4 text-emerald-500"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <span>{gettext("Trail Duration")}</span>
          </label>
          <form phx-change="update_trail_duration" class="relative">
            <select
              name="trail_duration"
              class="w-full px-4 py-3 border border-slate-300 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm bg-white text-gray-900 appearance-none transition-all duration-200 hover:border-slate-400"
            >
              <option value="1" selected={@trail_duration == "1"}>
                {ngettext("1 Hour", "%{count} Hours", 1)}
              </option>
              <option value="6" selected={@trail_duration == "6"}>
                {ngettext("1 Hour", "%{count} Hours", 6)}
              </option>
              <option value="12" selected={@trail_duration == "12"}>
                {ngettext("1 Hour", "%{count} Hours", 12)}
              </option>
              <option value="24" selected={@trail_duration == "24"}>
                {ngettext("1 Hour", "%{count} Hours", 24)}
              </option>
              <option value="48" selected={@trail_duration == "48"}>
                {ngettext("1 Hour", "%{count} Hours", 48)}
              </option>
              <option value="168" selected={@trail_duration == "168"}>
                {gettext("1 Week")}
              </option>
            </select>
            <div class="absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none">
              <svg
                class="w-4 h-4 text-slate-400"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  stroke-width="2"
                  d="M19 9l-7 7-7-7"
                />
              </svg>
            </div>
          </form>
        </div>
        
    <!-- Historical Data -->
        <div class="space-y-4">
          <label class="block text-sm font-semibold text-slate-700 flex items-center space-x-2">
            <svg class="w-4 h-4 text-amber-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <span>{gettext("Historical Data")}</span>
          </label>
          <form phx-change="update_historical_hours" class="relative">
            <select
              name="historical_hours"
              class="w-full px-4 py-3 border border-slate-300 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm bg-white text-gray-900 appearance-none transition-all duration-200 hover:border-slate-400"
            >
              <option value="1" selected={@historical_hours == "1"}>
                {ngettext("1 Hour", "%{count} Hours", 1)}
              </option>
              <option value="3" selected={@historical_hours == "3"}>
                {ngettext("1 Hour", "%{count} Hours", 3)}
              </option>
              <option value="6" selected={@historical_hours == "6"}>
                {ngettext("1 Hour", "%{count} Hours", 6)}
              </option>
              <option value="12" selected={@historical_hours == "12"}>
                {ngettext("1 Hour", "%{count} Hours", 12)}
              </option>
              <option value="24" selected={@historical_hours == "24"}>
                {ngettext("1 Hour", "%{count} Hours", 24)}
              </option>
            </select>
            <div class="absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none">
              <svg
                class="w-4 h-4 text-slate-400"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  stroke-width="2"
                  d="M19 9l-7 7-7-7"
                />
              </svg>
            </div>
          </form>
        </div>
        
    <!-- Navigation -->
        <div class="pt-4 border-t border-slate-200 space-y-3">
          <div class="flex items-center space-x-2 text-sm text-slate-600 mb-3">
            <svg class="w-4 h-4 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2H5a2 2 0 00-2-2z"
              />
            </svg>
            <span class="font-medium">{gettext("Navigation")}</span>
          </div>
          <.navigation variant={:vertical} class="text-sm" current_user={@current_user} />
        </div>
        
    <!-- Deployment Information -->
        <div class="pt-4 border-t border-slate-200 space-y-3">
          <div class="flex items-center space-x-2 text-sm text-slate-600">
            <svg class="w-4 h-4 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <span class="font-medium">{gettext("Last Deploy")}</span>
          </div>
          <div class="text-xs text-slate-500">
            <div class="font-mono">{time_ago_in_words(@deployed_at)}</div>
            <div class="font-mono text-slate-400">
              {Calendar.strftime(@deployed_at, "%Y-%m-%d %H:%M UTC")}
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  # Clean up expired markers from visible_packets and client, but do not re-query the DB
  defp handle_cleanup_old_packets(socket) do
    # Schedule next cleanup
    Process.send_after(self(), :cleanup_old_packets, 60_000)

    # Use the current packet_age_threshold instead of hardcoded one hour
    threshold = socket.assigns.packet_age_threshold

    # Remove expired packets from visible_packets
    expired_keys =
      socket.assigns.visible_packets
      |> Enum.filter(fn {_key, packet} ->
        not packet_within_time_threshold?(packet, threshold)
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
    {:noreply, assign(socket, visible_packets: updated_visible_packets)}
  end

  defp handle_reload_historical_packets(socket) do
    require Logger

    Logger.debug(
      "handle_reload_historical_packets called - map_ready: #{socket.assigns.map_ready}, map_bounds: #{inspect(socket.assigns.map_bounds)}"
    )

    if socket.assigns.map_ready and socket.assigns.map_bounds do
      # Clear existing historical packets
      socket = push_event(socket, "clear_historical_packets", %{})

      # Start progressive loading using LiveView's efficient batching
      socket = start_progressive_historical_loading(socket)

      {:noreply, socket}
    else
      Logger.debug("Skipping historical reload - conditions not met")
      {:noreply, socket}
    end
  end

  # Check if a packet is within the time threshold (not too old)
  @spec packet_within_time_threshold?(struct(), any()) :: boolean()
  defp packet_within_time_threshold?(packet, threshold) do
    case packet do
      %{received_at: received_at} when not is_nil(received_at) ->
        threshold_dt = convert_threshold_to_datetime(threshold)
        DateTime.compare(received_at, threshold_dt) in [:gt, :eq]

      _ ->
        # If no timestamp, treat as current
        true
    end
  end

  defp convert_threshold_to_datetime(threshold) do
    cond do
      is_integer(threshold) ->
        # Assume seconds since epoch
        DateTime.from_unix!(threshold)

      is_binary(threshold) ->
        case DateTime.from_iso8601(threshold) do
          {:ok, dt, _} -> dt
          _ -> DateTime.utc_now()
        end

      match?(%DateTime{}, threshold) ->
        threshold

      true ->
        DateTime.utc_now()
    end
  end

  # Helper functions

  # Fetch historical packets from the database

  # Select the best packet to display for a callsign - prioritize position over weather
  defp select_best_packet_for_display(packets) do
    # Separate position and weather packets using the same logic as PacketUtils.weather_packet?
    {position_packets, weather_packets} =
      Enum.split_with(packets, fn packet ->
        # A packet is a position packet if it's NOT a weather packet
        not PacketUtils.weather_packet?(packet)
      end)

    # Prefer the most recent position packet, fall back to most recent weather packet
    case position_packets do
      [] ->
        # No position packets, use most recent weather packet
        hd(weather_packets)

      [single_position] ->
        # Only one position packet, use it
        single_position

      position_list ->
        # Multiple position packets, use most recent one
        Enum.max_by(position_list, & &1.received_at, DateTime)
    end
  end

  defp build_packet_data_list(historical_packets) do
    # Group by callsign and identify most recent packet for each
    grouped_packets =
      Enum.group_by(historical_packets, fn packet ->
        packet.sender || "unknown"
      end)

    # Batch fetch weather information for all callsigns to avoid N+1 queries
    callsigns = Map.keys(grouped_packets)
    weather_callsigns = get_weather_callsigns_batch(callsigns)

    # For each callsign group, find the most recent packet and mark it appropriately
    grouped_packets
    |> Enum.flat_map(fn {callsign, packets} ->
      # Sort by received_at to find most recent
      sorted_packets = Enum.sort_by(packets, & &1.received_at, {:desc, DateTime})

      case sorted_packets do
        [] ->
          []

        packets_list ->
          # Find the best packet to display as "current" - prioritize position over weather
          selected_packet = select_best_packet_for_display(packets_list)
          historical = Enum.reject(packets_list, &(&1.id == selected_packet.id))

          # Always include the selected packet
          has_weather = MapSet.member?(weather_callsigns, String.upcase(callsign))
          most_recent_data = build_minimal_packet_data(selected_packet, true, has_weather)

          # Get coordinates of selected packet for distance filtering
          {most_recent_lat, most_recent_lon, _} = MapHelpers.get_coordinates(selected_packet)

          # Filter historical packets that are too close to most recent position
          filtered_historical =
            if most_recent_lat && most_recent_lon do
              Enum.filter(historical, fn packet ->
                {lat, lon, _} = MapHelpers.get_coordinates(packet)

                if lat && lon do
                  distance_meters = calculate_distance_meters(most_recent_lat, most_recent_lon, lat, lon)
                  # Only show if 10+ meters away
                  distance_meters >= 10.0
                else
                  # Skip packets without coordinates
                  false
                end
              end)
            else
              # If most recent has no coordinates, include all historical
              historical
            end

          # Build data for remaining historical packets
          historical_data =
            filtered_historical
            |> Enum.map(fn packet -> build_minimal_packet_data(packet, false, has_weather) end)
            |> Enum.filter(& &1)

          # Combine most recent and filtered historical
          Enum.filter([most_recent_data | historical_data], & &1)
      end
    end)
    |> Enum.filter(& &1)
  end

  defp build_minimal_packet_data(packet, is_most_recent, has_weather) do
    # Build minimal packet data without calling expensive PacketUtils.build_packet_data
    {lat, lon, _} = MapHelpers.get_coordinates(packet)

    if lat && lon do
      # Use PacketUtils to get symbol information properly (includes data_extended fallback)
      symbol_table_id = PacketUtils.get_packet_field(packet, :symbol_table_id, "/")
      symbol_code = PacketUtils.get_packet_field(packet, :symbol_code, ">")

      # Generate symbol HTML using the SymbolRenderer
      symbol_html =
        AprsmeWeb.SymbolRenderer.render_marker_symbol(
          symbol_table_id,
          symbol_code,
          packet.sender || "",
          32
        )

      %{
        "id" => if(is_most_recent, do: "current_#{packet.id}", else: "hist_#{packet.id}"),
        "lat" => lat,
        "lng" => lon,
        "callsign" => packet.sender || "",
        "symbol_table_id" => symbol_table_id,
        "symbol_code" => symbol_code,
        "symbol_html" => symbol_html,
        "comment" => packet.comment || "",
        "timestamp" => DateTime.to_unix(packet.received_at || DateTime.utc_now(), :millisecond),
        "historical" => !is_most_recent,
        "is_most_recent_for_callsign" => is_most_recent,
        "popup" => build_simple_popup(packet, has_weather)
      }
    end
  end

  defp build_simple_popup(packet, has_weather) do
    # Build popup HTML directly without database queries
    callsign = packet.sender || "Unknown"
    timestamp_dt = packet.received_at || DateTime.utc_now()
    cache_buster = System.system_time(:millisecond)

    # Check if this packet itself is a weather packet
    is_weather = PacketUtils.weather_packet?(packet)

    if is_weather do
      # Build weather popup
      %{
        callsign: callsign,
        comment: nil,
        timestamp_dt: timestamp_dt,
        cache_buster: cache_buster,
        weather: true,
        weather_link: true,
        temperature: PacketUtils.get_weather_field(packet, :temperature),
        temp_unit: "°F",
        humidity: PacketUtils.get_weather_field(packet, :humidity),
        wind_direction: PacketUtils.get_weather_field(packet, :wind_direction),
        wind_speed: PacketUtils.get_weather_field(packet, :wind_speed),
        wind_unit: "mph",
        wind_gust: PacketUtils.get_weather_field(packet, :wind_gust),
        gust_unit: "mph",
        pressure: PacketUtils.get_weather_field(packet, :pressure),
        rain_1h: PacketUtils.get_weather_field(packet, :rain_1h),
        rain_24h: PacketUtils.get_weather_field(packet, :rain_24h),
        rain_since_midnight: PacketUtils.get_weather_field(packet, :rain_since_midnight),
        rain_1h_unit: "in",
        rain_24h_unit: "in",
        rain_since_midnight_unit: "in"
      }
      |> PopupComponent.popup()
      |> Safe.to_iodata()
      |> IO.iodata_to_binary()
    else
      # Build standard popup
      %{
        callsign: callsign,
        comment: packet.comment || "",
        timestamp_dt: timestamp_dt,
        cache_buster: cache_buster,
        weather: false,
        # Use pre-fetched weather info
        weather_link: has_weather
      }
      |> PopupComponent.popup()
      |> Safe.to_iodata()
      |> IO.iodata_to_binary()
    end
  end

  # Batch fetch weather callsigns to avoid N+1 queries
  defp get_weather_callsigns_batch(callsigns) when is_list(callsigns) do
    import Ecto.Query

    # Normalize callsigns
    normalized_callsigns = Enum.map(callsigns, &String.upcase/1)

    # Single query to find all callsigns that have weather packets
    query =
      from p in Aprsme.Packet,
        where: fragment("UPPER(?)", p.sender) in ^normalized_callsigns,
        where: p.data_type == "weather" or (p.symbol_table_id == "/" and p.symbol_code == "_"),
        select: fragment("UPPER(?)", p.sender),
        distinct: true

    weather_callsigns = Aprsme.Repo.all(query)
    MapSet.new(weather_callsigns)
  rescue
    _ -> MapSet.new()
  end

  # Calculate distance between two lat/lon points in meters using Haversine formula
  defp calculate_distance_meters(lat1, lon1, lat2, lon2) do
    # Convert latitude and longitude from degrees to radians
    lat1_rad = lat1 * :math.pi() / 180
    lon1_rad = lon1 * :math.pi() / 180
    lat2_rad = lat2 * :math.pi() / 180
    lon2_rad = lon2 * :math.pi() / 180

    # Haversine formula
    dlat = lat2_rad - lat1_rad
    dlon = lon2_rad - lon1_rad

    a =
      :math.sin(dlat / 2) * :math.sin(dlat / 2) +
        :math.cos(lat1_rad) * :math.cos(lat2_rad) *
          :math.sin(dlon / 2) * :math.sin(dlon / 2)

    c = 2 * :math.atan2(:math.sqrt(a), :math.sqrt(1 - a))

    # Earth's radius in meters
    earth_radius_meters = 6_371_000

    # Distance in meters
    earth_radius_meters * c
  end

  # Progressive loading functions using LiveView's efficient update mechanisms
  @spec start_progressive_historical_loading(Socket.t()) :: Socket.t()
  defp start_progressive_historical_loading(socket) do
    require Logger

    Logger.debug(
      "start_progressive_historical_loading called with zoom: #{socket.assigns.map_zoom}, bounds: #{inspect(socket.assigns.map_bounds)}"
    )

    # Don't clear historical packets here - let the caller decide if clearing is needed
    # This prevents race conditions where we clear packets that were just loaded

    # For high zoom levels, load everything in one batch for maximum speed
    zoom = socket.assigns.map_zoom || 5

    if zoom >= 10 do
      # High zoom - load everything at once for maximum speed
      Logger.debug("High zoom (#{zoom}), loading in single batch")

      socket
      |> assign(loading_batch: 0, total_batches: 1)
      |> load_historical_batch(0)
    else
      # Low zoom - use progressive loading to prevent overwhelming
      total_batches = calculate_batch_count_for_zoom(zoom)

      # Start with first batch
      socket =
        socket
        |> assign(loading_batch: 0, total_batches: total_batches)
        |> load_historical_batch(0)

      # Load all remaining batches immediately - no delays
      Enum.each(1..(total_batches - 1), fn batch_index ->
        send(self(), {:load_historical_batch, batch_index})
      end)

      socket
    end
  end

  # Calculate optimal batch size based on zoom level
  # Higher zoom = smaller viewport = load everything at once for speed
  @spec calculate_batch_size_for_zoom(integer()) :: integer()
  # High zoom - load everything at once (up to 500 packets)
  defp calculate_batch_size_for_zoom(zoom) when zoom >= 10, do: 500
  # Medium zoom
  defp calculate_batch_size_for_zoom(zoom) when zoom >= 8, do: 100
  # Zoomed out
  defp calculate_batch_size_for_zoom(zoom) when zoom >= 5, do: 75
  # Very zoomed out
  defp calculate_batch_size_for_zoom(_), do: 50

  # Calculate optimal number of batches based on zoom level
  # Higher zoom = fewer batches needed since viewport is smaller
  @spec calculate_batch_count_for_zoom(integer()) :: integer()
  # Very zoomed in - fewer batches
  defp calculate_batch_count_for_zoom(zoom) when zoom >= 15, do: 2
  # Moderately zoomed in
  defp calculate_batch_count_for_zoom(zoom) when zoom >= 12, do: 3
  # Medium zoom
  defp calculate_batch_count_for_zoom(zoom) when zoom >= 8, do: 4
  # Zoomed out - more batches
  defp calculate_batch_count_for_zoom(_), do: 5

  @spec load_historical_batch(Socket.t(), integer()) :: Socket.t()
  defp load_historical_batch(socket, batch_offset) do
    if socket.assigns.map_bounds do
      bounds = [
        socket.assigns.map_bounds.west,
        socket.assigns.map_bounds.south,
        socket.assigns.map_bounds.east,
        socket.assigns.map_bounds.north
      ]

      # Calculate zoom-based batch size - higher zoom = smaller batches for faster response
      zoom = socket.assigns.map_zoom || 5
      batch_size = calculate_batch_size_for_zoom(zoom)
      offset = batch_offset * batch_size

      packets_module = Application.get_env(:aprsme, :packets_module, Aprsme.Packets)

      historical_packets =
        try do
          if packets_module == Aprsme.Packets do
            # Use cached queries for better performance
            # Include zoom level in cache key for better cache efficiency
            Aprsme.CachedQueries.get_recent_packets_cached(%{
              bounds: bounds,
              limit: batch_size,
              offset: offset,
              zoom: zoom
            })
          else
            # Fallback for testing
            packets_module.get_recent_packets_optimized(%{
              bounds: bounds,
              limit: batch_size,
              offset: offset
            })
          end
        rescue
          e ->
            require Logger

            Logger.error("Error loading historical packets: #{inspect(e)}")
            Logger.error("Stack trace: #{Exception.format_stacktrace()}")
            []
        end

      if Enum.any?(historical_packets) do
        # Process this batch and send to frontend
        packet_data_list =
          try do
            build_packet_data_list(historical_packets)
          rescue
            e ->
              require Logger

              Logger.error("Error building packet data list: #{inspect(e)}")
              []
          end

        if Enum.any?(packet_data_list) do
          # Check zoom level to decide between heat map and markers
          total_batches = socket.assigns.total_batches || 4
          is_final_batch = batch_offset >= total_batches - 1

          socket =
            if socket.assigns.map_zoom <= 8 do
              # For heat maps, store historical packets and update heat map when all batches are loaded
              # Add packets to historical_packets assign
              new_historical =
                Enum.reduce(historical_packets, socket.assigns.historical_packets, fn packet, acc ->
                  key = if Map.has_key?(packet, :id), do: to_string(packet.id), else: to_string(packet["id"])
                  Map.put(acc, key, packet)
                end)

              socket = assign(socket, historical_packets: new_historical)

              # If this is the final batch, update the heat map
              if is_final_batch do
                send_heat_map_for_current_bounds(socket)
              else
                socket
              end
            else
              # Use LiveView's efficient push_event for incremental updates
              push_event(socket, "add_historical_packets_batch", %{
                packets: packet_data_list,
                batch: batch_offset,
                is_final: is_final_batch
              })
            end

          # Update progress for user feedback
          socket = assign(socket, loading_batch: batch_offset + 1)

          socket
        else
          socket
        end
      else
        socket
      end
    else
      socket
    end
  end

  @spec within_bounds?(map() | struct(), map()) :: boolean()
  defp within_bounds?(packet, bounds) do
    {lat, lon, _data_extended} = MapHelpers.get_coordinates(packet)

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

  @impl true
  def terminate(_reason, socket) do
    if socket.assigns.buffer_timer, do: Process.cancel_timer(socket.assigns.buffer_timer)
    # Clean up any pending bounds update timer
    if socket.assigns[:bounds_update_timer] do
      Process.cancel_timer(socket.assigns.bounds_update_timer)
    end

    :ok
  end

  # Add a helper for robust bounds comparison

  defp compare_bounds(b1, b2) do
    # Handle nil bounds
    cond do
      is_nil(b1) and is_nil(b2) -> true
      is_nil(b1) or is_nil(b2) -> false
      true -> compare_bounds_maps(b1, b2)
    end
  end

  defp compare_bounds_maps(b1, b2) do
    round4 = fn x ->
      case x do
        n when is_float(n) -> Float.round(n, 4)
        n when is_integer(n) -> n * 1.0
        _ -> x
      end
    end

    Enum.all?([:north, :south, :east, :west], fn key ->
      round4.(Map.get(b1, key)) == round4.(Map.get(b2, key))
    end)
  end

  # --- Private bounds update helpers ---
  @spec handle_bounds_update(map(), Socket.t()) :: {:noreply, Socket.t()}
  defp handle_bounds_update(bounds, socket) do
    # Update the map bounds from the client, converting to atom keys
    map_bounds = MapHelpers.normalize_bounds(bounds)

    # Validate bounds to prevent invalid coordinates
    if valid_bounds?(map_bounds) do
      handle_valid_bounds_update(map_bounds, socket)
    else
      # Invalid bounds, skip update
      {:noreply, socket}
    end
  end

  defp valid_bounds?(map_bounds) do
    map_bounds.north <= 90 and map_bounds.south >= -90 and map_bounds.north > map_bounds.south
  end

  defp handle_valid_bounds_update(map_bounds, socket) do
    # Force processing if we need initial historical load, regardless of bounds comparison
    cond do
      socket.assigns[:needs_initial_historical_load] ->
        require Logger

        Logger.debug("Processing initial bounds update immediately (forced): #{inspect(map_bounds)}")
        socket = process_bounds_update(map_bounds, socket)
        {:noreply, socket}

      compare_bounds(map_bounds, socket.assigns.map_bounds) ->
        {:noreply, socket}

      true ->
        # For subsequent updates, use the timer to debounce
        schedule_bounds_update(map_bounds, socket)
    end
  end

  defp schedule_bounds_update(map_bounds, socket) do
    if socket.assigns[:bounds_update_timer] do
      Process.cancel_timer(socket.assigns.bounds_update_timer)
    end

    timer_ref = Process.send_after(self(), {:process_bounds_update, map_bounds}, 100)
    socket = assign(socket, bounds_update_timer: timer_ref, pending_bounds: map_bounds)
    {:noreply, socket}
  end

  defp send_heat_map_data(socket, filtered_packets) do
    # Convert map of packets to list
    packet_list = Map.values(filtered_packets)

    # Get clustering data
    case Clustering.cluster_packets(packet_list, socket.assigns.map_zoom) do
      {:heat_map, heat_points} ->
        push_event(socket, "show_heat_map", %{heat_points: heat_points})

      {:raw_packets, _packets} ->
        # Shouldn't happen at zoom <= 8, but handle it anyway
        socket
    end
  end

  defp send_heat_map_for_current_bounds(socket) do
    # Get all packets within current bounds
    all_packets =
      Map.values(socket.assigns.visible_packets) ++
        Map.values(socket.assigns.historical_packets)

    # Filter by bounds
    filtered_packets =
      all_packets
      |> Enum.filter(&within_bounds?(&1, socket.assigns.map_bounds))
      |> Enum.uniq_by(fn packet ->
        Map.get(packet, :id) || Map.get(packet, "id")
      end)

    # Get clustering data
    case Clustering.cluster_packets(filtered_packets, socket.assigns.map_zoom) do
      {:heat_map, heat_points} ->
        push_event(socket, "show_heat_map", %{heat_points: heat_points})

      {:raw_packets, _packets} ->
        socket
    end
  end

  defp trigger_marker_display(socket) do
    # Clear heat map and show markers
    socket = push_event(socket, "show_markers", %{})

    # Re-send all visible packets as markers
    locale = Map.get(socket.assigns, :locale, "en")

    visible_packets_list =
      socket.assigns.visible_packets
      |> Enum.map(fn {_callsign, packet} ->
        PacketUtils.build_packet_data(packet, true, locale)
      end)
      |> Enum.filter(& &1)

    socket =
      if Enum.any?(visible_packets_list) do
        push_event(socket, "add_markers", %{markers: visible_packets_list})
      else
        socket
      end

    # Trigger historical packet reload for markers
    start_progressive_historical_loading(socket)
  end

  @spec process_bounds_update(map(), Socket.t()) :: Socket.t()
  defp process_bounds_update(map_bounds, socket) do
    require Logger

    Logger.debug("process_bounds_update called with bounds: #{inspect(map_bounds)}")

    # Check if this is the initial load or if bounds have actually changed
    is_initial_load = socket.assigns[:needs_initial_historical_load] || !socket.assigns[:initial_bounds_loaded]
    bounds_changed = socket.assigns.map_bounds && not compare_bounds(map_bounds, socket.assigns.map_bounds)

    # Check if we've completed the initial historical load
    initial_historical_completed = socket.assigns[:initial_historical_completed] || false

    Logger.debug(
      "is_initial_load: #{is_initial_load}, bounds_changed: #{bounds_changed}, initial_historical_completed: #{initial_historical_completed}"
    )

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

    # Only clear historical packets if:
    # 1. Bounds actually changed AND
    # 2. This is not the initial load AND  
    # 3. We've already completed the initial historical load
    socket =
      if bounds_changed and not is_initial_load and initial_historical_completed do
        Logger.debug("Bounds changed after initial load - clearing historical packets")
        push_event(socket, "clear_historical_packets", %{})
      else
        Logger.debug("Initial load or no significant change - keeping existing markers")
        socket
      end

    # Always filter markers by bounds
    socket = push_event(socket, "filter_markers_by_bounds", %{bounds: map_bounds})

    # Update map bounds FIRST so progressive loading uses the correct bounds
    socket =
      socket
      |> assign(map_bounds: map_bounds, visible_packets: new_visible_packets)
      |> assign(needs_initial_historical_load: false)

    # Load historical packets for the new bounds (now socket.assigns.map_bounds is correct)
    Logger.debug("Starting progressive historical loading for new bounds")
    socket = start_progressive_historical_loading(socket)

    # Mark initial historical as completed if this was the initial load
    if is_initial_load do
      assign(socket, initial_historical_completed: true)
    else
      socket
    end
  end
end
