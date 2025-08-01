defmodule AprsmeWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsmeWeb, :live_view

  import AprsmeWeb.Components.ErrorBoundary
  import AprsmeWeb.Live.Shared.PacketUtils, only: [get_callsign_key: 1]
  import AprsmeWeb.TimeHelpers, only: [time_ago_in_words: 1]
  import Phoenix.LiveView, only: [connected?: 1, push_event: 3, push_patch: 2, put_flash: 3]

  alias Aprsme.Packets
  alias Aprsme.Packets.Clustering
  alias AprsmeWeb.Endpoint
  alias AprsmeWeb.Live.Shared.BoundsUtils
  alias AprsmeWeb.Live.Shared.CoordinateUtils
  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.Live.Shared.ParamUtils
  alias AprsmeWeb.MapLive.BoundsManager
  alias AprsmeWeb.MapLive.DataBuilder
  alias AprsmeWeb.MapLive.DisplayManager
  alias AprsmeWeb.MapLive.HistoricalLoader
  alias AprsmeWeb.MapLive.MapHelpers
  alias AprsmeWeb.MapLive.Navigation
  alias AprsmeWeb.MapLive.PacketBatcher
  alias AprsmeWeb.MapLive.PacketManager
  alias AprsmeWeb.MapLive.PacketProcessor
  alias AprsmeWeb.MapLive.RfPath
  alias AprsmeWeb.MapLive.UrlParams
  alias AprsmeWeb.TimeUtils
  alias Phoenix.LiveView.Socket
  alias Phoenix.Socket.Broadcast

  @impl true
  def mount(params, session, socket) do
    socket = setup_subscriptions(socket)

    # Basic setup
    deployed_at = Aprsme.Release.deployed_at()
    one_hour_ago = TimeUtils.one_day_ago()

    # Parse and determine map location
    {map_center, map_zoom, should_skip_initial_url_update} = Navigation.determine_map_location(params, session)

    # Parse trail duration and historical hours from URL
    trail_duration = params |> Map.get("trail", "1") |> parse_trail_duration() |> to_string()
    historical_hours = params |> Map.get("hist", "1") |> parse_historical_hours() |> to_string()

    # Calculate packet age threshold based on trail duration
    hours = String.to_integer(trail_duration)
    packet_age_threshold = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)

    # Setup defaults with parsed values
    socket = assign_defaults(socket, packet_age_threshold)

    socket =
      assign(socket,
        initial_historical_completed: false,
        trail_duration: trail_duration,
        historical_hours: historical_hours
      )

    # Setup additional subscriptions if connected
    socket = setup_additional_subscriptions(socket)

    # Handle callsign tracking - check path params first, then query params
    tracked_callsign =
      case Map.get(params, "callsign", Map.get(params, "call", "")) do
        "" -> ""
        nil -> ""
        callsign -> callsign |> String.trim() |> String.upcase()
      end

    {final_map_center, final_map_zoom} =
      Navigation.handle_callsign_tracking(
        tracked_callsign,
        map_center,
        map_zoom,
        UrlParams.has_explicit_url_params?(params)
      )

    # Calculate initial bounds
    initial_bounds = BoundsManager.calculate_bounds_from_center_and_zoom(final_map_center, final_map_zoom)

    # Final socket assignment
    {:ok,
     finalize_mount_assigns(socket, %{
       initial_bounds: initial_bounds,
       final_map_center: final_map_center,
       final_map_zoom: final_map_zoom,
       should_skip_initial_url_update: should_skip_initial_url_update,
       tracked_callsign: tracked_callsign,
       deployed_at: deployed_at,
       one_hour_ago: one_hour_ago
     })}
  end

  defp setup_subscriptions(socket) do
    do_setup_subscriptions(socket, connected?(socket))
  end

  defp do_setup_subscriptions(socket, true) do
    # Check if we should accept new connections
    if Application.get_env(:aprsme, :cluster_enabled, false) and
         not Aprsme.ConnectionMonitor.accepting_connections?() do
      # Redirect to another node or show message
      socket
      |> put_flash(:info, "This server is currently at capacity. Please try again in a moment.")
      |> push_event("redirect_to_least_loaded", %{})
      |> assign(:connection_draining, true)
    else
      # Generate a unique client ID for this LiveView instance
      client_id = "liveview_#{:erlang.phash2(self())}"

      # Register with connection monitor
      if Application.get_env(:aprsme, :cluster_enabled, false) do
        Aprsme.ConnectionMonitor.register_connection()
        # Subscribe to drain events for this node
        Phoenix.PubSub.subscribe(Aprsme.PubSub, "connection:drain:#{Node.self()}")
      end

      # Register with spatial PubSub (will get viewport later)
      # Start with a default viewport that will be updated when we get actual bounds
      default_bounds = %{north: 90.0, south: -90.0, east: 180.0, west: -180.0}
      {:ok, spatial_topic} = Aprsme.SpatialPubSub.register_viewport(client_id, default_bounds)

      # Subscribe to the spatial topic for this client
      Phoenix.PubSub.subscribe(Aprsme.PubSub, spatial_topic)

      # Still subscribe to bad packets (they don't have location)
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "bad_packets")

      # Subscribe to deployment events
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "deployment_events")

      # Subscribe to StreamingPacketsPubSub with initial bounds
      Aprsme.StreamingPacketsPubSub.subscribe_to_bounds(self(), default_bounds)

      Process.send_after(self(), :cleanup_old_packets, 60_000)
      # Schedule UI update timer to refresh "time ago" display every 30 seconds
      Process.send_after(self(), :update_time_display, 30_000)

      socket
      |> assign(:spatial_client_id, client_id)
      |> assign(:spatial_topic, spatial_topic)
      |> assign(:connection_draining, false)
    end
  end

  defp do_setup_subscriptions(socket, false), do: socket

  defp setup_additional_subscriptions(socket) do
    do_setup_additional_subscriptions(socket, connected?(socket))
  end

  defp do_setup_additional_subscriptions(socket, true) do
    Endpoint.subscribe("aprs_messages")
    Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")
    socket
  end

  defp do_setup_additional_subscriptions(socket, false), do: socket

  defp finalize_mount_assigns(socket, %{
         initial_bounds: initial_bounds,
         final_map_center: final_map_center,
         final_map_zoom: final_map_zoom,
         should_skip_initial_url_update: should_skip_initial_url_update,
         tracked_callsign: tracked_callsign,
         deployed_at: deployed_at,
         one_hour_ago: one_hour_ago
       }) do
    # Don't override trail_duration and historical_hours if they're already set
    trail_duration = Map.get(socket.assigns, :trail_duration, "1")
    historical_hours = Map.get(socket.assigns, :historical_hours, "1")
    packet_age_threshold = Map.get(socket.assigns, :packet_age_threshold, one_hour_ago)

    # If tracking a specific callsign, fetch their latest packet
    tracked_callsign_latest_packet =
      if tracked_callsign == "" do
        nil
      else
        try do
          Packets.get_latest_packet_for_callsign(tracked_callsign)
        rescue
          # Handle database connection errors gracefully (especially in tests)
          DBConnection.OwnershipError ->
            nil

          _ ->
            nil
        end
      end

    # Start packet batcher for efficient updates
    {:ok, batcher_pid} = PacketBatcher.start_link(self())

    assign(socket,
      map_ready: false,
      map_bounds: initial_bounds,
      map_center: final_map_center,
      map_zoom: final_map_zoom,
      should_skip_initial_url_update: should_skip_initial_url_update,
      packet_state: PacketManager.init_packet_state(),
      overlay_callsign: "",
      tracked_callsign: tracked_callsign,
      tracked_callsign_latest_packet: tracked_callsign_latest_packet,
      trail_duration: trail_duration,
      historical_hours: historical_hours,
      packet_age_threshold: packet_age_threshold,
      slideover_open: true,
      deployed_at: deployed_at,
      map_page: true,
      packet_buffer: [],
      buffer_timer: nil,
      batcher_pid: batcher_pid,
      station_popup_open: false,
      initial_bounds_loaded: false,
      # Always load historical data on initial page load
      needs_initial_historical_load: true,
      # Loading state management
      historical_loading: false,
      loading_generation: 0,
      pending_batch_tasks: []
    )
  end

  @spec assign_defaults(Socket.t(), DateTime.t()) :: Socket.t()
  defp assign_defaults(socket, one_hour_ago) do
    assign(socket,
      packets: [],
      all_packets: %{},
      visible_packets: %{},
      historical_packets: %{},
      page_title: "APRS Map",
      packet_state: PacketManager.init_packet_state(),
      station_popup_open: false,
      map_bounds: %{
        north: 49.0,
        south: 24.0,
        east: -66.0,
        west: -125.0
      },
      map_center: UrlParams.default_center(),
      map_zoom: UrlParams.default_zoom(),
      packet_age_threshold: one_hour_ago,
      map_ready: false,
      historical_loaded: false,
      bounds_update_timer: nil,
      pending_bounds: nil,
      initial_bounds_loaded: false,
      # Loading state management
      historical_loading: false,
      loading_generation: 0,
      pending_batch_tasks: [],
      # Overlay controls
      overlay_callsign: "",
      # Slideover state - will be set based on screen size
      slideover_open: true,
      # Track when last update occurred for real-time display in map sidebar
      # Updated when packets are processed or map bounds change
      last_update_at: DateTime.utc_now()
    )
  end

  # Handle both bounds_changed and update_bounds events
  @impl true
  def handle_event(event, %{"bounds" => bounds}, socket) when event in ["bounds_changed", "update_bounds"] do
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
    # Parse and validate coordinates
    lat_float = UrlParams.parse_latitude(lat)
    lng_float = UrlParams.parse_longitude(lng)

    # Additional validation - ensure we got valid coordinates
    if UrlParams.valid_coordinates?(lat_float, lng_float) do
      socket = Navigation.update_and_zoom_to_location(socket, lat_float, lng_float, 12)
      {:noreply, socket}
    else
      # Invalid coordinates - ignore the event
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("clear_and_reload_markers", _params, socket) do
    # Get current visible packets from PacketManager
    current_packets = PacketManager.get_visible_packets(socket.assigns.packet_state)

    # Filter by time and bounds
    filtered_packets =
      filter_packets_by_time_and_bounds_with_tracked(
        Map.new(current_packets, fn packet ->
          {get_callsign_key(packet), packet}
        end),
        socket.assigns.map_bounds,
        socket.assigns.packet_age_threshold,
        socket.assigns.tracked_callsign,
        socket.assigns.tracked_callsign_latest_packet
      )

    visible_packets_list = DataBuilder.build_packet_data_list_from_map(filtered_packets, false, socket)

    # Check zoom level to decide between heat map and markers
    socket =
      if socket.assigns.map_zoom <= 8 do
        # Use heat map for low zoom levels
        send_heat_map_data(socket, filtered_packets)
      else
        # Use regular markers for high zoom levels
        add_markers_if_any(socket, visible_packets_list)
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("map_ready", _params, socket) do
    require Logger

    # Mark map as ready - preserve existing needs_initial_historical_load state
    # (it's already set correctly in mount based on whether we're tracking a callsign)
    socket = assign(socket, map_ready: true)

    # If we have non-default center coordinates (e.g., from geolocation), apply them now
    default_center = UrlParams.default_center()

    socket =
      if socket.assigns.map_center.lat == default_center.lat and
           socket.assigns.map_center.lng == default_center.lng do
        socket
      else
        Navigation.zoom_to_current_location(socket)
      end

    # If we're tracking a callsign and have its latest packet, display it immediately
    socket =
      if socket.assigns.tracked_callsign != "" and socket.assigns.tracked_callsign_latest_packet do
        packet = socket.assigns.tracked_callsign_latest_packet
        packet_data = DataBuilder.build_packet_data(packet)

        push_event(socket, "add_historical_packets_batch", %{
          packets: [packet_data],
          batch: 0,
          is_final: false
        })
      else
        socket
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
  def handle_event("marker_hover_start", %{"id" => _id, "path" => path, "lat" => lat, "lng" => lng}, socket) do
    require Logger

    # Cancel any pending hover end timer
    socket =
      if socket.assigns[:hover_end_timer] do
        Process.cancel_timer(socket.assigns.hover_end_timer)
        assign(socket, hover_end_timer: nil)
      else
        socket
      end

    # Validate coordinates first
    lat_float = ParamUtils.safe_parse_coordinate(lat, 0.0, -90.0, 90.0)
    lng_float = ParamUtils.safe_parse_coordinate(lng, 0.0, -180.0, 180.0)

    # Validate path string
    safe_path = ParamUtils.sanitize_path_string(path)

    # Parse the path to find digipeater/igate stations
    path_stations = RfPath.parse_rf_path(safe_path)

    # Query for positions of path stations
    path_station_positions = RfPath.get_path_station_positions(path_stations, socket)

    # Send event to draw the RF path lines
    socket =
      if length(path_station_positions) > 0 do
        push_event(socket, "draw_rf_path", %{
          station_lat: lat_float,
          station_lng: lng_float,
          path_stations: path_station_positions
        })
      else
        socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("marker_hover_end", _params, socket) do
    # Debounce hover end to prevent flicker during rapid mouse movements
    timer = Process.send_after(self(), :clear_rf_path, 100)
    {:noreply, assign(socket, hover_end_timer: timer)}
  end

  @impl true
  def handle_event("update_callsign", %{"callsign" => callsign}, socket) do
    {:noreply, assign(socket, overlay_callsign: callsign)}
  end

  @impl true
  def handle_event("track_callsign", %{"callsign" => callsign}, socket) do
    normalized_callsign = String.upcase(String.trim(callsign))

    socket =
      if normalized_callsign == "" do
        # Clear tracking
        socket
        |> assign(tracked_callsign: "")
        |> update_url_with_current_state()
      else
        # Set tracking and navigate to callsign URL
        socket
        |> assign(tracked_callsign: normalized_callsign)
        |> push_patch(to: "/#{normalized_callsign}")
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("clear_tracking", _params, socket) do
    socket =
      socket
      |> assign(tracked_callsign: "", overlay_callsign: "")
      |> update_url_with_current_state()

    {:noreply, socket}
  end

  @impl true
  def handle_event("update_trail_duration", %{"trail_duration" => duration}, socket) do
    # Validate and convert duration string to hours
    hours = parse_trail_duration(duration)

    # Calculate new threshold safely
    new_threshold = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)

    socket = assign(socket, trail_duration: duration, packet_age_threshold: new_threshold)

    # Update client-side TrailManager with new duration
    socket = push_event(socket, "update_trail_duration", %{duration_hours: hours})

    # Update URL with new trail duration
    socket = update_url_with_current_state(socket)

    # Trigger cleanup to remove packets that are now outside the new duration
    send(self(), :cleanup_old_packets)

    {:noreply, socket}
  end

  @impl true
  def handle_event("update_historical_hours", %{"historical_hours" => hours}, socket) do
    # Validate hours value
    validated_hours = parse_historical_hours(hours)
    socket = assign(socket, historical_hours: to_string(validated_hours))

    # Update URL with new historical hours
    socket = update_url_with_current_state(socket)

    # Trigger a reload of historical packets with the new time range
    if socket.assigns.map_ready do
      send(self(), :reload_historical_packets)
    end

    {:noreply, socket}
  end

  @impl true
  def handle_event("search_callsign", %{"callsign" => callsign}, socket) do
    callsign
    |> String.trim()
    |> String.upcase()
    |> handle_callsign_search(socket)
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

    # Parse and validate coordinates
    {lat, lng} = parse_center_coordinates(center, socket)
    zoom = clamp_zoom(zoom)
    map_center = %{lat: lat, lng: lng}

    # Update map state
    socket = update_map_state(socket, map_center, zoom)

    # Handle URL updates
    socket = handle_url_update(socket, lat, lng, zoom)

    # Process bounds if included
    socket = process_bounds_from_params(socket, params)

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

  defp parse_center_coordinates(center, socket) when is_map(center) do
    CoordinateUtils.parse_center_coordinates(center, socket)
  end

  defp parse_center_coordinates(_, socket) do
    CoordinateUtils.parse_center_coordinates(nil, socket)
  end

  defp clamp_zoom(zoom) do
    ParamUtils.clamp_zoom(zoom)
  end

  defp update_map_state(socket, map_center, zoom) do
    old_zoom = socket.assigns.map_zoom
    crossing_threshold = crossing_zoom_threshold?(old_zoom, zoom)

    socket = assign(socket, map_center: map_center, map_zoom: zoom)

    if crossing_threshold do
      handle_zoom_threshold_crossing(socket, zoom)
    else
      socket
    end
  end

  defp crossing_zoom_threshold?(old_zoom, new_zoom) do
    DisplayManager.crossing_zoom_threshold?(old_zoom, new_zoom)
  end

  defp handle_zoom_threshold_crossing(socket, zoom) do
    DisplayManager.handle_zoom_threshold_crossing(socket, zoom)
  end

  defp update_url_with_current_state(socket) do
    lat = socket.assigns.map_center.lat
    lng = socket.assigns.map_center.lng
    zoom = socket.assigns.map_zoom
    handle_url_update(socket, lat, lng, zoom)
  end

  defp handle_url_update(socket, lat, lng, zoom) do
    if socket.assigns[:should_skip_initial_url_update] && !socket.assigns[:initial_bounds_loaded] do
      require Logger

      Logger.debug("Skipping URL update on initial load")
      assign(socket, should_skip_initial_url_update: false)
    else
      require Logger

      # Include trail duration and historical hours in URL
      trail_param =
        if socket.assigns[:trail_duration] && socket.assigns[:trail_duration] != "1",
          do: "&trail=#{socket.assigns[:trail_duration]}",
          else: ""

      hist_param =
        if socket.assigns[:historical_hours] && socket.assigns[:historical_hours] != "1",
          do: "&hist=#{socket.assigns[:historical_hours]}",
          else: ""

      # Preserve callsign in path if tracking
      base_path =
        if socket.assigns.tracked_callsign == "" do
          "/"
        else
          "/#{socket.assigns.tracked_callsign}"
        end

      new_path = "#{base_path}?lat=#{lat}&lng=#{lng}&z=#{zoom}#{trail_param}#{hist_param}"
      Logger.debug("Updating URL to: #{new_path}")
      push_patch(socket, to: new_path, replace: true)
    end
  end

  defp process_bounds_from_params(socket, params) do
    case Map.get(params, "bounds") do
      %{"north" => north, "south" => south, "east" => east, "west" => west} ->
        map_bounds = %{north: north, south: south, east: east, west: west}

        if should_process_bounds?(socket, map_bounds) do
          require Logger

          Logger.debug(
            "Sending bounds update (initial_load: #{!socket.assigns[:initial_bounds_loaded]}, " <>
              "needs_historical: #{socket.assigns[:needs_initial_historical_load]}): #{inspect(map_bounds)}"
          )

          send(self(), {:process_bounds_update, map_bounds})
        end

        socket

      _ ->
        socket
    end
  end

  defp should_process_bounds?(socket, new_bounds) do
    socket.assigns.map_bounds != new_bounds or
      !socket.assigns[:initial_bounds_loaded] or
      socket.assigns[:needs_initial_historical_load]
  end

  defp handle_callsign_search(callsign, socket) do
    Navigation.handle_callsign_search(callsign, socket)
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
      {map_center, map_zoom} = UrlParams.parse_map_params(params)

      # Parse trail duration and historical hours from URL
      trail_duration = params |> Map.get("trail", "1") |> parse_trail_duration() |> to_string()
      historical_hours = params |> Map.get("hist", "1") |> parse_historical_hours() |> to_string()

      # Update socket state
      socket =
        socket
        |> assign(map_center: map_center, map_zoom: map_zoom)
        |> assign(trail_duration: trail_duration, historical_hours: historical_hours)

      # Update packet age threshold based on trail duration
      hours = String.to_integer(trail_duration)
      new_threshold = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)
      socket = assign(socket, packet_age_threshold: new_threshold)

      # If map is ready, update the client-side map and trail duration
      socket =
        if socket.assigns.map_ready do
          socket
          |> push_event("zoom_to_location", %{
            lat: map_center.lat,
            lng: map_center.lng,
            zoom: map_zoom
          })
          |> push_event("update_trail_duration", %{duration_hours: hours})
        else
          socket
        end

      # Trigger cleanup and reload if settings changed
      if socket.assigns[:map_ready] do
        send(self(), :cleanup_old_packets)
        send(self(), :reload_historical_packets)
      end

      {:noreply, socket}
    end
  end

  # Parse trail duration with validation and bounds checking
  defp parse_trail_duration(duration), do: SharedPacketUtils.parse_trail_duration(duration)

  # Parse historical hours with validation
  defp parse_historical_hours(hours), do: SharedPacketUtils.parse_historical_hours(hours)

  @impl true
  def handle_info({:process_bounds_update, map_bounds}, socket),
    do: handle_info_process_bounds_update(map_bounds, socket)

  def handle_info(:initialize_replay, socket), do: handle_info_initialize_replay(socket)

  def handle_info(:cleanup_old_packets, socket), do: handle_cleanup_old_packets(socket)

  def handle_info(:update_time_display, socket), do: handle_update_time_display(socket)

  def handle_info(:reload_historical_packets, socket), do: handle_reload_historical_packets(socket)

  def handle_info({:postgres_packet, packet}, socket) do
    # Add packet to batcher instead of processing immediately
    if socket.assigns[:batcher_pid] do
      PacketBatcher.add_packet(socket.assigns.batcher_pid, packet)
    else
      handle_info_postgres_packet(packet, socket)
    end

    {:noreply, socket}
  end

  def handle_info({:spatial_packet, packet}, socket) do
    # Add packet to batcher instead of processing immediately
    if socket.assigns[:batcher_pid] do
      PacketBatcher.add_packet(socket.assigns.batcher_pid, packet)
    else
      handle_info_postgres_packet(packet, socket)
    end

    {:noreply, socket}
  end

  def handle_info({:streaming_packet, packet}, socket) do
    # Add packet to batcher instead of processing immediately
    if socket.assigns[:batcher_pid] do
      PacketBatcher.add_packet(socket.assigns.batcher_pid, packet)
    else
      handle_info_postgres_packet(packet, socket)
    end

    {:noreply, socket}
  end

  def handle_info({:packet_batch, packets}, socket) do
    # Process batch of packets efficiently
    socket =
      Enum.reduce(packets, socket, fn packet, acc_socket ->
        case handle_info_postgres_packet(packet, acc_socket) do
          {:noreply, new_socket} -> new_socket
          _ -> acc_socket
        end
      end)

    {:noreply, socket}
  end

  def handle_info(:clear_rf_path, socket) do
    # Clear the RF path lines with debouncing
    {:noreply, push_event(socket, "clear_rf_path", %{})}
  end

  def handle_info(
        %Broadcast{topic: "deployment_events", payload: {:new_deployment, %{deployed_at: deployed_at}}},
        socket
      ) do
    {:noreply, assign(socket, :deployed_at, deployed_at)}
  end

  def handle_info({:drain_connections, to_drain}, socket) do
    # Check if this connection should be drained
    # Use a random selection to determine if this connection should disconnect
    if :rand.uniform(100) <= to_drain * 10 do
      # Gracefully disconnect this client
      socket
      |> put_flash(:info, "Server load balancing in progress. Reconnecting...")
      |> push_event("reconnect", %{delay: :rand.uniform(5000)})
    else
      {:noreply, socket}
    end
  end

  def handle_info({:load_rf_path_station_packets, stations}, socket) do
    # Load the most recent packet for each RF path station
    station_packets =
      stations
      |> Enum.map(fn callsign ->
        Packets.get_latest_packet_for_callsign(callsign)
      end)
      |> Enum.filter(& &1)

    socket =
      if Enum.any?(station_packets) do
        # Build packet data for the RF path stations
        packet_data_list = DataBuilder.build_packet_data_list(station_packets)

        # Send these packets to the frontend
        DisplayManager.add_markers_if_any(socket, packet_data_list)
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_info({:process_pending_bounds}, socket) do
    if socket.assigns.pending_bounds && !socket.assigns.historical_loading do
      # Process the pending bounds update
      bounds = socket.assigns.pending_bounds
      socket = assign(socket, pending_bounds: nil)
      handle_bounds_update(bounds, socket)
    else
      {:noreply, socket}
    end
  end

  def handle_info({:load_historical_batch, batch_offset}, socket) do
    # For backward compatibility with old messages
    socket = HistoricalLoader.load_historical_batch(socket, batch_offset, socket.assigns.loading_generation)
    {:noreply, socket}
  end

  def handle_info({:load_historical_batch, batch_offset, generation}, socket) do
    # Only process if generation matches current loading generation
    if generation == socket.assigns.loading_generation do
      socket = HistoricalLoader.load_historical_batch(socket, batch_offset, generation)
      {:noreply, socket}
    else
      # Stale request, ignore it
      {:noreply, socket}
    end
  end

  def handle_info({:historical_loading_timeout, generation}, socket) do
    # Only process if generation matches current loading generation and we're still loading
    if generation == socket.assigns.loading_generation && socket.assigns.historical_loading do
      require Logger

      Logger.warning("Historical loading timeout reached, forcing completion")

      socket =
        socket
        |> assign(:historical_loading, false)
        |> assign(:loading_batch, socket.assigns.total_batches || 1)

      {:noreply, socket}
    else
      # Stale timeout or already completed, ignore it
      {:noreply, socket}
    end
  end

  def handle_info(%Broadcast{topic: "aprs_messages", event: "packet", payload: packet}, socket),
    do: handle_info({:postgres_packet, packet}, socket)

  def handle_info({:show_error, message}, socket) do
    socket = put_flash(socket, :error, message)
    {:noreply, socket}
  end

  # Private handler functions for each message type

  defp handle_info_process_bounds_update(map_bounds, socket) do
    require Logger

    # Check if this is a stale update (newer bounds have been scheduled)
    if socket.assigns[:pending_bounds] && socket.assigns.pending_bounds != map_bounds do
      Logger.debug("Skipping stale bounds update, newer bounds pending")
      {:noreply, socket}
    else
      # Clear the timer reference since we're processing this update
      socket = assign(socket, bounds_update_timer: nil, pending_bounds: nil)

      # Check if we need to process this bounds update
      should_process =
        !socket.assigns[:initial_bounds_loaded] or
          socket.assigns[:needs_initial_historical_load] or
          not BoundsUtils.compare_bounds(map_bounds, socket.assigns.map_bounds)

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
  end

  defp handle_info_initialize_replay(socket) do
    if not socket.assigns.historical_loaded and socket.assigns.map_ready do
      # Only proceed if we have actual map bounds - don't use world bounds
      if socket.assigns.map_bounds do
        # Use progressive loading for better performance
        socket = HistoricalLoader.start_progressive_historical_loading(socket)
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
    # Check if we're tracking a specific callsign
    # Only process if this packet is from the tracked callsign
    if socket.assigns.tracked_callsign == "" do
      # No tracking - show all packets
      process_packet_for_display(packet, socket)
    else
      packet_sender = Map.get(packet, :sender, Map.get(packet, "sender", ""))

      if String.upcase(packet_sender) == String.upcase(socket.assigns.tracked_callsign) do
        # Update the tracked callsign's latest packet
        socket = assign(socket, :tracked_callsign_latest_packet, packet)
        process_packet_for_display(packet, socket)
      else
        {:noreply, socket}
      end
    end
  end

  defp process_packet_for_display(packet, socket) do
    PacketProcessor.process_packet_for_display(packet, socket)
  end

  # Handle replaying the next historical packet

  @impl true
  def render(assigns) do
    ~H"""
    <%!-- All vendor libraries are now loaded from vendor.js bundle --%>

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

      /* Slideover panel base styles */
      .slideover-panel {
        position: fixed;
        top: 0;
        right: 0;
        bottom: 0;
        width: 352px;
        background: white;
        box-shadow: -2px 0 8px rgba(0, 0, 0, 0.1);
        z-index: 50;
        display: flex;
        flex-direction: column;
        transition: transform 0.3s ease-in-out;
        overflow: hidden;
      }

      .slideover-panel.slideover-open {
        transform: translateX(0);
      }

      .slideover-panel.slideover-closed {
        transform: translateX(100%);
      }

      /* Mobile styles */
      @media (max-width: 1023px) {
        #aprs-map {
          right: 0 !important;
        }
        
        .slideover-panel {
          width: 90vw !important;
          max-width: 400px;
        }
        
        .locate-button {
          top: 65px;
          width: 44px;
          height: 44px;
          display: flex;
          align-items: center;
          justify-content: center;
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

      @media (prefers-color-scheme: dark) {
        .locate-button {
          background: rgb(30 41 59); /* slate-800 */
          border-color: rgba(255, 255, 255, 0.1);
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
        }
        
        .locate-button svg {
          fill: #e2e8f0; /* slate-200 */
        }
      }

      .locate-button:hover {
        background: #f4f4f4;
      }

      @media (prefers-color-scheme: dark) {
        .locate-button:hover {
          background: rgb(51 65 85); /* slate-700 */
        }
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

      /* Slideover panel styles */
      .slideover-panel {
        position: fixed;
        top: 0;
        right: 0;
        height: 100vh;
        background: white;
        box-shadow: -4px 0 16px rgba(0, 0, 0, 0.1);
        z-index: 1000;
        display: flex;
        flex-direction: column;
        transition: transform 0.3s ease-in-out;
        overflow: hidden;
        box-sizing: border-box;
      }

      @media (prefers-color-scheme: dark) {
        .slideover-panel {
          background: rgb(15 23 42); /* slate-900 */
          box-shadow: -4px 0 16px rgba(0, 0, 0, 0.3);
        }
      }

      /* Ensure proper box-sizing for all children */
      .slideover-panel * {
        box-sizing: border-box;
      }

      /* Desktop styles */
      @media (min-width: 1024px) {
        .slideover-panel {
          width: 352px;
          transform: translateX(0);
        }

        .slideover-panel.slideover-closed {
          transform: translateX(100%);
        }
      }

      /* Mobile styles - override the desktop styles */
      @media (max-width: 1023px) {
        .slideover-panel {
          width: 90vw !important;
          max-width: 400px;
        }

        .slideover-panel.slideover-closed {
          transform: translateX(100%);
        }

        .slideover-panel.slideover-open {
          transform: translateX(0);
        }
      }

      /* Slideover toggle button */
      .slideover-toggle {
        position: fixed;
        top: 50%;
        transform: translateY(-50%);
        z-index: 999;
        background: white;
        border: 2px solid rgba(0, 0, 0, 0.1);
        border-radius: 8px 0 0 8px;
        padding: 12px 8px;
        cursor: pointer;
        transition: all 0.3s ease-in-out;
        box-shadow: -2px 0 8px rgba(0, 0, 0, 0.1);
      }

      @media (prefers-color-scheme: dark) {
        .slideover-toggle {
          background: rgb(30 41 59); /* slate-800 */
          border-color: rgba(255, 255, 255, 0.1);
          box-shadow: -2px 0 8px rgba(0, 0, 0, 0.3);
        }
      }

      .slideover-toggle.slideover-open {
        right: 352px;
      }

      .slideover-toggle.slideover-closed {
        right: 0;
        border-right: none;
      }

      @media (max-width: 1023px) {
        .slideover-toggle {
          display: none;
        }
      }

      .slideover-toggle:hover {
        background: #f3f4f6;
      }

      @media (prefers-color-scheme: dark) {
        .slideover-toggle:hover {
          background: rgb(51 65 85); /* slate-700 */
        }
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
        role="application"
        aria-label={gettext("APRS packet map showing real-time amateur radio stations")}
      >
      </div>
    </.error_boundary>

    <button class="locate-button" phx-click="locate_me" title={Gettext.gettext(AprsmeWeb.Gettext, "Find my location")}>
      <svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="#374151" stroke="none">
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

    <!-- Loading Indicator -->
    <%= if @historical_loading do %>
      <div class="absolute bottom-4 left-1/2 transform -translate-x-1/2 z-[1000]
                  bg-white rounded-lg shadow-lg px-4 py-2 flex items-center space-x-2">
        <div class="animate-spin rounded-full h-4 w-4 border-b-2 border-indigo-600"></div>
        <span class="text-sm text-gray-600">
          {gettext("Loading historical data...")}
          <%= if @loading_batch && @total_batches do %>
            ({@loading_batch}/{@total_batches})
          <% end %>
        </span>
      </div>
    <% end %>

    <!-- Mobile Backdrop -->
    <%= if @slideover_open do %>
      <div class="fixed inset-0 bg-black bg-opacity-50 z-[999] lg:hidden backdrop-blur-sm" phx-click="toggle_slideover">
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
      <div class="flex items-center justify-between p-6 border-b border-slate-200 dark:border-slate-700 bg-gradient-to-r from-indigo-600 to-purple-600 text-white">
        <div class="flex items-center space-x-2">
          <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
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
      <div class="p-6 space-y-6 bg-slate-50 dark:bg-slate-900 flex-1 overflow-y-auto">
        <!-- Callsign Search -->
        <div class="space-y-4">
          <label class="block text-sm font-semibold text-slate-700 dark:text-slate-300 flex items-center space-x-2">
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
          <form phx-submit="track_callsign" class="flex flex-col space-y-2">
            <div class="flex space-x-2 w-full">
              <input
                type="text"
                name="callsign"
                value={@tracked_callsign || @overlay_callsign}
                phx-change="update_callsign"
                placeholder={gettext("Enter callsign...")}
                class="flex-1 px-4 py-3 border border-slate-300 dark:border-slate-600 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm uppercase placeholder-slate-400 dark:placeholder-slate-500 transition-all duration-200 hover:border-slate-400 dark:hover:border-slate-500 bg-white dark:bg-slate-800 text-gray-900 dark:text-white"
              />
              <button
                type="submit"
                class="px-4 py-3 bg-gradient-to-r from-indigo-600 to-purple-600 text-white rounded-xl hover:from-indigo-700 hover:to-purple-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 transition-all duration-200 text-sm font-semibold shadow-lg hover:shadow-xl whitespace-nowrap flex-shrink-0"
              >
                {gettext("Track")}
              </button>
            </div>
            <%= if @tracked_callsign != "" do %>
              <div class="flex items-center justify-between bg-blue-50 px-3 py-2 rounded-lg">
                <span class="text-sm text-blue-700 font-medium">
                  {gettext("Tracking:")} <span class="font-bold">{@tracked_callsign}</span>
                </span>
                <button
                  type="button"
                  phx-click="clear_tracking"
                  class="text-blue-600 hover:text-blue-800 font-medium text-sm"
                >
                  {gettext("Clear")}
                </button>
              </div>
            <% end %>
          </form>
        </div>
        
    <!-- Trail Duration -->
        <div class="space-y-4">
          <label class="block text-sm font-semibold text-slate-700 dark:text-slate-300 flex items-center space-x-2">
            <svg class="w-4 h-4 text-emerald-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
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
              class="w-full px-4 py-3 border border-slate-300 dark:border-slate-600 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm bg-white dark:bg-slate-800 text-gray-900 dark:text-white appearance-none transition-all duration-200 hover:border-slate-400 dark:hover:border-slate-500"
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
              <svg class="w-4 h-4 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
              </svg>
            </div>
          </form>
        </div>
        
    <!-- Historical Data -->
        <div class="space-y-4">
          <label class="block text-sm font-semibold text-slate-700 dark:text-slate-300 flex items-center space-x-2">
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
              class="w-full px-4 py-3 border border-slate-300 dark:border-slate-600 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm bg-white dark:bg-slate-800 text-gray-900 dark:text-white appearance-none transition-all duration-200 hover:border-slate-400 dark:hover:border-slate-500"
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
              <svg class="w-4 h-4 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
              </svg>
            </div>
          </form>
        </div>
        
    <!-- Navigation -->
        <div class="pt-4 border-t border-slate-200 dark:border-slate-700 space-y-3">
          <div class="flex items-center space-x-2 text-sm text-slate-600 dark:text-slate-400 mb-3">
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
          <.navigation
            variant={:vertical}
            class="text-sm"
            current_user={@current_user}
            map_state={%{lat: @map_center.lat, lng: @map_center.lng, zoom: @map_zoom}}
            tracked_callsign={@tracked_callsign}
          />
        </div>
        
    <!-- Last Update -->
        <div class="pt-4 border-t border-slate-200 dark:border-slate-700 space-y-3">
          <div class="flex items-center space-x-2 text-sm text-slate-600 dark:text-slate-400">
            <svg class="w-4 h-4 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"
              />
            </svg>
            <span class="font-medium">{gettext("Last Update")}</span>
          </div>
          <div class="text-xs text-slate-500 dark:text-slate-400">
            <%= if @last_update_at do %>
              <div class="font-mono">{time_ago_in_words(@last_update_at)}</div>
              <div class="font-mono text-slate-400 dark:text-slate-500">
                {Calendar.strftime(@last_update_at, "%Y-%m-%d %H:%M UTC")}
              </div>
            <% else %>
              <div class="font-mono text-slate-500 dark:text-slate-400">No updates yet</div>
            <% end %>
          </div>
        </div>
        
    <!-- Deployment Information -->
        <div class="pt-4 border-t border-slate-200 dark:border-slate-700 space-y-3">
          <div class="flex items-center space-x-2 text-sm text-slate-600 dark:text-slate-400">
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
          <div class="text-xs text-slate-500 dark:text-slate-400">
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

    # Remove expired packets using PacketManager predicate
    updated_packet_state =
      PacketManager.remove_packets_where(socket.assigns.packet_state, fn packet ->
        SharedPacketUtils.packet_within_time_threshold?(packet, threshold)
      end)

    # Get expired packet IDs that need to be removed from client
    current_packets = PacketManager.get_visible_packets(socket.assigns.packet_state)
    remaining_packets = PacketManager.get_visible_packets(updated_packet_state)

    expired_keys =
      current_packets
      |> Enum.reject(fn current_packet ->
        Enum.any?(remaining_packets, fn remaining_packet ->
          get_callsign_key(current_packet) == get_callsign_key(remaining_packet)
        end)
      end)
      |> Enum.map(&get_callsign_key/1)

    # Only update the client if there are expired markers
    socket = DisplayManager.remove_markers_batch(socket, expired_keys)

    {:noreply, assign(socket, packet_state: updated_packet_state)}
  end

  defp handle_update_time_display(socket) do
    # Schedule next update
    Process.send_after(self(), :update_time_display, 30_000)

    # Simply triggering a re-render will cause time_ago_in_words to recalculate
    # No need to update any assigns, just return the socket
    {:noreply, socket}
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
      socket = HistoricalLoader.start_progressive_historical_loading(socket)

      {:noreply, socket}
    else
      Logger.debug("Skipping historical reload - conditions not met")
      {:noreply, socket}
    end
  end

  # Helper functions

  # Fetch historical packets from the database

  # Select the best packet to display for a callsign - prioritize position over weather

  @spec within_bounds?(map() | struct(), map()) :: boolean()
  defp within_bounds?(packet, bounds) do
    {lat, lon, _data_extended} = MapHelpers.get_coordinates(packet)

    # Basic validation
    check_coordinate_bounds(lat, lon, bounds)
  end

  defp check_coordinate_bounds(nil, _, _), do: false
  defp check_coordinate_bounds(_, nil, _), do: false

  defp check_coordinate_bounds(lat, lon, bounds) do
    # Check latitude bounds (straightforward)
    lat_in_bounds = lat >= bounds.south && lat <= bounds.north

    # Check longitude bounds (handle potential wrapping)
    lng_in_bounds = check_longitude_bounds(lon, bounds.west, bounds.east)

    lat_in_bounds && lng_in_bounds
  end

  defp check_longitude_bounds(lon, west, east) when west <= east do
    # Normal case: bounds don't cross antimeridian
    lon >= west && lon <= east
  end

  defp check_longitude_bounds(lon, west, east) do
    # Bounds cross antimeridian (e.g., west=170, east=-170)
    lon >= west || lon <= east
  end

  # Helper functions to reduce duplicate filtering logic

  @spec filter_packets_by_bounds(map(), map()) :: map()
  defp filter_packets_by_bounds(packets_map, bounds) when is_map(packets_map) do
    packets_map
    |> Enum.filter(fn {_k, packet} -> within_bounds?(packet, bounds) end)
    |> Map.new()
  end

  @spec filter_packets_by_bounds(list(), map()) :: list()
  defp filter_packets_by_bounds(packets_list, bounds) when is_list(packets_list) do
    Enum.filter(packets_list, &within_bounds?(&1, bounds))
  end

  @spec reject_packets_by_bounds(map(), map()) :: list()
  defp reject_packets_by_bounds(packets_map, bounds) when is_map(packets_map) do
    packets_map
    |> Enum.reject(fn {_k, packet} -> within_bounds?(packet, bounds) end)
    |> Enum.map(fn {k, _} -> k end)
  end

  @spec filter_packets_by_time_and_bounds(map(), map(), DateTime.t()) :: map()
  defp filter_packets_by_time_and_bounds(packets, bounds, time_threshold) do
    packets
    |> Enum.filter(fn {_callsign, packet} ->
      within_bounds?(packet, bounds) &&
        SharedPacketUtils.packet_within_time_threshold?(packet, time_threshold)
    end)
    |> Map.new()
  end

  @spec filter_packets_by_time_and_bounds_with_tracked(map(), map(), DateTime.t(), String.t(), map() | nil) :: map()
  defp filter_packets_by_time_and_bounds_with_tracked(
         packets,
         bounds,
         time_threshold,
         tracked_callsign,
         tracked_latest_packet
       ) do
    filtered = filter_packets_by_time_and_bounds(packets, bounds, time_threshold)

    # Always include the tracked callsign's latest packet if we have one
    if tracked_callsign != "" and tracked_latest_packet do
      key = get_callsign_key(tracked_latest_packet)
      Map.put(filtered, key, tracked_latest_packet)
    else
      filtered
    end
  end

  # Helper functions for marker operations

  @spec remove_markers_batch(Socket.t(), list()) :: Socket.t()
  defp remove_markers_batch(socket, []), do: socket

  defp remove_markers_batch(socket, marker_ids) do
    Enum.reduce(marker_ids, socket, fn id, acc ->
      push_event(acc, "remove_marker", %{id: id})
    end)
  end

  @spec add_markers_if_any(Socket.t(), list()) :: Socket.t()
  defp add_markers_if_any(socket, []), do: socket

  defp add_markers_if_any(socket, markers) do
    push_event(socket, "add_markers", %{markers: markers})
  end

  @impl true
  def terminate(_reason, socket) do
    # Unregister from connection monitor
    if Application.get_env(:aprsme, :cluster_enabled, false) and
         not socket.assigns[:connection_draining] do
      Aprsme.ConnectionMonitor.unregister_connection()
    end

    # Cleanup spatial registration if we have a client ID
    if socket.assigns[:spatial_client_id] do
      Aprsme.SpatialPubSub.unregister_client(socket.assigns.spatial_client_id)
    end

    # Unsubscribe from StreamingPacketsPubSub
    Aprsme.StreamingPacketsPubSub.unsubscribe(self())

    if socket.assigns.buffer_timer, do: Process.cancel_timer(socket.assigns.buffer_timer)
    # Clean up any pending bounds update timer
    if socket.assigns[:bounds_update_timer] do
      Process.cancel_timer(socket.assigns.bounds_update_timer)
    end

    # Clean up any pending batch tasks
    if socket.assigns[:pending_batch_tasks] do
      Enum.each(socket.assigns.pending_batch_tasks, &Process.cancel_timer/1)
    end

    :ok
  end

  # Bounds comparison now handled by shared utilities

  # --- Private bounds update helpers ---
  @spec handle_bounds_update(map(), Socket.t()) :: {:noreply, Socket.t()}
  defp handle_bounds_update(bounds, socket) do
    # Skip if we're currently loading historical data
    if socket.assigns.historical_loading do
      # Defer the bounds update
      {:noreply, assign(socket, pending_bounds: bounds)}
    else
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

      BoundsUtils.compare_bounds(map_bounds, socket.assigns.map_bounds) ->
        {:noreply, socket}

      true ->
        # For subsequent updates, use the timer to debounce
        schedule_bounds_update(map_bounds, socket)
    end
  end

  # Configurable debounce delay (in milliseconds)
  @bounds_update_debounce_ms Application.compile_env(:aprsme, :bounds_update_debounce_ms, 400)

  defp schedule_bounds_update(map_bounds, socket) do
    # Cancel existing timer if present
    if socket.assigns[:bounds_update_timer] do
      case Process.cancel_timer(socket.assigns.bounds_update_timer) do
        false ->
          # Timer already fired, receive the message to clear it
          receive do
            {:process_bounds_update, _} -> :ok
          after
            0 -> :ok
          end

        _ ->
          :ok
      end
    end

    # Cancel any in-progress historical loading to prevent race conditions
    socket = HistoricalLoader.cancel_pending_loads(socket)

    # Use configurable debounce time for better stability during rapid zooming
    timer_ref = Process.send_after(self(), {:process_bounds_update, map_bounds}, @bounds_update_debounce_ms)
    socket = assign(socket, bounds_update_timer: timer_ref, pending_bounds: map_bounds)
    {:noreply, socket}
  end

  defp send_heat_map_data(socket, filtered_packets) do
    # Convert map of packets to list
    packet_list = Map.values(filtered_packets)
    send_heat_map_for_packets(socket, packet_list)
  end

  # Common heat map display logic
  defp send_heat_map_for_packets(socket, packets) do
    # Get clustering data
    case Clustering.cluster_packets(packets, socket.assigns.map_zoom) do
      {:heat_map, heat_points} ->
        push_event(socket, "show_heat_map", %{heat_points: heat_points})

      {:raw_packets, _packets} ->
        # Shouldn't happen at zoom <= 8, but handle it anyway
        socket
    end
  end

  @spec process_bounds_update(map(), Socket.t()) :: Socket.t()
  defp process_bounds_update(map_bounds, socket) do
    require Logger

    Logger.debug("process_bounds_update called with bounds: #{inspect(map_bounds)}")

    # Update spatial viewport if we have a client ID
    if socket.assigns[:spatial_client_id] do
      Aprsme.SpatialPubSub.update_viewport(socket.assigns.spatial_client_id, map_bounds)
    end

    # Update StreamingPacketsPubSub subscription with new bounds
    Aprsme.StreamingPacketsPubSub.subscribe_to_bounds(self(), map_bounds)

    # Check if this is the initial load or if bounds have actually changed
    is_initial_load = socket.assigns[:needs_initial_historical_load] || !socket.assigns[:initial_bounds_loaded]
    bounds_changed = socket.assigns.map_bounds && not BoundsUtils.compare_bounds(map_bounds, socket.assigns.map_bounds)

    # Check if we've completed the initial historical load
    initial_historical_completed = socket.assigns[:initial_historical_completed] || false

    Logger.debug(
      "is_initial_load: #{is_initial_load}, bounds_changed: #{bounds_changed}, initial_historical_completed: #{initial_historical_completed}"
    )

    # Remove out-of-bounds packets and markers immediately
    current_packets = PacketManager.get_visible_packets(socket.assigns.packet_state)
    current_packets_map = Map.new(current_packets, fn packet -> {get_callsign_key(packet), packet} end)

    new_visible_packets = filter_packets_by_bounds(current_packets_map, map_bounds)
    packets_to_remove = reject_packets_by_bounds(current_packets_map, map_bounds)

    # Remove markers for out-of-bounds packets
    socket = remove_markers_batch(socket, packets_to_remove)

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

    # Update packet state to keep only packets within bounds
    filtered_packets = Map.values(new_visible_packets)
    {updated_packet_state, _} = PacketManager.add_visible_packets(socket.assigns.packet_state, filtered_packets)

    # Update map bounds FIRST so progressive loading uses the correct bounds
    socket =
      socket
      |> assign(map_bounds: map_bounds, packet_state: updated_packet_state)
      |> assign(needs_initial_historical_load: false)

    # Load historical packets for the new bounds (now socket.assigns.map_bounds is correct)
    Logger.debug("Starting progressive historical loading for new bounds")
    socket = HistoricalLoader.start_progressive_historical_loading(socket)

    # Mark initial historical as completed if this was the initial load
    if is_initial_load do
      assign(socket, initial_historical_completed: true)
    else
      socket
    end
  end
end
