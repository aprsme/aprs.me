defmodule AprsmeWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsmeWeb, :live_view

  import AprsmeWeb.TimeHelpers, only: [time_ago_in_words: 1]
  import Phoenix.LiveView, only: [connected?: 1, push_event: 3, push_navigate: 2]

  alias AprsmeWeb.Endpoint
  alias AprsmeWeb.MapLive.MapHelpers
  alias AprsmeWeb.MapLive.PacketUtils
  alias Phoenix.LiveView.Socket

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to packet updates
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "packets")
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "bad_packets")

      # Schedule periodic cleanup of old packets
      Process.send_after(self(), :cleanup_old_packets, 60_000)
    end

    # Get deployment timestamp from config (set during application startup)
    deployed_at = Aprsme.Release.deployed_at()

    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    socket = assign_defaults(socket, one_hour_ago)
    socket = assign(socket, packet_buffer: [], buffer_timer: nil)
    socket = assign(socket, all_packets: %{}, station_popup_open: false)

    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")
      # Defer IP geolocation to avoid blocking initial page load
      Process.send_after(self(), :start_geolocation, 2000)
    end

    {:ok,
     assign(socket,
       map_ready: false,
       map_bounds: nil,
       map_center: %{lat: 39.8283, lng: -98.5795},
       map_zoom: 4,
       visible_packets: %{},
       historical_packets: %{},
       overlay_callsign: "",
       trail_duration: "1",
       historical_hours: "1",
       packet_age_threshold: one_hour_ago,
       slideover_open: true,
       deployed_at: deployed_at,
       map_page: true
     )}
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
      pending_geolocation: nil,
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

    # Load historical packets with a small delay to ensure map is fully ready
    Process.send_after(self(), :reload_historical_packets, 100)

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
  def handle_info({:process_bounds_update, map_bounds}, socket), do: handle_info_process_bounds_update(map_bounds, socket)

  def handle_info({:delayed_zoom, %{lat: lat, lng: lng}}, socket), do: handle_info_delayed_zoom(lat, lng, socket)

  def handle_info({:ip_location, %{lat: lat, lng: lng}}, socket), do: handle_info_ip_location(lat, lng, socket)

  def handle_info(:initialize_replay, socket), do: handle_info_initialize_replay(socket)

  def handle_info(:cleanup_old_packets, socket), do: handle_cleanup_old_packets(socket)

  def handle_info(:reload_historical_packets, socket), do: handle_reload_historical_packets(socket)

  def handle_info({:postgres_packet, packet}, socket), do: handle_info_postgres_packet(packet, socket)

  def handle_info(:start_geolocation, socket), do: handle_info_start_geolocation(socket)

  def handle_info(%Phoenix.Socket.Broadcast{topic: "aprs_messages", event: "packet", payload: packet}, socket),
    do: handle_info({:postgres_packet, packet}, socket)

  # Private handler functions for each message type

  defp handle_info_process_bounds_update(map_bounds, socket) do
    if !socket.assigns.initial_bounds_loaded or
         not compare_bounds(map_bounds, socket.assigns.map_bounds) do
      socket = process_bounds_update(map_bounds, socket)
      socket = assign(socket, initial_bounds_loaded: true)
      {:noreply, socket}
    else
      {:noreply, socket}
    end
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

    # Schedule a delayed zoom to give the user a moment to see the map
    Process.send_after(self(), {:delayed_zoom, %{lat: lat_float, lng: lng_float}}, 500)

    # We can still optimistically set the center and zoom
    socket = assign(socket, map_center: %{lat: lat_float, lng: lng_float}, map_zoom: 12)
    {:noreply, socket}
  end

  defp handle_info_initialize_replay(socket) do
    if not socket.assigns.historical_loaded and socket.assigns.map_ready do
      # Use default bounds if map_bounds is not available yet
      map_bounds =
        socket.assigns.map_bounds ||
          %{
            north: 90.0,
            south: -90.0,
            east: 180.0,
            west: -180.0
          }

      socket = assign(socket, map_bounds: map_bounds)

      # Use optimized query for initial load
      socket = load_historical_packets_for_bounds_optimized(socket, map_bounds)
      {:noreply, socket}
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

    # Live packets are always the most recent for their callsign
    locale = Map.get(socket.assigns, :locale, "en")
    marker_data = PacketUtils.build_packet_data(packet, true, locale)

    socket =
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

    {:noreply, assign(socket, visible_packets: new_visible_packets)}
  end

  defp handle_info_start_geolocation(socket) do
    if geolocation_enabled?() do
      ip_for_geolocation =
        if Application.get_env(:aprsme, AprsmeWeb.Endpoint)[:code_reloader] do
          # For testing geolocation in dev environment, use a public IP address.
          # This will be geolocated to Mountain View, CA.
          "8.8.8.8"
        else
          extract_ip(socket)
        end

      if valid_ip_for_geolocation?(ip_for_geolocation) do
        start_geolocation_task(ip_for_geolocation)
      end
    end

    {:noreply, socket}
  end

  defp geolocation_enabled? do
    Application.get_env(:aprsme, :disable_aprs_connection, false) != true
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

    <div
      id="aprs-map"
      class={if @slideover_open, do: "slideover-open", else: "slideover-closed"}
      phx-hook="APRSMap"
      phx-update="ignore"
      data-center={Jason.encode!(@map_center)}
      data-zoom={@map_zoom}
    >
    </div>

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
    if socket.assigns.map_ready and socket.assigns.map_bounds do
      # Clear existing historical packets
      socket = push_event(socket, "clear_historical_packets", %{})

      # Use optimized loading for better performance
      socket = load_historical_packets_for_bounds_optimized(socket, socket.assigns.map_bounds)

      {:noreply, socket}
    else
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
  defp process_historical_packets(socket, historical_packets) do
    socket = push_event(socket, "clear_historical_packets", %{})

    packet_data_list = build_packet_data_list(historical_packets)

    if Enum.any?(packet_data_list) do
      push_event(socket, "add_historical_packets", %{packets: packet_data_list})
    else
      socket
    end
  end

  defp build_packet_data_list(historical_packets) do
    historical_packets
    |> Enum.group_by(&PacketUtils.generate_callsign/1)
    |> Enum.flat_map(&process_callsign_packets/1)
  end

  defp process_callsign_packets({callsign, packets}) do
    sorted_packets = sort_packets_by_inserted_at(packets)
    unique_position_packets = filter_unique_positions(sorted_packets)

    unique_position_packets
    |> Enum.with_index()
    |> Enum.map(&build_packet_data_with_index(&1, callsign))
    |> Enum.filter(& &1)
  end

  defp sort_packets_by_inserted_at(packets) do
    Enum.sort_by(
      packets,
      fn packet ->
        case packet.inserted_at do
          %NaiveDateTime{} = naive_dt -> DateTime.from_naive!(naive_dt, "Etc/UTC")
          %DateTime{} = dt -> dt
          _other -> DateTime.utc_now()
        end
      end,
      {:desc, DateTime}
    )
  end

  defp build_packet_data_with_index({packet, index}, callsign) do
    # The first packet (index 0) is the most recent for this callsign
    # Only show as red dot if it's not the most recent position
    is_most_recent = index == 0

    # Note: We don't have access to locale here, so we'll use default "en"
    packet_data = PacketUtils.build_packet_data(packet, is_most_recent, "en")

    if packet_data do
      packet_data
      |> Map.put(:callsign, callsign)
      |> Map.put(:historical, true)
    end
  end

  defp filter_unique_positions(packets) do
    packets
    |> Enum.reduce([], fn packet, acc ->
      add_if_unique_position(packet, acc)
    end)
    |> Enum.reverse()
  end

  defp add_if_unique_position(packet, []), do: if_position_present(packet, [])
  defp add_if_unique_position(packet, [last_packet | _] = acc), do: if_position_changed(packet, last_packet, acc)

  defp if_position_present(packet, acc) do
    {lat, lon, _} = MapHelpers.get_coordinates(packet)
    if lat && lon, do: [packet | acc], else: acc
  end

  defp if_position_changed(packet, last_packet, acc) do
    {lat, lon, _} = MapHelpers.get_coordinates(packet)

    if lat && lon do
      if position_changed?(packet, last_packet), do: [packet | acc], else: acc
    else
      acc
    end
  end

  # Check if position changed significantly between two packets (more than ~1 meter)
  @spec position_changed?(struct(), struct()) :: boolean()
  defp position_changed?(packet1, packet2) do
    {lat1, lng1, _} = MapHelpers.get_coordinates(packet1)
    {lat2, lng2, _} = MapHelpers.get_coordinates(packet2)

    abs(lat1 - lat2) > 0.0001 || abs(lng1 - lng2) > 0.0001
  end

  # Fetch historical packets from the database
  @spec fetch_historical_packets(list(), DateTime.t(), DateTime.t()) :: [struct()]
  defp fetch_historical_packets(bounds, start_time, end_time) do
    effective_start_time = start_time

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
    packets_module = Application.get_env(:aprsme, :packets_module, Aprsme.Packets)
    packets = packets_module.get_packets_for_replay(packets_params)

    # Sort packets by received_at timestamp to ensure chronological replay
    Enum.sort_by(packets, & &1.received_at)
  end

  @spec load_historical_packets_for_bounds(Socket.t(), map()) :: Socket.t()
  defp load_historical_packets_for_bounds(socket, map_bounds) do
    now = DateTime.utc_now()
    historical_hours = String.to_integer(socket.assigns.historical_hours)
    start_time = DateTime.add(now, -historical_hours * 3600, :second)

    bounds = [
      map_bounds.west,
      map_bounds.south,
      map_bounds.east,
      map_bounds.north
    ]

    historical_packets = fetch_historical_packets(bounds, start_time, now)

    if Enum.empty?(historical_packets) do
      assign(socket, historical_loaded: true)
    else
      process_historical_packets(socket, historical_packets)
    end
  end

  @spec load_historical_packets_for_bounds_optimized(Socket.t(), map()) :: Socket.t()
  defp load_historical_packets_for_bounds_optimized(socket, map_bounds) do
    bounds = [
      map_bounds.west,
      map_bounds.south,
      map_bounds.east,
      map_bounds.north
    ]

    # Use the optimized query for initial load with smaller limit for faster loading
    packets_module = Application.get_env(:aprsme, :packets_module, Aprsme.Packets)

    historical_packets =
      packets_module.get_recent_packets_optimized(%{
        bounds: bounds,
        # Reduced limit for faster initial load
        limit: 200
      })

    if Enum.empty?(historical_packets) do
      assign(socket, historical_loaded: true)
    else
      process_historical_packets(socket, historical_packets)
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

  # Get location from IP using ip-api.com
  @spec get_ip_location(String.t() | nil) :: {float(), float()} | nil
  defp get_ip_location(nil), do: nil

  defp get_ip_location(ip) do
    # Asynchronously fetch IP location
    case Req.get("https://ip-api.com/json/#{ip}") do
      {:ok, %Req.Response{status: 200, body: body}} -> handle_ip_api_response(body)
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
    map_bounds = %{
      north: bounds["north"],
      south: bounds["south"],
      east: bounds["east"],
      west: bounds["west"]
    }

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
    # Only schedule a bounds update if the bounds have actually changed (with rounding)
    if compare_bounds(map_bounds, socket.assigns.map_bounds) do
      {:noreply, socket}
    else
      schedule_bounds_update(map_bounds, socket)
    end
  end

  defp schedule_bounds_update(map_bounds, socket) do
    if socket.assigns[:bounds_update_timer] do
      Process.cancel_timer(socket.assigns.bounds_update_timer)
    end

    timer_ref = Process.send_after(self(), {:process_bounds_update, map_bounds}, 250)
    socket = assign(socket, bounds_update_timer: timer_ref, pending_bounds: map_bounds)
    {:noreply, socket}
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

    # Remove only out-of-bounds historical packets instead of clearing all
    socket = push_event(socket, "filter_markers_by_bounds", %{bounds: map_bounds})

    # Load additional historical packets for the new bounds if needed
    socket = load_historical_packets_for_bounds(socket, map_bounds)

    # Update map bounds and visible packets
    assign(socket, map_bounds: map_bounds, visible_packets: new_visible_packets)
  end
end
