defmodule AprsWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsWeb, :live_view

  alias AprsWeb.Endpoint
  alias AprsWeb.MapLive.MapHelpers
  alias AprsWeb.MapLive.PacketUtils
  alias Phoenix.LiveView.Socket

  require Logger

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5
  @finch_name Aprs.Finch

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to packet updates
      Phoenix.PubSub.subscribe(Aprs.PubSub, "packets")
      Phoenix.PubSub.subscribe(Aprs.PubSub, "bad_packets")

      # Schedule periodic cleanup of old packets
      Process.send_after(self(), :cleanup_old_packets, 60_000)
    end

    # Get deployment timestamp from environment variable or use current time as fallback
    deployed_at =
      case System.get_env("DEPLOYED_AT") do
        nil ->
          # Fallback to current time if not set
          DateTime.utc_now()

        timestamp when is_binary(timestamp) ->
          case DateTime.from_iso8601(timestamp) do
            {:ok, datetime, _} -> datetime
            _ -> DateTime.utc_now()
          end
      end

    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    socket = assign_defaults(socket, one_hour_ago)
    socket = assign(socket, packet_buffer: [], buffer_timer: nil)
    socket = assign(socket, all_packets: %{})

    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      Phoenix.PubSub.subscribe(Aprs.PubSub, "postgres:aprs_packets")
      maybe_start_geolocation(socket)
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
       packet_age_threshold: 3600,
       slideover_open: true,
       replay_active: false,
       replay_start_time: nil,
       replay_end_time: nil,
       replay_current_time: nil,
       replay_speed: 1,
       deployed_at: deployed_at
     )}
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

  @spec maybe_start_geolocation(Socket.t()) :: Socket.t()
  defp maybe_start_geolocation(socket) do
    if geolocation_enabled?() do
      ip_for_geolocation =
        if Application.get_env(:aprs, AprsWeb.Endpoint)[:code_reloader] do
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

  @impl true
  def handle_event("bounds_changed", %{"bounds" => bounds}, socket) do
    Logger.debug("handle_event bounds_changed: #{inspect(bounds)} vs current #{inspect(socket.assigns.map_bounds)}")

    handle_bounds_update(bounds, socket)
  end

  @impl true
  def handle_event("update_bounds", %{"bounds" => bounds}, socket) do
    Logger.debug("handle_event update_bounds: #{inspect(bounds)} vs current #{inspect(socket.assigns.map_bounds)}")

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

    visible_packets_list =
      filtered_packets
      |> Enum.map(fn {_callsign, packet} -> build_packet_data(packet) end)
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

    # Start historical replay
    Process.send_after(self(), :initialize_replay, 500)

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
    {:noreply, socket}
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

  @spec handle_bounds_update(map(), Socket.t()) :: {:noreply, Socket.t()}
  defp handle_bounds_update(bounds, socket) do
    # Update the map bounds from the client
    map_bounds = %{
      north: bounds["north"],
      south: bounds["south"],
      east: bounds["east"],
      west: bounds["west"]
    }

    Logger.debug("handle_bounds_update: new #{inspect(map_bounds)} vs current #{inspect(socket.assigns.map_bounds)}")

    # Validate bounds to prevent invalid coordinates
    if map_bounds.north > 90 or map_bounds.south < -90 or
         map_bounds.north <= map_bounds.south do
      # Invalid bounds, skip update
      {:noreply, socket}
    else
      # Only schedule a bounds update if the bounds have actually changed (with rounding)
      if compare_bounds(map_bounds, socket.assigns.map_bounds) do
        {:noreply, socket}
        # Cancel any pending bounds update timer
      else
        if socket.assigns[:bounds_update_timer] do
          Process.cancel_timer(socket.assigns.bounds_update_timer)
        end

        timer_ref = Process.send_after(self(), {:process_bounds_update, map_bounds}, 250)
        socket = assign(socket, bounds_update_timer: timer_ref, pending_bounds: map_bounds)
        {:noreply, socket}
      end
    end
  end

  @spec process_bounds_update(map(), Socket.t()) :: Socket.t()
  defp process_bounds_update(map_bounds, socket) do
    Logger.debug("process_bounds_update: Loading historical packets for bounds #{inspect(map_bounds)}")

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

    # Clear existing historical packets
    socket = push_event(socket, "clear_historical_packets", %{})

    # Load historical packets for the new bounds
    socket = load_historical_packets_for_bounds(socket, map_bounds)

    # Update map bounds and visible packets
    assign(socket, map_bounds: map_bounds, visible_packets: new_visible_packets)
  end

  @impl true
  def handle_info({:process_bounds_update, map_bounds}, socket), do: handle_info_process_bounds_update(map_bounds, socket)

  def handle_info({:delayed_zoom, %{lat: lat, lng: lng}}, socket), do: handle_info_delayed_zoom(lat, lng, socket)

  def handle_info({:ip_location, %{lat: lat, lng: lng}}, socket), do: handle_info_ip_location(lat, lng, socket)

  def handle_info(:initialize_replay, socket), do: handle_info_initialize_replay(socket)

  def handle_info(:cleanup_old_packets, socket), do: handle_cleanup_old_packets(socket)

  def handle_info(:reload_historical_packets, socket), do: handle_reload_historical_packets(socket)

  def handle_info({:postgres_packet, packet}, socket), do: handle_info_postgres_packet(packet, socket)

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
      socket = start_historical_replay(socket)
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
    marker_data = build_packet_data(packet)

    socket =
      if marker_data do
        push_event(socket, "add_markers", %{markers: [marker_data]})
      else
        socket
      end

    {:noreply, assign(socket, visible_packets: new_visible_packets)}
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

    <!-- Slideover Toggle Button -->
    <button
      class={["slideover-toggle", if(@slideover_open, do: "slideover-open", else: "slideover-closed")]}
      phx-click="toggle_slideover"
      title={if @slideover_open, do: "Hide controls", else: "Show controls"}
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
          title="Close controls"
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
            <span>Search Callsign</span>
          </label>
          <form phx-submit="search_callsign" class="flex space-x-2">
            <input
              type="text"
              name="callsign"
              value={@overlay_callsign}
              phx-change="update_callsign"
              placeholder="Enter callsign..."
              class="flex-1 px-4 py-3 border border-slate-300 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm uppercase placeholder-slate-400 transition-all duration-200 hover:border-slate-400"
            />
            <button
              type="submit"
              class="px-6 py-3 bg-gradient-to-r from-indigo-600 to-purple-600 text-white rounded-xl hover:from-indigo-700 hover:to-purple-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 transition-all duration-200 text-sm font-semibold shadow-lg hover:shadow-xl"
            >
              Search
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
            <span>Trail Duration</span>
          </label>
          <form phx-change="update_trail_duration" class="relative">
            <select
              name="trail_duration"
              class="w-full px-4 py-3 border border-slate-300 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm bg-white appearance-none transition-all duration-200 hover:border-slate-400"
            >
              <option value="1" selected={@trail_duration == "1"}>1 Hour</option>
              <option value="6" selected={@trail_duration == "6"}>6 Hours</option>
              <option value="12" selected={@trail_duration == "12"}>12 Hours</option>
              <option value="24" selected={@trail_duration == "24"}>24 Hours</option>
              <option value="48" selected={@trail_duration == "48"}>48 Hours</option>
              <option value="168" selected={@trail_duration == "168"}>1 Week</option>
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
          <p class="text-xs text-slate-500 flex items-center space-x-1">
            <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <span>How long should position trails be displayed</span>
          </p>
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
            <span>Historical Data</span>
          </label>
          <form phx-change="update_historical_hours" class="relative">
            <select
              name="historical_hours"
              class="w-full px-4 py-3 border border-slate-300 rounded-xl shadow-sm focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500 text-sm bg-white appearance-none transition-all duration-200 hover:border-slate-400"
            >
              <option value="1" selected={@historical_hours == "1"}>1 Hour</option>
              <option value="3" selected={@historical_hours == "3"}>3 Hours</option>
              <option value="6" selected={@historical_hours == "6"}>6 Hours</option>
              <option value="12" selected={@historical_hours == "12"}>12 Hours</option>
              <option value="24" selected={@historical_hours == "24"}>24 Hours</option>
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
          <p class="text-xs text-slate-500 flex items-center space-x-1">
            <svg class="w-3 h-3" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <span>How many hours of historical packets to load</span>
          </p>
        </div>
        
    <!-- Navigation Links (Mobile) -->
        <div class="lg:hidden pt-4 border-t border-slate-200 space-y-3">
          <.link
            navigate={~p"/packets"}
            class="flex items-center space-x-2 text-sm text-blue-600 hover:text-blue-800 transition-colors"
          >
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M9 5H7a2 2 0 00-2 2v10a2 2 0 002 2h8a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"
              />
            </svg>
            <span>View All Packets</span>
          </.link>
          <.link
            navigate={~p"/badpackets"}
            class="flex items-center space-x-2 text-sm text-blue-600 hover:text-blue-800 transition-colors"
          >
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path
                stroke-linecap="round"
                stroke-linejoin="round"
                stroke-width="2"
                d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-2.5L13.732 4c-.77-.833-1.964-.833-2.732 0L3.082 16.5c-.77.833.192 2.5 1.732 2.5z"
              />
            </svg>
            <span>View Bad Packets</span>
          </.link>
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
            <span class="font-medium">Last Deploy</span>
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
      socket = assign(socket, historical_packets: %{})

      # Clear all markers on the client
      socket = push_event(socket, "clear_all_markers", %{})

      # Load historical packets with new time range
      socket = load_historical_packets_for_bounds(socket, socket.assigns.map_bounds)

      {:noreply, socket}
    else
      {:noreply, socket}
    end
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
    # Only fetch historical packets if map is ready and bounds are available
    if socket.assigns.map_ready and socket.assigns.map_bounds do
      # Get time range for historical data
      now = DateTime.utc_now()
      # Use the user's selected historical hours setting
      historical_hours = String.to_integer(socket.assigns.historical_hours)
      start_time = DateTime.add(now, -historical_hours * 3600, :second)

      # Convert map bounds to the format expected by the database query
      bounds = [
        socket.assigns.map_bounds.west,
        socket.assigns.map_bounds.south,
        socket.assigns.map_bounds.east,
        socket.assigns.map_bounds.north
      ]

      # Fetch historical packets with position data within the current map bounds
      historical_packets = fetch_historical_packets(bounds, start_time, now)

      if Enum.empty?(historical_packets) do
        # No historical packets found - silently continue
        socket
      else
        # Clear any previous historical packets from the map
        socket = push_event(socket, "clear_historical_packets", %{})

        # Group packets by callsign-ssid and process all positions
        packet_data_list =
          historical_packets
          |> Enum.group_by(&generate_callsign/1)
          |> Enum.flat_map(fn {callsign, packets} ->
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

            # Process all packets with unique positions, marking which is the most recent
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
          end)

        # Send all historical packets at once
        socket = push_event(socket, "add_historical_packets", %{packets: packet_data_list})

        # Store historical packets in assigns for reference
        historical_packets_map =
          packet_data_list
          |> Enum.zip(historical_packets)
          |> Enum.reduce(%{}, fn {packet_data, packet}, acc ->
            Map.put(acc, packet_data["id"], packet)
          end)

        assign(socket,
          historical_packets: historical_packets_map,
          historical_loaded: true
        )
      end
    else
      socket
    end
  end

  # Filter packets to only include those with unique positions (lat/lon changed)
  @spec filter_unique_positions([struct()]) :: [struct()]
  defp filter_unique_positions(packets) do
    packets
    |> Enum.reduce([], fn packet, acc ->
      {lat, lon, _} = MapHelpers.get_coordinates(packet)

      if lat && lon do
        case acc do
          [] ->
            # First packet, always include
            [packet | acc]

          [last_packet | _] ->
            if position_changed?(packet, last_packet) do
              [packet | acc]
            else
              acc
            end
        end
      else
        acc
      end
    end)
    |> Enum.reverse()
  end

  # Check if position changed significantly between two packets (more than ~1 meter)
  @spec position_changed?(struct(), struct()) :: boolean()
  defp position_changed?(packet1, packet2) do
    {lat1, lng1, _} = MapHelpers.get_coordinates(packet1)
    {lat2, lng2, _} = MapHelpers.get_coordinates(packet2)

    abs(lat1 - lat2) > 0.00001 || abs(lng1 - lng2) > 0.00001
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
    packets_module = Application.get_env(:aprs, :packets_module, Aprs.Packets)
    packets = packets_module.get_packets_for_replay(packets_params)

    # Sort packets by received_at timestamp to ensure chronological replay
    Enum.sort_by(packets, fn packet -> packet.received_at end)
  end

  @spec load_historical_packets_for_bounds(Socket.t(), map()) :: Socket.t()
  defp load_historical_packets_for_bounds(socket, map_bounds) do
    # Get time range for historical data
    now = DateTime.utc_now()
    # Use the user's selected historical hours setting
    historical_hours = String.to_integer(socket.assigns.historical_hours)
    start_time = DateTime.add(now, -historical_hours * 3600, :second)

    # Convert map bounds to the format expected by the database query
    bounds = [
      map_bounds.west,
      map_bounds.south,
      map_bounds.east,
      map_bounds.north
    ]

    # Fetch historical packets with position data within the current map bounds
    historical_packets = fetch_historical_packets(bounds, start_time, now)

    if Enum.empty?(historical_packets) do
      # No historical packets found
      socket
    else
      # Group packets by callsign-ssid and process all positions
      packet_data_list =
        historical_packets
        |> Enum.group_by(&generate_callsign/1)
        |> Enum.flat_map(fn {callsign, packets} ->
          # Sort packets by inserted_at to identify the most recent
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

          # Filter out packets with unchanged positions
          unique_position_packets = filter_unique_positions(sorted_packets)

          # Process all packets with unique positions, marking which is the most recent
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
        end)

      # Send all historical packets at once
      socket = push_event(socket, "add_historical_packets", %{packets: packet_data_list})

      # Store historical packets in assigns for reference
      historical_packets_map =
        packet_data_list
        |> Enum.zip(historical_packets)
        |> Enum.reduce(%{}, fn {packet_data, packet}, acc ->
          Map.put(acc, packet_data["id"], packet)
        end)

      assign(socket, historical_packets: historical_packets_map)
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

  @spec build_packet_data(map() | struct()) :: map() | nil
  defp build_packet_data(packet) do
    {lat, lon, data_extended} = MapHelpers.get_coordinates(packet)
    callsign = Map.get(packet, :base_callsign, Map.get(packet, "base_callsign", ""))

    # Only include packets with valid position data and a non-empty callsign
    if lat && lon && callsign != "" && callsign != nil do
      build_packet_map(packet, lat, lon, data_extended)
    end
  end

  @spec build_packet_map(map() | struct(), number(), number(), map() | nil) :: map()
  defp build_packet_map(packet, lat, lon, data_extended) do
    data_extended = data_extended || %{}
    packet_info = extract_packet_info(packet, data_extended)
    popup = build_popup_content(packet, packet_info, lat, lon)

    build_packet_result(packet, packet_info, lat, lon, popup)
  end

  defp extract_packet_info(packet, data_extended) do
    %{
      callsign: PacketUtils.generate_callsign(packet),
      symbol_table_id: PacketUtils.get_packet_field(packet, :symbol_table_id, "/"),
      symbol_code: PacketUtils.get_packet_field(packet, :symbol_code, ">"),
      timestamp: PacketUtils.get_timestamp(packet),
      comment: PacketUtils.get_packet_field(packet, :comment, ""),
      safe_data_extended: PacketUtils.convert_tuples_to_strings(data_extended),
      is_weather_packet: PacketUtils.is_weather_packet?(packet)
    }
  end

  defp build_popup_content(packet, packet_info, lat, lon) do
    if packet_info.is_weather_packet do
      build_weather_popup_html(packet, packet_info.callsign)
    else
      build_standard_popup_html(packet_info, lat, lon)
    end
  end

  defp build_standard_popup_html(packet_info, lat, lon) do
    comment_html =
      if packet_info.comment == "",
        do: "",
        else: ~s(<div class="aprs-comment">#{packet_info.comment}</div>)

    """
    <div class="aprs-popup">
      <div class="aprs-callsign"><strong><a href="/map/callsign/#{packet_info.callsign}">#{packet_info.callsign}</a></strong></div>
      #{comment_html}
      <div class="aprs-coords">#{Float.round(PacketUtils.to_float(lat), 4)}, #{Float.round(PacketUtils.to_float(lon), 4)}</div>
      <div class="aprs-time">#{packet_info.timestamp}</div>
    </div>
    """
  end

  defp build_packet_result(packet, packet_info, lat, lon, popup) do
    %{
      "id" => packet_info.callsign,
      "callsign" => packet_info.callsign,
      "base_callsign" => PacketUtils.get_packet_field(packet, :base_callsign, ""),
      "ssid" => PacketUtils.get_packet_field(packet, :ssid, 0),
      "lat" => PacketUtils.to_float(lat),
      "lng" => PacketUtils.to_float(lon),
      "data_type" => to_string(PacketUtils.get_packet_field(packet, :data_type, "unknown")),
      "path" => PacketUtils.get_packet_field(packet, :path, ""),
      "comment" => packet_info.comment,
      "data_extended" => packet_info.safe_data_extended || %{},
      "symbol_table_id" => packet_info.symbol_table_id,
      "symbol_code" => packet_info.symbol_code,
      "symbol_description" => "Symbol: #{packet_info.symbol_table_id}#{packet_info.symbol_code}",
      "timestamp" => packet_info.timestamp,
      "popup" => popup
    }
  end

  defp build_weather_popup_html(packet, callsign) do
    received_at = get_received_at(packet)
    timestamp_str = format_timestamp(received_at)

    """
    <strong>#{callsign} - Weather Report</strong><br>
    <small>#{timestamp_str} UTC</small>
    <hr>
    Temperature: #{PacketUtils.get_weather_field(packet, :temperature)}°F<br>
    Humidity: #{PacketUtils.get_weather_field(packet, :humidity)}%<br>
    Wind: #{PacketUtils.get_weather_field(packet, :wind_direction)}° at #{PacketUtils.get_weather_field(packet, :wind_speed)} mph, gusts to #{PacketUtils.get_weather_field(packet, :wind_gust)} mph<br>
    Pressure: #{PacketUtils.get_weather_field(packet, :pressure)} hPa<br>
    Rain (1h): #{PacketUtils.get_weather_field(packet, :rain_1h)} in.<br>
    Rain (24h): #{PacketUtils.get_weather_field(packet, :rain_24h)} in.<br>
    Rain (since midnight): #{PacketUtils.get_weather_field(packet, :rain_since_midnight)} in.<br>
    """
  end

  defp get_received_at(packet) do
    cond do
      Map.has_key?(packet, :received_at) -> packet.received_at
      Map.has_key?(packet, "received_at") -> packet["received_at"]
      true -> nil
    end
  end

  defp format_timestamp(received_at) do
    if received_at,
      do: Calendar.strftime(received_at, "%Y-%m-%d %H:%M:%S"),
      else: "N/A"
  end

  @spec generate_callsign(map() | struct()) :: String.t()
  defp generate_callsign(packet) do
    base_callsign = Map.get(packet, :base_callsign, Map.get(packet, "base_callsign", ""))
    ssid = Map.get(packet, :ssid, Map.get(packet, "ssid", 0))

    if ssid != 0 and ssid != "" and ssid != nil do
      "#{base_callsign}-#{ssid}"
    else
      base_callsign
    end
  end

  # Get location from IP using ip-api.com
  @spec get_ip_location(String.t() | nil) :: {float(), float()} | nil
  defp get_ip_location(nil), do: nil

  defp get_ip_location(ip) do
    # Asynchronously fetch IP location
    case :get |> Finch.build("https://ip-api.com/json/#{ip}") |> Finch.request(@finch_name) do
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

  # Rails-style time ago helper
  defp time_ago_in_words(datetime) do
    now = DateTime.utc_now()
    diff_seconds = DateTime.diff(now, datetime, :second)

    cond do
      diff_seconds < 60 -> "less than a minute"
      diff_seconds < 120 -> "1 minute"
      diff_seconds < 3600 -> "#{div(diff_seconds, 60)} minutes"
      diff_seconds < 7200 -> "1 hour"
      diff_seconds < 86_400 -> "#{div(diff_seconds, 3600)} hours"
      diff_seconds < 172_800 -> "1 day"
      diff_seconds < 2_592_000 -> "#{div(diff_seconds, 86_400)} days"
      diff_seconds < 5_184_000 -> "1 month"
      diff_seconds < 31_536_000 -> "#{div(diff_seconds, 2_592_000)} months"
      diff_seconds < 63_072_000 -> "1 year"
      true -> "#{div(diff_seconds, 31_536_000)} years"
    end <> " ago"
  end
end
