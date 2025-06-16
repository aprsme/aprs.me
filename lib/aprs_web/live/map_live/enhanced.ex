defmodule AprsWeb.MapLive.Enhanced do
  @moduledoc false
  use AprsWeb, :live_view

  import Ecto.Query
  import Phoenix.Component, except: [update: 3]

  alias Aprs.Packet
  alias Aprs.Repo

  # Default map settings
  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 4
  @max_markers 500
  # 30 seconds
  @cleanup_interval 30_000
  @packet_retention_minutes 60

  def mount(_params, _session, socket) do
    # Schedule periodic cleanup
    if connected?(socket) do
      Process.send_after(self(), :cleanup_old_markers, @cleanup_interval)
    end

    socket =
      assign(socket,
        # Map state
        map_center: @default_center,
        map_zoom: @default_zoom,
        map_bounds: default_bounds(),

        # Marker management
        active_markers: %{},
        historical_markers: %{},
        marker_count: 0,

        # UI state
        page_title: "APRS Map - Enhanced",
        loading: false,
        geolocation_error: nil,

        # Replay functionality
        replay_active: false,
        replay_speed: 1000,
        replay_paused: false,
        replay_timer: nil,
        replay_packets: [],
        replay_index: 0
      )

    {:ok, socket}
  end

  def handle_event("map_ready", _params, socket) do
    # Map is ready, load initial markers
    {:noreply, load_markers_in_bounds(socket)}
  end

  def handle_event("bounds_changed", params, socket) do
    %{
      "bounds" => bounds,
      "center" => center,
      "zoom" => zoom
    } = params

    # Normalize center to use atom keys
    normalized_center = %{
      lat: center["lat"],
      lng: center["lng"]
    }

    socket =
      socket
      |> assign(:map_bounds, bounds)
      |> assign(:map_center, normalized_center)
      |> assign(:map_zoom, zoom)
      |> load_markers_in_bounds()

    {:noreply, socket}
  end

  def handle_event("marker_clicked", params, socket) do
    %{"id" => marker_id, "callsign" => callsign} = params

    # You could add marker click logic here
    # For example, show detailed info, center map, etc.
    IO.puts("Marker clicked: #{callsign} (#{marker_id})")

    {:noreply, socket}
  end

  def handle_event("locate_me", _params, socket) do
    # Request geolocation from client
    {:noreply, push_event(socket, "request_geolocation", %{})}
  end

  def handle_event("set_location", %{"lat" => lat, "lng" => lng}, socket) do
    center = %{lat: lat, lng: lng}
    zoom = 12

    socket =
      socket
      |> assign(:map_center, center)
      |> assign(:map_zoom, zoom)
      |> assign(:geolocation_error, nil)

    # Tell client to zoom to location
    socket =
      push_event(socket, "zoom_to_location", %{
        lat: lat,
        lng: lng,
        zoom: zoom
      })

    {:noreply, socket}
  end

  def handle_event("geolocation_error", %{"error" => error}, socket) do
    {:noreply, assign(socket, :geolocation_error, error)}
  end

  def handle_event("toggle_replay", _params, socket) do
    if socket.assigns.replay_active do
      stop_replay(socket)
    else
      start_replay(socket)
    end
  end

  def handle_event("pause_replay", _params, socket) do
    socket =
      if socket.assigns.replay_timer do
        Process.cancel_timer(socket.assigns.replay_timer)
        assign(socket, replay_timer: nil, replay_paused: true)
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event("adjust_replay_speed", %{"speed" => speed}, socket) do
    speed_ms = String.to_integer(speed)
    {:noreply, assign(socket, :replay_speed, speed_ms)}
  end

  def handle_info(:cleanup_old_markers, socket) do
    socket = cleanup_old_markers(socket)

    # Schedule next cleanup
    Process.send_after(self(), :cleanup_old_markers, @cleanup_interval)

    {:noreply, socket}
  end

  def handle_info(:replay_next, socket) do
    socket = process_next_replay_packet(socket)
    {:noreply, socket}
  end

  def handle_info({:new_packet, packet}, socket) do
    # Handle real-time packet updates
    if has_position_data?(packet) and within_bounds?(packet, socket.assigns.map_bounds) do
      socket = add_packet_marker(socket, packet, false)
      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end

  def render(assigns) do
    ~H"""
    <!-- Leaflet CSS -->
    <link
      rel="stylesheet"
      href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
      integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="
      crossorigin=""
    />

    <!-- Leaflet JS -->
    <script
      src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
      integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="
      crossorigin=""
    >
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

      .map-controls {
        position: absolute;
        left: 10px;
        top: 10px;
        z-index: 1000;
        display: flex;
        flex-direction: column;
        gap: 10px;
      }

      .control-button {
        background: white;
        border: 2px solid rgba(0,0,0,0.2);
        border-radius: 4px;
        padding: 8px 12px;
        cursor: pointer;
        font-size: 14px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.3);
      }

      .control-button:hover {
        background: #f4f4f4;
      }

      .control-button:disabled {
        opacity: 0.6;
        cursor: not-allowed;
      }

      .map-info {
        position: absolute;
        right: 10px;
        top: 10px;
        z-index: 1000;
        background: white;
        padding: 10px;
        border-radius: 4px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.3);
        font-size: 12px;
      }

      .replay-controls {
        position: absolute;
        bottom: 20px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 1000;
        background: white;
        padding: 10px;
        border-radius: 4px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.3);
        display: flex;
        gap: 10px;
        align-items: center;
      }

      .aprs-marker {
        background: transparent !important;
        border: none !important;
      }

      .historical-marker {
        opacity: 0.7;
      }
    </style>

    <!-- Map Container -->
    <div
      id="aprs-map"
      phx-hook="APRSMap"
      phx-update="ignore"
      data-center={Jason.encode!(@map_center)}
      data-zoom={@map_zoom}
    >
    </div>

    <!-- Map Controls -->
    <div class="map-controls">
      <button class="control-button" phx-click="locate_me" title="Find my location">
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="16"
          height="16"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
        >
          <circle cx="12" cy="12" r="10" />
          <line x1="12" y1="8" x2="12" y2="16" />
          <line x1="8" y1="12" x2="16" y2="12" />
        </svg>
        Locate Me
      </button>

      <button class="control-button" phx-click="toggle_replay">
        <%= if @replay_active do %>
          Stop Replay
        <% else %>
          Start Replay
        <% end %>
      </button>

      <%= if @replay_active do %>
        <button class="control-button" phx-click="pause_replay" disabled={@replay_paused}>
          <%= if @replay_paused do %>
            Resume
          <% else %>
            Pause
          <% end %>
        </button>
      <% end %>
    </div>

    <!-- Map Info Panel -->
    <div class="map-info">
      <div><strong>Active Markers:</strong> {map_size(@active_markers)}</div>
      <div><strong>Historical:</strong> {map_size(@historical_markers)}</div>
      <div><strong>Zoom:</strong> {@map_zoom}</div>
      <div>
        <strong>Center:</strong> {Float.round(@map_center.lat, 4)}, {Float.round(@map_center.lng, 4)}
      </div>

      <%= if @geolocation_error do %>
        <div style="color: red; margin-top: 5px;">
          <strong>Location Error:</strong> {@geolocation_error}
        </div>
      <% end %>

      <%= if @loading do %>
        <div style="color: blue; margin-top: 5px;">Loading markers...</div>
      <% end %>
    </div>

    <!-- Replay Controls -->
    <%= if @replay_active do %>
      <div class="replay-controls">
        <label>Speed:</label>
        <select phx-change="adjust_replay_speed" name="speed">
          <option value="2000" selected={@replay_speed == 2000}>Slow</option>
          <option value="1000" selected={@replay_speed == 1000}>Normal</option>
          <option value="500" selected={@replay_speed == 500}>Fast</option>
          <option value="200" selected={@replay_speed == 200}>Very Fast</option>
        </select>

        <span>Progress: {@replay_index}/{length(@replay_packets)}</span>
      </div>
    <% end %>
    """
  end

  # Private helper functions

  defp default_bounds do
    %{
      north: 49.0,
      south: 24.0,
      east: -66.0,
      west: -125.0
    }
  end

  defp load_markers_in_bounds(socket) do
    bounds = socket.assigns.map_bounds

    # Don't load if we don't have proper bounds yet
    if bounds == default_bounds() do
      socket
    else
      socket = assign(socket, :loading, true)

      # Fetch recent packets within bounds
      packets = fetch_packets_in_bounds(bounds, @max_markers)

      # Convert packets to marker data and send to client
      markers = Enum.map(packets, &packet_to_marker_data/1)

      # Clear existing markers and add new ones
      socket =
        socket
        |> push_event("clear_markers", %{})
        |> push_event("add_markers", %{markers: markers})
        |> update_active_markers(packets)
        |> assign(:loading, false)

      socket
    end
  end

  defp fetch_packets_in_bounds(bounds, limit) do
    cutoff_time = DateTime.add(DateTime.utc_now(), -@packet_retention_minutes * 60, :second)

    Repo.all(
      from(p in Packet,
        where: p.has_position == true,
        where: p.received_at >= ^cutoff_time,
        where: p.lat >= ^bounds["south"],
        where: p.lat <= ^bounds["north"],
        where: p.lon >= ^bounds["west"],
        where: p.lon <= ^bounds["east"],
        order_by: [desc: p.received_at],
        limit: ^limit
      )
    )
  end

  defp packet_to_marker_data(packet) do
    data_extended = packet.data_extended || %{}
    callsign = packet.base_callsign <> if packet.ssid, do: "-#{packet.ssid}", else: ""

    %{
      id: callsign,
      callsign: callsign,
      lat: packet.lat,
      lng: packet.lon,
      symbol_table: data_extended["symbol_table_id"] || "/",
      symbol_code: data_extended["symbol_code"] || ">",
      historical: false,
      popup: build_popup_content(packet, callsign, false)
    }
  end

  defp build_popup_content(packet, callsign, historical) do
    data_extended = packet.data_extended || %{}

    timestamp =
      if historical do
        DateTime.to_string(packet.received_at)
      else
        packet.received_at |> DateTime.to_time() |> Time.to_string()
      end

    """
    <div style="min-width: 200px;">
      <h4 style="margin: 0 0 5px 0; font-weight: bold;">#{callsign} #{if historical, do: "(Historical)", else: ""}</h4>
      <p style="margin: 2px 0; font-size: 12px;">
        <strong>Position:</strong> #{Float.round(packet.lat, 4)}°, #{Float.round(packet.lon, 4)}°<br>
        <strong>Type:</strong> #{packet.data_type}<br>
        #{if data_extended["comment"], do: "<strong>Comment:</strong> #{data_extended["comment"]}<br>", else: ""}
        <strong>Path:</strong> #{packet.path}<br>
        <strong>Time:</strong> #{timestamp}
      </p>
    </div>
    """
  end

  defp update_active_markers(socket, packets) do
    active_markers =
      Map.new(packets, fn packet ->
        callsign = packet.base_callsign <> if packet.ssid, do: "-#{packet.ssid}", else: ""
        {callsign, %{packet: packet, added_at: DateTime.utc_now()}}
      end)

    assign(socket, :active_markers, active_markers)
  end

  defp add_packet_marker(socket, packet, historical) do
    if has_position_data?(packet) do
      marker_data = packet_to_marker_data(packet)
      marker_data = Map.put(marker_data, :historical, historical)

      socket = push_event(socket, "add_marker", marker_data)

      # Update marker tracking
      callsign = marker_data.id
      marker_info = %{packet: packet, added_at: DateTime.utc_now()}

      if historical do
        Phoenix.Component.update(socket, :historical_markers, &Map.put(&1, callsign, marker_info))
      else
        Phoenix.Component.update(socket, :active_markers, &Map.put(&1, callsign, marker_info))
      end
    else
      socket
    end
  end

  defp cleanup_old_markers(socket) do
    cutoff_time = DateTime.add(DateTime.utc_now(), -@packet_retention_minutes * 60, :second)

    # Find old markers
    old_markers =
      socket.assigns.active_markers
      |> Enum.filter(fn {_callsign, %{added_at: added_at}} ->
        DateTime.before?(added_at, cutoff_time)
      end)
      |> Enum.map(fn {callsign, _} -> callsign end)

    # Remove old markers from client
    socket =
      Enum.reduce(old_markers, socket, fn callsign, acc_socket ->
        push_event(acc_socket, "remove_marker", %{id: callsign})
      end)

    # Update server state
    active_markers =
      socket.assigns.active_markers
      |> Enum.reject(fn {callsign, _} -> callsign in old_markers end)
      |> Map.new()

    assign(socket, :active_markers, active_markers)
  end

  defp start_replay(socket) do
    # Fetch historical packets for replay
    bounds = socket.assigns.map_bounds
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)
    two_hours_ago = DateTime.add(DateTime.utc_now(), -7200, :second)

    packets = fetch_historical_packets(bounds, two_hours_ago, one_hour_ago)

    socket =
      socket
      |> assign(:replay_active, true)
      |> assign(:replay_packets, packets)
      |> assign(:replay_index, 0)
      |> assign(:replay_paused, false)

    # Start replay timer
    timer = Process.send_after(self(), :replay_next, socket.assigns.replay_speed)
    assign(socket, :replay_timer, timer)
  end

  defp stop_replay(socket) do
    # Cancel timer if running
    if socket.assigns.replay_timer do
      Process.cancel_timer(socket.assigns.replay_timer)
    end

    # Clear historical markers
    socket = push_event(socket, "clear_markers", %{})

    socket
    |> assign(:replay_active, false)
    |> assign(:replay_timer, nil)
    |> assign(:replay_packets, [])
    |> assign(:replay_index, 0)
    |> assign(:historical_markers, %{})
    # Reload current markers
    |> load_markers_in_bounds()
  end

  defp process_next_replay_packet(socket) do
    packets = socket.assigns.replay_packets
    index = socket.assigns.replay_index

    if index < length(packets) do
      packet = Enum.at(packets, index)
      socket = add_packet_marker(socket, packet, true)

      # Schedule next packet
      timer = Process.send_after(self(), :replay_next, socket.assigns.replay_speed)

      socket
      |> assign(:replay_index, index + 1)
      |> assign(:replay_timer, timer)
    else
      # Replay finished
      assign(socket, :replay_timer, nil)
    end
  end

  defp fetch_historical_packets(bounds, start_time, end_time) do
    Repo.all(
      from(p in Packet,
        where: p.has_position == true,
        where: p.received_at >= ^start_time,
        where: p.received_at <= ^end_time,
        where: p.lat >= ^bounds["south"],
        where: p.lat <= ^bounds["north"],
        where: p.lon >= ^bounds["west"],
        where: p.lon <= ^bounds["east"],
        order_by: [asc: p.received_at],
        limit: 1000
      )
    )
  end

  defp has_position_data?(packet) do
    packet.has_position == true and
      packet.lat != nil and
      packet.lon != nil
  end

  defp within_bounds?(packet, bounds) do
    lat = packet.lat
    lng = packet.lon

    lat >= bounds["south"] and lat <= bounds["north"] and
      lng >= bounds["west"] and lng <= bounds["east"]
  end
end
