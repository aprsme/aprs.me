defmodule AprsWeb.MapLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS packets on a map
  """
  use AprsWeb, :live_view

  alias Aprs.EncodingUtils
  alias AprsWeb.Endpoint
  alias Parser.Types.MicE

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5
  @ip_api_url "http://ip-api.com/json/"
  @finch_name Aprs.Finch

  @impl true
  def mount(_params, _session, socket) do
    socket =
      assign(socket,
        packets: [],
        packet_count: 0,
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
        map_zoom: @default_zoom
      )

    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      # Get IP-based location on initial load
      ip =
        case socket.private[:connect_info][:peer_data][:address] do
          {a, b, c, d} -> "#{a}.#{b}.#{c}.#{d}"
          {a, b, c, d, e, f, g, h} -> "#{a}:#{b}:#{c}:#{d}:#{e}:#{f}:#{g}:#{h}"
          _ -> nil
        end

      if ip, do: Task.async(fn -> get_ip_location(ip) end)
    end

    {:ok, socket}
  end

  @impl true
  def handle_event("update_bounds", %{"bounds" => bounds}, socket) do
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

    packet_count = map_size(visible_packets)

    {:noreply, assign(socket, map_bounds: map_bounds, visible_packets: visible_packets, packet_count: packet_count)}
  end

  @impl true
  def handle_event("update_packet_count", %{"count" => count}, socket) do
    # Update packet count from client after markers are removed
    {:noreply, assign(socket, packet_count: count)}
  end

  @impl true
  def handle_event("locate_me", _params, socket) do
    # Send JavaScript command to request browser geolocation
    {:noreply, push_event(socket, "request_geolocation", %{})}
  end

  @impl true
  def handle_event("set_location", %{"lat" => lat, "lng" => lng}, socket) do
    # Update map center and zoom when location is received
    {:noreply, assign(socket, map_center: %{lat: lat, lng: lng}, map_zoom: 12)}
  end

  @impl true
  def handle_info(msg, socket) do
    case msg do
      {:ip_location, %{lat: lat, lng: lng}} ->
        {:noreply, assign(socket, map_center: %{lat: lat, lng: lng}, map_zoom: 12)}

      %{event: "packet", payload: payload} ->
        # Sanitize the packet to prevent encoding errors
        sanitized_packet = EncodingUtils.sanitize_packet(payload)

        # Log packet type for debugging
        IO.inspect(sanitized_packet.data_type, label: "Packet type")

        if sanitized_packet.data_extended do
          # Check if data_extended is a struct before accessing __struct__
          case sanitized_packet.data_extended do
            %{__struct__: module} -> IO.inspect(module, label: "Data extended type")
            _ -> IO.inspect("Plain map", label: "Data extended type")
          end
        end

        # Only process packets with position data that are within current map bounds
        if has_position_data?(sanitized_packet) && within_bounds?(sanitized_packet, socket.assigns.map_bounds) do
          # Convert to a simple map structure for JSON encoding
          packet_data = build_packet_data(sanitized_packet)

          # Only push if we have valid packet data
          if packet_data do
            # Generate a unique key for this packet
            callsign_key =
              "#{sanitized_packet.base_callsign}#{if sanitized_packet.ssid, do: "-#{sanitized_packet.ssid}", else: ""}"

            # Update visible packets tracking
            visible_packets = Map.put(socket.assigns.visible_packets, callsign_key, sanitized_packet)
            packet_count = map_size(visible_packets)

            # Push the packet to the client-side JavaScript
            socket =
              socket
              |> push_event("new_packet", packet_data)
              |> assign(visible_packets: visible_packets, packet_count: packet_count)

            {:noreply, socket}
          else
            # Invalid packet data, skip it
            {:noreply, socket}
          end
        else
          # Ignore packets without position data or outside bounds
          {:noreply, socket}
        end
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

      .map-overlay {
        position: absolute;
        top: 10px;
        right: 10px;
        background: rgba(255, 255, 255, 0.9);
        padding: 10px 15px;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        z-index: 1000;
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

      .packet-counter {
        font-size: 14px;
        font-weight: 600;
        color: #333;
      }

      .aprs-marker {
        background: transparent !important;
        border: none !important;
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

    <div class="map-overlay">
      <div class="packet-counter">
        <span id="packet-count">{@packet_count}</span> packets in view
      </div>
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

  # Helper functions

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

    if data_extended do
      %{
        "base_callsign" => packet.base_callsign || "",
        "ssid" => packet.ssid || "",
        "data_type" => to_string(packet.data_type || "unknown"),
        "path" => packet.path || "",
        "data_extended" => data_extended
      }
    end
  end

  # Get IP location from external service

  # Get location from IP using ip-api.com
  defp get_ip_location(nil), do: nil

  defp get_ip_location(ip) do
    url = "#{@ip_api_url}#{ip}"
    request = Finch.build(:get, url)

    case Finch.request(request, @finch_name) do
      {:ok, %{status: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, %{"lat" => lat, "lon" => lng}} when is_number(lat) and is_number(lng) ->
            if lat >= -90 and lat <= 90 and lng >= -180 and lng <= 180 do
              send(self(), {:ip_location, %{lat: lat, lng: lng}})
            else
              send(self(), {:ip_location, @default_center})
            end

          _ ->
            send(self(), {:ip_location, @default_center})
        end

      {:ok, _} ->
        send(self(), {:ip_location, @default_center})

      {:error, _} ->
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
end
