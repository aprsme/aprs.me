defmodule AprsmeWeb.InfoLive.Show do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Packets
  alias AprsmeWeb.MapLive.PacketUtils

  @neighbor_radius_km 10
  @neighbor_limit 10

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = String.upcase(String.trim(callsign))

    # Subscribe to Postgres notifications for live updates
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")
    end

    packet = get_latest_packet(normalized_callsign)
    packet = enrich_packet_with_device_info(packet)
    neighbors = get_neighbors(packet, normalized_callsign)
    has_weather_packets = PacketUtils.has_weather_packets?(normalized_callsign)

    socket =
      socket
      |> assign(:callsign, normalized_callsign)
      |> assign(:packet, packet)
      |> assign(:neighbors, neighbors)
      |> assign(:page_title, "APRS station #{normalized_callsign}")
      |> assign(:has_weather_packets, has_weather_packets)

    {:ok, socket}
  end

  @impl true
  def handle_info({:postgres_packet, packet}, socket) do
    # Only update if the packet is for our callsign
    if packet_matches_callsign?(packet, socket.assigns.callsign) do
      # Refresh data when new packet arrives
      packet = get_latest_packet(socket.assigns.callsign)
      packet = enrich_packet_with_device_info(packet)
      neighbors = get_neighbors(packet, socket.assigns.callsign)
      has_weather_packets = PacketUtils.has_weather_packets?(socket.assigns.callsign)

      socket =
        socket
        |> assign(:packet, packet)
        |> assign(:neighbors, neighbors)
        |> assign(:has_weather_packets, has_weather_packets)

      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp packet_matches_callsign?(packet, callsign) do
    packet_sender = Map.get(packet, "sender") || Map.get(packet, :sender, "")
    String.upcase(packet_sender) == String.upcase(callsign)
  end

  defp get_latest_packet(callsign) do
    %{callsign: callsign, limit: 1}
    |> Packets.get_recent_packets()
    |> List.first()
  end

  defp enrich_packet_with_device_info(nil), do: nil

  defp enrich_packet_with_device_info(packet) do
    device_identifier = Map.get(packet, :device_identifier) || Map.get(packet, "device_identifier")

    device =
      if is_binary(device_identifier), do: Aprsme.DeviceIdentification.lookup_device_by_identifier(device_identifier)

    model = if device, do: device.model
    vendor = if device, do: device.vendor
    contact = if device, do: device.contact
    class = if device, do: device.class

    packet
    |> Map.put(:device_model, model)
    |> Map.put(:device_vendor, vendor)
    |> Map.put(:device_contact, contact)
    |> Map.put(:device_class, class)
  end

  defp get_neighbors(nil, _callsign), do: []

  defp get_neighbors(packet, callsign) do
    lat = packet.lat
    lon = packet.lon

    if is_nil(lat) or is_nil(lon) do
      []
    else
      # Simple bounding box for ~10km radius
      delta = @neighbor_radius_km / 111.0
      min_lat = lat - delta
      max_lat = lat + delta
      min_lon = lon - delta
      max_lon = lon + delta
      opts = %{bounds: [min_lon, min_lat, max_lon, max_lat], limit: 50}

      opts
      |> Packets.get_recent_packets()
      |> Enum.filter(fn p ->
        (p.sender != callsign and p.lat) && p.lon
      end)
      |> uniq_by(& &1.sender)
      |> Enum.map(fn p ->
        dist = haversine(lat, lon, p.lat, p.lon)
        course = calculate_course(lat, lon, p.lat, p.lon)

        %{
          callsign: p.sender,
          distance: format_distance(dist),
          course: course,
          last_heard: PacketUtils.get_timestamp(p),
          packet: p
        }
      end)
      |> Enum.sort_by(& &1.distance)
      |> Enum.take(@neighbor_limit)
    end
  end

  defp uniq_by(list, fun) do
    list
    |> Enum.reduce({MapSet.new(), []}, fn item, {set, acc} ->
      key = fun.(item)

      if MapSet.member?(set, key) do
        {set, acc}
      else
        {MapSet.put(set, key), [item | acc]}
      end
    end)
    |> elem(1)
    |> Enum.reverse()
  end

  defp haversine(lat1, lon1, lat2, lon2) do
    # Returns distance in km
    r = 6371
    dlat = :math.pi() / 180 * (lat2 - lat1)
    dlon = :math.pi() / 180 * (lon2 - lon1)

    a =
      :math.sin(dlat / 2) * :math.sin(dlat / 2) +
        :math.cos(:math.pi() / 180 * lat1) * :math.cos(:math.pi() / 180 * lat2) *
          :math.sin(dlon / 2) * :math.sin(dlon / 2)

    c = 2 * :math.atan2(:math.sqrt(a), :math.sqrt(1 - a))
    r * c
  end

  defp format_distance(km) when km < 1.0 do
    "#{Float.round(km * 1000, 0)} m"
  end

  defp format_distance(km) do
    "#{Float.round(km, 2)} km"
  end

  defp calculate_course(lat1, lon1, lat2, lon2) do
    # Calculate bearing from point 1 to point 2
    dlon = :math.pi() / 180 * (lon2 - lon1)

    lat1_rad = :math.pi() / 180 * lat1
    lat2_rad = :math.pi() / 180 * lat2

    y = :math.sin(dlon) * :math.cos(lat2_rad)
    x = :math.cos(lat1_rad) * :math.sin(lat2_rad) - :math.sin(lat1_rad) * :math.cos(lat2_rad) * :math.cos(dlon)

    bearing = :math.atan2(y, x) * 180 / :math.pi()
    # Convert to 0-360 range
    if bearing < 0, do: bearing + 360, else: bearing
  end
end
