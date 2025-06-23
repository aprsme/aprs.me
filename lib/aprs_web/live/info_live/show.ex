defmodule AprsWeb.InfoLive.Show do
  @moduledoc false
  use AprsWeb, :live_view

  alias Aprs.Packets
  alias AprsWeb.MapLive.PacketUtils

  @neighbor_radius_km 10
  @neighbor_limit 10

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = String.upcase(String.trim(callsign))
    packet = get_latest_packet(normalized_callsign)
    neighbors = get_neighbors(packet, normalized_callsign)

    socket =
      socket
      |> assign(:callsign, normalized_callsign)
      |> assign(:packet, packet)
      |> assign(:neighbors, neighbors)
      |> assign(:page_title, "APRS station #{normalized_callsign}")

    {:ok, socket}
  end

  defp get_latest_packet(callsign) do
    %{callsign: callsign, limit: 1}
    |> Packets.get_recent_packets()
    |> List.first()
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

        %{
          callsign: p.sender,
          distance: format_distance(dist),
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
end
