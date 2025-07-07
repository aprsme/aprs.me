defmodule AprsmeWeb.InfoLive.Show do
  @moduledoc false
  use AprsmeWeb, :live_view
  use Gettext, backend: AprsmeWeb.Gettext

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
    other_ssids = get_other_ssids(normalized_callsign)

    socket =
      socket
      |> assign(:callsign, normalized_callsign)
      |> assign(:packet, packet)
      |> assign(:neighbors, neighbors)
      |> assign(:page_title, "APRS station #{normalized_callsign}")
      |> assign(:has_weather_packets, has_weather_packets)
      |> assign(:other_ssids, other_ssids)

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
      other_ssids = get_other_ssids(socket.assigns.callsign)

      socket =
        socket
        |> assign(:packet, packet)
        |> assign(:neighbors, neighbors)
        |> assign(:has_weather_packets, has_weather_packets)
        |> assign(:other_ssids, other_ssids)

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
    # Get the most recent packet for this callsign, regardless of type
    # This ensures we show the most recent activity, not just position packets
    Packets.get_latest_packet_for_callsign(callsign)
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
          last_heard: format_timestamp_for_display(p),
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
    # Convert Decimal to float if needed
    lat1 = to_float(lat1)
    lon1 = to_float(lon1)
    lat2 = to_float(lat2)
    lon2 = to_float(lon2)

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

  defp to_float(%Decimal{} = decimal), do: Decimal.to_float(decimal)
  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_integer(value), do: value * 1.0

  defp to_float(value) when is_binary(value) do
    case Float.parse(value) do
      {f, _} -> f
      :error -> 0.0
    end
  end

  defp to_float(_), do: 0.0

  defp format_timestamp_for_display(packet) do
    received_at = get_received_at(packet)

    if received_at do
      timestamp_dt = AprsmeWeb.TimeHelpers.to_datetime(received_at)

      if timestamp_dt do
        %{
          time_ago: AprsmeWeb.TimeHelpers.time_ago_in_words(timestamp_dt),
          formatted: Calendar.strftime(timestamp_dt, "%Y-%m-%d %H:%M:%S UTC")
        }
      else
        %{
          time_ago: gettext("Unknown"),
          formatted: ""
        }
      end
    else
      %{
        time_ago: gettext("Unknown"),
        formatted: ""
      }
    end
  end

  defp get_received_at(packet) do
    cond do
      Map.has_key?(packet, :received_at) -> packet.received_at
      Map.has_key?(packet, "received_at") -> packet["received_at"]
      true -> nil
    end
  end

  defp format_distance(km) when km < 1.0 do
    "#{Float.round(km * 1000, 0)} m"
  end

  defp format_distance(km) do
    "#{Float.round(km, 2)} km"
  end

  defp calculate_course(lat1, lon1, lat2, lon2) do
    # Calculate bearing from point 1 to point 2
    # Convert Decimal to float if needed
    lat1 = to_float(lat1)
    lon1 = to_float(lon1)
    lat2 = to_float(lat2)
    lon2 = to_float(lon2)

    dlon = :math.pi() / 180 * (lon2 - lon1)

    lat1_rad = :math.pi() / 180 * lat1
    lat2_rad = :math.pi() / 180 * lat2

    y = :math.sin(dlon) * :math.cos(lat2_rad)
    x = :math.cos(lat1_rad) * :math.sin(lat2_rad) - :math.sin(lat1_rad) * :math.cos(lat2_rad) * :math.cos(dlon)

    bearing = :math.atan2(y, x) * 180 / :math.pi()
    # Convert to 0-360 range
    if bearing < 0, do: bearing + 360, else: bearing
  end

  defp get_other_ssids(callsign) do
    import Ecto.Query

    alias Aprsme.Packet
    alias Aprsme.Repo
    # Extract base callsign from the full callsign (remove SSID if present)
    base_callsign = extract_base_callsign(callsign)

    # Query directly for packets with the same base_callsign

    # Get recent packets for the base callsign to find other SSIDs
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    query =
      from p in Packet,
        where: p.base_callsign == ^base_callsign,
        where: p.received_at >= ^one_hour_ago,
        order_by: [desc: p.received_at],
        limit: 100

    query
    |> Repo.all()
    |> Enum.map(fn p ->
      %{
        callsign: p.sender,
        ssid: p.ssid,
        last_heard: format_timestamp_for_display(p),
        packet: p
      }
    end)
    |> uniq_by(& &1.callsign)
    |> Enum.filter(fn ssid_info -> ssid_info.callsign != callsign end)
    |> Enum.sort_by(& &1.last_heard, :desc)
    |> Enum.take(10)
  end

  defp extract_base_callsign(callsign) do
    case String.split(callsign, "-") do
      [base, _ssid] -> base
      [base] -> base
    end
  end

  defp decode_aprs_path(path) when is_binary(path) and path != "" do
    path_elements = String.split(path, ",")

    decoded_elements = Enum.map(path_elements, &decode_path_element/1)

    # Filter out nil results and join with explanations
    decoded_elements
    |> Enum.reject(&is_nil/1)
    |> Enum.join(" → ")
  end

  defp decode_aprs_path(_), do: nil

  defp decode_path_element(element) do
    element = String.trim(element)

    cond do
      # WIDE digipeaters
      String.starts_with?(element, "WIDE") ->
        case element do
          "WIDE1-1" -> gettext("WIDE1-1 (Wide area digipeater, 1 hop)")
          "WIDE2-1" -> gettext("WIDE2-1 (Wide area digipeater, 2 hops)")
          "WIDE3-1" -> gettext("WIDE3-1 (Wide area digipeater, 3 hops)")
          "WIDE4-1" -> gettext("WIDE4-1 (Wide area digipeater, 4 hops)")
          "WIDE5-1" -> gettext("WIDE5-1 (Wide area digipeater, 5 hops)")
          "WIDE6-1" -> gettext("WIDE6-1 (Wide area digipeater, 6 hops)")
          "WIDE7-1" -> gettext("WIDE7-1 (WIDE area digipeater, 7 hops)")
          "WIDE1-2" -> gettext("WIDE1-2 (Wide area digipeater, 1 hop, 2nd attempt)")
          "WIDE2-2" -> gettext("WIDE2-2 (Wide area digipeater, 2 hops, 2nd attempt)")
          _ -> gettext("WIDE digipeater (%{element})", element: element)
        end

      # TRACE digipeaters
      String.starts_with?(element, "TRACE") ->
        case element do
          "TRACE1-1" -> gettext("TRACE1-1 (Trace digipeater, 1 hop)")
          "TRACE2-1" -> gettext("TRACE2-1 (Trace digipeater, 2 hops)")
          "TRACE3-1" -> gettext("TRACE3-1 (Trace digipeater, 3 hops)")
          "TRACE4-1" -> gettext("TRACE4-1 (Trace digipeater, 4 hops)")
          "TRACE5-1" -> gettext("TRACE5-1 (Trace digipeater, 5 hops)")
          "TRACE6-1" -> gettext("TRACE6-1 (Trace digipeater, 6 hops)")
          "TRACE7-1" -> gettext("TRACE7-1 (Trace digipeater, 7 hops)")
          _ -> gettext("TRACE digipeater (%{element})", element: element)
        end

      # RELAY digipeaters
      String.starts_with?(element, "RELAY") ->
        case element do
          "RELAY" -> gettext("RELAY (Relay digipeater)")
          "RELAY-1" -> gettext("RELAY-1 (Relay digipeater, 1 hop)")
          "RELAY-2" -> gettext("RELAY-2 (Relay digipeater, 2 hops)")
          _ -> gettext("RELAY digipeater (%{element})", element: element)
        end

      # qAC (APRS-IS connection)
      element == "qAC" ->
        "qAC (APRS-IS connection)"

      # qAO (APRS-IS origin)
      element == "qAO" ->
        "qAO (APRS-IS origin)"

      # qAR (APRS-IS relay)
      element == "qAR" ->
        "qAR (APRS-IS relay)"

      # qAS (APRS-IS server)
      element == "qAS" ->
        "qAS (APRS-IS server)"

      # qAX (APRS-IS client)
      element == "qAX" ->
        "qAX (APRS-IS client)"

      # qAY (APRS-IS gateway)
      element == "qAY" ->
        "qAY (APRS-IS gateway)"

      # qAZ (APRS-IS zone)
      element == "qAZ" ->
        "qAZ (APRS-IS zone)"

      # qBU (APRS-IS user)
      element == "qBU" ->
        "qBU (APRS-IS user)"

      # qBV (APRS-IS vendor)
      element == "qBV" ->
        "qBV (APRS-IS vendor)"

      # qBW (APRS-IS web)
      element == "qBW" ->
        "qBW (APRS-IS web)"

      # qBX (APRS-IS experimental)
      element == "qBX" ->
        "qBX (APRS-IS experimental)"

      # qBY (APRS-IS Y2K)
      element == "qBY" ->
        "qBY (APRS-IS Y2K)"

      # qBZ (APRS-IS Zulu)
      element == "qBZ" ->
        "qBZ (APRS-IS Zulu)"

      # qCA (APRS-IS client application)
      element == "qCA" ->
        "qCA (APRS-IS client application)"

      # qCB (APRS-IS client browser)
      element == "qCB" ->
        "qCB (APRS-IS client browser)"

      # qCC (APRS-IS client console)
      element == "qCC" ->
        "qCC (APRS-IS client console)"

      # qCD (APRS-IS client daemon)
      element == "qCD" ->
        "qCD (APRS-IS client daemon)"

      # qCE (APRS-IS client editor)
      element == "qCE" ->
        "qCE (APRS-IS client editor)"

      # qCF (APRS-IS client filter)
      element == "qCF" ->
        "qCF (APRS-IS client filter)"

      # qCG (APRS-IS client gateway)
      element == "qCG" ->
        "qCG (APRS-IS client gateway)"

      # qCH (APRS-IS client host)
      element == "qCH" ->
        "qCH (APRS-IS client host)"

      # qCI (APRS-IS client interface)
      element == "qCI" ->
        "qCI (APRS-IS client interface)"

      # qCJ (APRS-IS client java)
      element == "qCJ" ->
        "qCJ (APRS-IS client java)"

      # qCK (APRS-IS client kernel)
      element == "qCK" ->
        "qCK (APRS-IS client kernel)"

      # qCL (APRS-IS client library)
      element == "qCL" ->
        "qCL (APRS-IS client library)"

      # qCM (APRS-IS client module)
      element == "qCM" ->
        "qCM (APRS-IS client module)"

      # qCN (APRS-IS client network)
      element == "qCN" ->
        "qCN (APRS-IS client network)"

      # qCO (APRS-IS client object)
      element == "qCO" ->
        "qCO (APRS-IS client object)"

      # qCP (APRS-IS client protocol)
      element == "qCP" ->
        "qCP (APRS-IS client protocol)"

      # qCQ (APRS-IS client query)
      element == "qCQ" ->
        "qCQ (APRS-IS client query)"

      # qCR (APRS-IS client router)
      element == "qCR" ->
        "qCR (APRS-IS client router)"

      # qCS (APRS-IS client server)
      element == "qCS" ->
        "qCS (APRS-IS client server)"

      # qCT (APRS-IS client terminal)
      element == "qCT" ->
        "qCT (APRS-IS client terminal)"

      # qCU (APRS-IS client user)
      element == "qCU" ->
        "qCU (APRS-IS client user)"

      # qCV (APRS-IS client vendor)
      element == "qCV" ->
        "qCV (APRS-IS client vendor)"

      # qCW (APRS-IS client web)
      element == "qCW" ->
        "qCW (APRS-IS client web)"

      # qCX (APRS-IS client experimental)
      element == "qCX" ->
        "qCX (APRS-IS client experimental)"

      # qCY (APRS-IS client Y2K)
      element == "qCY" ->
        "qCY (APRS-IS client Y2K)"

      # qCZ (APRS-IS client Zulu)
      element == "qCZ" ->
        "qCZ (APRS-IS client Zulu)"

      # TCPIP digipeaters
      String.starts_with?(element, "TCPIP") ->
        case element do
          "TCPIP" -> gettext("TCPIP (Internet gateway)")
          "TCPIP*" -> gettext("TCPIP* (Internet gateway, no forward)")
          _ -> gettext("TCPIP gateway (%{element})", element: element)
        end

      # Generic callsign with SSID (likely a digipeater)
      Regex.match?(~r/^[A-Z0-9]+-\d+$/, element) ->
        gettext("%{element} (Digipeater)", element: element)

      # Generic callsign without SSID
      Regex.match?(~r/^[A-Z0-9]+$/, element) ->
        gettext("%{element} (Station)", element: element)

      # Default case
      true ->
        gettext("%{element} (Unknown)", element: element)
    end
  end
end
