defmodule AprsmeWeb.InfoLive.Show do
  @moduledoc false
  use AprsmeWeb, :live_view
  use Gettext, backend: AprsmeWeb.Gettext

  import Phoenix.HTML, only: [raw: 1]
  import AprsmeWeb.Components.InfoMapComponent

  alias Aprsme.Callsign
  alias Aprsme.EncodingUtils
  alias Aprsme.Packets
  alias AprsmeWeb.AprsSymbol
  alias AprsmeWeb.Live.SharedPacketHandler
  alias AprsmeWeb.MapLive.PacketUtils
  alias AprsmeWeb.TimeUtils

  @neighbor_limit 10

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = Callsign.normalize(callsign)

    # Subscribe to Postgres notifications for live updates
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")
    end

    packet = get_latest_packet(normalized_callsign)
    packet = if packet, do: SharedPacketHandler.enrich_with_device_info(packet)
    # Get locale from socket assigns (set by LocaleHook)
    locale = Map.get(socket.assigns, :locale, "en")
    neighbors = get_neighbors(packet, normalized_callsign, locale)
    has_weather_packets = PacketUtils.has_weather_packets?(normalized_callsign)
    other_ssids = get_other_ssids(normalized_callsign)

    heard_by_stations = get_heard_by_stations(normalized_callsign, locale)
    stations_heard_by = get_stations_heard_by(normalized_callsign, locale)

    socket =
      socket
      |> assign(:callsign, normalized_callsign)
      |> assign(:packet, packet)
      |> assign(:neighbors, neighbors)
      |> assign(:page_title, "APRS station #{normalized_callsign}")
      |> assign(:has_weather_packets, has_weather_packets)
      |> assign(:other_ssids, other_ssids)
      |> assign(:heard_by_stations, heard_by_stations)
      |> assign(:stations_heard_by, stations_heard_by)

    {:ok, socket}
  end

  @impl true
  def handle_info({:postgres_packet, packet}, socket) do
    SharedPacketHandler.handle_packet_update(packet, socket,
      filter_fn: SharedPacketHandler.callsign_filter(socket.assigns.callsign),
      process_fn: &process_packet_update/2,
      enrich_packet: false
    )
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp process_packet_update(_packet, socket) do
    # Refresh data when new packet arrives
    packet = get_latest_packet(socket.assigns.callsign)
    packet = if packet, do: SharedPacketHandler.enrich_with_device_info(packet)
    # Get locale from socket assigns
    locale = Map.get(socket.assigns, :locale, "en")
    neighbors = get_neighbors(packet, socket.assigns.callsign, locale)
    has_weather_packets = PacketUtils.has_weather_packets?(socket.assigns.callsign)
    other_ssids = get_other_ssids(socket.assigns.callsign)
    heard_by_stations = get_heard_by_stations(socket.assigns.callsign, locale)
    stations_heard_by = get_stations_heard_by(socket.assigns.callsign, locale)

    socket =
      socket
      |> assign(:packet, packet)
      |> assign(:neighbors, neighbors)
      |> assign(:has_weather_packets, has_weather_packets)
      |> assign(:other_ssids, other_ssids)
      |> assign(:heard_by_stations, heard_by_stations)
      |> assign(:stations_heard_by, stations_heard_by)

    {:noreply, socket}
  end

  defp get_latest_packet(callsign) do
    # Get the most recent packet for this callsign, regardless of type
    # This ensures we show the most recent activity, not just position packets
    # Use cached version for better performance
    Aprsme.CachedQueries.get_latest_packet_for_callsign_cached(callsign)
  end

  defp get_neighbors(nil, _callsign, _locale), do: []

  defp get_neighbors(packet, callsign, locale) do
    lat = packet.lat
    lon = packet.lon

    if is_nil(lat) or is_nil(lon) do
      []
    else
      # Convert Decimal to float if needed
      lat_float = to_float(lat)
      lon_float = to_float(lon)

      # Use the spatial query to get the closest stations
      # The query already returns them sorted by distance
      nearby_packets =
        Packets.get_nearby_stations(
          lat_float,
          lon_float,
          callsign,
          %{limit: @neighbor_limit}
        )

      # Calculate distance and course for each neighbor
      # The packets are already sorted by distance from the database
      Enum.map(nearby_packets, fn p ->
        dist = haversine(lat, lon, p.lat, p.lon)
        course = calculate_course(lat, lon, p.lat, p.lon)

        %{
          callsign: p.sender,
          distance: format_distance(dist, locale),
          distance_km: dist,
          course: course,
          last_heard: format_timestamp_for_display(p),
          packet: p
        }
      end)
    end
  end

  def haversine(lat1, lon1, lat2, lon2) do
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

  defp to_float(value), do: EncodingUtils.to_float(value) || 0.0

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

  def format_distance(km, locale \\ "en") do
    case locale do
      "en" ->
        # Use imperial units for English locale
        miles = Aprsme.Convert.kph_to_mph(km)

        if miles < 1.0 do
          feet = miles * 5280
          "#{Float.round(feet, 0)} ft"
        else
          "#{Float.round(miles, 2)} mi"
        end

      _ ->
        # Use metric units for other locales
        if km < 1.0 do
          "#{Float.round(km * 1000, 0)} m"
        else
          "#{Float.round(km, 2)} km"
        end
    end
  end

  def calculate_course(lat1, lon1, lat2, lon2) do
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
    alias Aprsme.Packet
    alias Aprsme.Repo
    # Extract base callsign from the full callsign (remove SSID if present)
    base_callsign = extract_base_callsign(callsign)

    # Get recent packets for the base callsign to find other SSIDs
    one_hour_ago = TimeUtils.one_hour_ago()

    # Use a window function to get the most recent packet per sender
    # This is much more efficient than fetching 100 packets and filtering in Elixir
    query = """
    WITH recent_ssids AS (
      SELECT DISTINCT ON (sender)
        sender, ssid, received_at, id
      FROM packets
      WHERE base_callsign = $1
        AND received_at >= $2
        AND sender != $3
      ORDER BY sender, received_at DESC
    )
    SELECT * FROM recent_ssids
    ORDER BY received_at DESC
    LIMIT 10
    """

    case Repo.query(query, [base_callsign, one_hour_ago, callsign]) do
      {:ok, result} ->
        Enum.map(result.rows, fn [sender, ssid, received_at, id] ->
          # Create a minimal packet struct for display
          packet = %Packet{
            id: id,
            sender: sender,
            ssid: ssid,
            received_at: received_at
          }

          %{
            callsign: sender,
            ssid: ssid,
            last_heard: format_timestamp_for_display(packet),
            packet: packet
          }
        end)

      {:error, _} ->
        []
    end
  end

  defp extract_base_callsign(callsign) do
    Callsign.extract_base(callsign)
  end

  defp get_heard_by_stations(callsign, locale) do
    alias Aprsme.Repo

    # Get packets from the last month where this callsign was heard on RF
    one_month_ago = DateTime.add(DateTime.utc_now(), -30, :day)

    # Query to find stations that heard this callsign directly
    # In APRS, the first station with an asterisk (*) in the path is the one that heard the packet directly
    query = """
    WITH parsed_paths AS (
      SELECT
        id,
        sender,
        path,
        received_at,
        lat,
        lon,
        location,
        -- Extract the first digipeater from the path
        CASE
          WHEN path ~ '[A-Z0-9]+-?[0-9]*\\*' THEN
            substring(path from '([A-Z0-9]+-?[0-9]*)\\*')
          ELSE NULL
        END as first_digipeater
      FROM packets
      WHERE sender = $1
        AND received_at >= $2
        AND path IS NOT NULL
        AND path != ''
        AND path !~ '^TCPIP'
        AND path !~ ',TCPIP'
    ),
    digipeater_stats AS (
      SELECT
        first_digipeater as digipeater,
        MIN(received_at) as first_heard,
        MAX(received_at) as last_heard,
        COUNT(*) as packet_count,
        -- Find the packet with maximum distance for this digipeater
        (SELECT pp2.id
         FROM parsed_paths pp2
         WHERE pp2.first_digipeater = pp.first_digipeater
           AND pp2.lat IS NOT NULL
           AND pp2.lon IS NOT NULL
         ORDER BY
           ST_Distance(
             pp2.location::geography,
             (SELECT location::geography
              FROM packets
              WHERE sender = pp2.first_digipeater
                AND location IS NOT NULL
              ORDER BY received_at DESC
              LIMIT 1)
           ) DESC NULLS LAST
         LIMIT 1
        ) as longest_path_packet_id
      FROM parsed_paths pp
      WHERE first_digipeater IS NOT NULL
      GROUP BY first_digipeater
    )
    SELECT
      ds.digipeater,
      ds.first_heard,
      ds.last_heard,
      ds.packet_count,
      p.received_at as longest_path_time,
      CASE
        WHEN p.location IS NOT NULL AND dig_loc.location IS NOT NULL THEN
          ST_Distance(p.location::geography, dig_loc.location::geography) / 1000.0
        ELSE NULL
      END as longest_distance_km
    FROM digipeater_stats ds
    LEFT JOIN packets p ON p.id = ds.longest_path_packet_id
    LEFT JOIN LATERAL (
      SELECT location
      FROM packets
      WHERE sender = ds.digipeater
        AND location IS NOT NULL
      ORDER BY received_at DESC
      LIMIT 1
    ) dig_loc ON true
    ORDER BY ds.last_heard DESC
    LIMIT 50
    """

    case Repo.query(query, [callsign, one_month_ago]) do
      {:ok, result} ->
        result.rows
        |> Enum.map(fn row ->
          case row do
            [digipeater, first_heard, last_heard, packet_count, longest_path_time, longest_distance_km] ->
              %{
                digipeater: digipeater || "",
                first_heard: first_heard,
                last_heard: last_heard,
                packet_count: packet_count || 0,
                longest_distance_km: longest_distance_km,
                longest_distance: if(longest_distance_km, do: format_distance(longest_distance_km, locale)),
                longest_path_time: longest_path_time
              }

            _ ->
              nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      {:error, error} ->
        require Logger

        Logger.error("Error in get_heard_by_stations: #{inspect(error)}")
        []
    end
  end

  defp get_stations_heard_by(callsign, locale) do
    alias Aprsme.Repo

    # Get packets from the last month where this callsign heard other stations on RF
    one_month_ago = DateTime.add(DateTime.utc_now(), -30, :day)

    # Optimized query to find stations that were heard directly by this callsign
    # Eliminates correlated subqueries and reduces redundant operations
    query = """
    WITH digipeater_location AS (
      -- Get the most recent location for the digipeater (cached once)
      SELECT location::geography as dig_loc
      FROM packets
      WHERE sender = $1
        AND location IS NOT NULL
      ORDER BY received_at DESC
      LIMIT 1
    ),
    parsed_paths AS (
      SELECT
        id,
        sender,
        path,
        received_at,
        lat,
        lon,
        location,
        -- Extract the first digipeater from the path more efficiently
        CASE
          WHEN path ~ ($1 || '\\*') THEN
            substring(path from ($1 || '\\*'))
          ELSE NULL
        END as first_digipeater
      FROM packets
      WHERE path ~ ($1 || '\\*')
        AND received_at >= $2
        AND path IS NOT NULL
        AND path != ''
        AND path !~ '^TCPIP'
        AND path !~ ',TCPIP'
        AND lat IS NOT NULL
        AND lon IS NOT NULL
    ),
    station_stats AS (
      SELECT
        sender as station,
        MIN(received_at) as first_heard,
        MAX(received_at) as last_heard,
        COUNT(*) as packet_count
      FROM parsed_paths
      WHERE first_digipeater = $1
      GROUP BY sender
    ),
    longest_paths AS (
      -- Find the longest path packet for each station in a single pass
      SELECT DISTINCT ON (pp.sender)
        pp.sender,
        pp.id as longest_path_packet_id,
        pp.received_at as longest_path_time,
        CASE
          WHEN pp.location IS NOT NULL AND dl.dig_loc IS NOT NULL THEN
            ST_Distance(pp.location::geography, dl.dig_loc) / 1000.0
          ELSE NULL
        END as longest_distance_km
      FROM parsed_paths pp
      CROSS JOIN digipeater_location dl
      WHERE pp.first_digipeater = $1
      ORDER BY pp.sender,
               ST_Distance(pp.location::geography, dl.dig_loc) DESC NULLS LAST,
               pp.received_at DESC
    )
    SELECT
      ss.station,
      ss.first_heard,
      ss.last_heard,
      ss.packet_count,
      lp.longest_path_time,
      lp.longest_distance_km
    FROM station_stats ss
    LEFT JOIN longest_paths lp ON lp.sender = ss.station
    ORDER BY ss.last_heard DESC
    LIMIT 50
    """

    case Repo.query(query, [callsign, one_month_ago]) do
      {:ok, result} ->
        result.rows
        |> Enum.map(fn row ->
          case row do
            [station, first_heard, last_heard, packet_count, longest_path_time, longest_distance_km] ->
              %{
                station: station || "",
                first_heard: first_heard,
                last_heard: last_heard,
                packet_count: packet_count || 0,
                longest_distance_km: longest_distance_km,
                longest_distance: if(longest_distance_km, do: format_distance(longest_distance_km, locale)),
                longest_path_time: longest_path_time
              }

            _ ->
              nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      {:error, error} ->
        require Logger

        Logger.error("Error in get_stations_heard_by: #{inspect(error)}")
        []
    end
  end

  @doc """
  Renders an APRS symbol style for use in templates.
  """
  def render_symbol_style(packet, size \\ 32) do
    if packet do
      {symbol_table_id, symbol_code} = AprsSymbol.extract_from_packet(packet)
      AprsSymbol.render_style(symbol_table_id, symbol_code, size)
    else
      # Return empty style if no packet
      ""
    end
  end

  @doc """
  Renders an APRS symbol as HTML for overlay symbols that need proper overlay character display.
  """
  def render_symbol_html(packet, size \\ 32) do
    if packet do
      {symbol_table_id, symbol_code} = AprsSymbol.extract_from_packet(packet)

      # Check if this is an overlay symbol
      if symbol_table_id && String.match?(symbol_table_id, ~r/^[A-Z0-9]$/) do
        # Use layered sprite backgrounds for overlay symbols
        sprite_info = AprsSymbol.get_sprite_info(symbol_table_id, symbol_code)
        overlay_sprite_info = AprsSymbol.get_overlay_character_sprite_info(symbol_table_id)

        raw("""
        <div style="
          position: relative;
          width: #{size}px;
          height: #{size}px;
          background-image: url(#{overlay_sprite_info.sprite_file}), url(#{sprite_info.sprite_file});
          background-position: #{overlay_sprite_info.background_position}, #{sprite_info.background_position};
          background-size: #{overlay_sprite_info.background_size}, #{sprite_info.background_size};
          background-repeat: no-repeat, no-repeat;
          image-rendering: pixelated;
          display: inline-block;
          vertical-align: middle;
          margin-bottom: -6px;
        ">
        </div>
        """)
      else
        # Use style rendering for non-overlay symbols
        raw("""
        <div style="#{AprsSymbol.render_style(symbol_table_id, symbol_code, size)}"></div>
        """)
      end
    else
      # Return empty if no packet
      raw("")
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

  @doc """
  Parses the APRS path and creates linked callsigns where appropriate.
  Returns a list of tuples: {:text, string} or {:link, callsign, display_text}
  """
  def parse_path_with_links(path) when is_binary(path) and path != "" do
    path
    |> String.split(",")
    |> Enum.map(&parse_path_element_with_link/1)
  end

  def parse_path_with_links(_), do: []

  defp parse_path_element_with_link(element) do
    element = String.trim(element)

    # Check if it's a callsign (with or without SSID)
    if Regex.match?(~r/^[A-Z0-9]{1,6}(-\d{1,2})?(\*)?$/, element) and
         not String.starts_with?(element, "WIDE") and
         not String.starts_with?(element, "TRACE") and
         not String.starts_with?(element, "RELAY") and
         not String.starts_with?(element, "TCPIP") and
         not String.starts_with?(element, "q") do
      # Remove asterisk if present (indicates the packet was digipeated through this station)
      callsign = String.replace(element, "*", "")
      display = if String.ends_with?(element, "*"), do: "#{callsign}*", else: callsign
      {:link, callsign, display}
    else
      # Not a linkable callsign, just return as text
      {:text, element, element}
    end
  end

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
