defmodule AprsmeWeb.InfoLive.Show do
  @moduledoc false
  use AprsmeWeb, :live_view
  use Gettext, backend: AprsmeWeb.Gettext

  import AprsmeWeb.Components.InfoMapComponent
  import Phoenix.HTML, only: [raw: 1]

  alias Aprsme.Callsign
  alias Aprsme.EncodingUtils
  alias Aprsme.Packets
  alias AprsmeWeb.AprsSymbol
  alias AprsmeWeb.Live.SharedPacketHandler
  alias AprsmeWeb.MapLive.PacketUtils
  alias AprsmeWeb.TimeUtils

  @neighbor_limit 10

  # APRS Q-construct descriptions (APRS-IS codes)
  @q_constructs %{
    "qAC" => "qAC (APRS-IS connection)",
    "qAO" => "qAO (APRS-IS origin)",
    "qAR" => "qAR (APRS-IS relay)",
    "qAS" => "qAS (APRS-IS server)",
    "qAX" => "qAX (APRS-IS client)",
    "qAY" => "qAY (APRS-IS gateway)",
    "qAZ" => "qAZ (APRS-IS zone)",
    "qBU" => "qBU (APRS-IS user)",
    "qBV" => "qBV (APRS-IS vendor)",
    "qBW" => "qBW (APRS-IS web)",
    "qBX" => "qBX (APRS-IS experimental)",
    "qBY" => "qBY (APRS-IS Y2K)",
    "qBZ" => "qBZ (APRS-IS Zulu)",
    "qCA" => "qCA (APRS-IS client application)",
    "qCB" => "qCB (APRS-IS client browser)",
    "qCC" => "qCC (APRS-IS client console)",
    "qCD" => "qCD (APRS-IS client daemon)",
    "qCE" => "qCE (APRS-IS client editor)",
    "qCF" => "qCF (APRS-IS client filter)",
    "qCG" => "qCG (APRS-IS client gateway)",
    "qCH" => "qCH (APRS-IS client host)",
    "qCI" => "qCI (APRS-IS client interface)",
    "qCJ" => "qCJ (APRS-IS client java)",
    "qCK" => "qCK (APRS-IS client kernel)",
    "qCL" => "qCL (APRS-IS client library)",
    "qCM" => "qCM (APRS-IS client module)",
    "qCN" => "qCN (APRS-IS client network)",
    "qCO" => "qCO (APRS-IS client object)",
    "qCP" => "qCP (APRS-IS client protocol)",
    "qCQ" => "qCQ (APRS-IS client query)",
    "qCR" => "qCR (APRS-IS client router)",
    "qCS" => "qCS (APRS-IS client server)",
    "qCT" => "qCT (APRS-IS client terminal)",
    "qCU" => "qCU (APRS-IS client user)",
    "qCV" => "qCV (APRS-IS client vendor)",
    "qCW" => "qCW (APRS-IS client web)",
    "qCX" => "qCX (APRS-IS client experimental)",
    "qCY" => "qCY (APRS-IS client Y2K)",
    "qCZ" => "qCZ (APRS-IS client Zulu)"
  }

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = Callsign.normalize(callsign)

    # Subscribe to callsign-specific topic for live updates
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "packets:#{normalized_callsign}")
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
    # Since we're subscribed to callsign-specific topic, no need to filter
    # The packet from PostgreSQL notify already contains all fields
    process_packet_update(packet, socket)
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp process_packet_update(incoming_packet, socket) do
    # Log for debugging
    require Logger

    Logger.debug(
      "InfoLive received packet update for #{socket.assigns.callsign}: #{inspect(Map.get(incoming_packet, "raw_packet"))}"
    )

    # Get the new packet data
    new_packet = get_latest_packet(socket.assigns.callsign)
    new_packet = if new_packet, do: SharedPacketHandler.enrich_with_device_info(new_packet)

    current_packet = socket.assigns.packet

    # Check if this is a position update by comparing location and other key fields
    position_changed = position_changed?(current_packet, new_packet)

    if position_changed do
      # Only update position-related data if position changed
      # Get locale from socket assigns
      locale = Map.get(socket.assigns, :locale, "en")
      neighbors = get_neighbors(new_packet, socket.assigns.callsign, locale)

      socket =
        socket
        |> assign(:packet, new_packet)
        |> assign(:neighbors, neighbors)

      {:noreply, socket}
    else
      # Just update the packet data (for timestamp, comment, etc.) without affecting neighbors
      socket = assign(socket, :packet, new_packet)

      {:noreply, socket}
    end
  end

  defp position_changed?(nil, _new_packet), do: true
  defp position_changed?(_current_packet, nil), do: false

  defp position_changed?(current_packet, new_packet) do
    # Compare lat/lon to see if position actually changed
    current_lat = to_float_safe(current_packet.lat)
    current_lon = to_float_safe(current_packet.lon)
    new_lat = to_float_safe(new_packet.lat)
    new_lon = to_float_safe(new_packet.lon)

    # If any coordinate is invalid, consider it a change
    case {current_lat, current_lon, new_lat, new_lon} do
      {nil, _, _, _} ->
        true

      {_, nil, _, _} ->
        true

      {_, _, nil, _} ->
        true

      {_, _, _, nil} ->
        true

      {curr_lat, curr_lon, new_lat, new_lon} ->
        # Consider position changed if coordinates differ by more than the configured threshold
        threshold = Application.get_env(:aprsme, :position_tracking, [])[:change_threshold] || 0.001
        lat_diff = abs(curr_lat - new_lat)
        lon_diff = abs(curr_lon - new_lon)
        lat_diff > threshold or lon_diff > threshold
    end
  end

  # Expose for testing
  if Mix.env() == :test do
    def position_changed_for_test(current, new), do: position_changed?(current, new)
  end

  defp get_latest_packet(callsign) do
    # Get the most recent packet for this callsign, regardless of type
    # This ensures we show the most recent activity, not just position packets
    packet = Packets.get_latest_packet_for_callsign(callsign)

    # If this packet doesn't have position data, try to get the latest position packet
    if packet && (is_nil(packet.lat) || is_nil(packet.lon)) do
      position_packet = get_latest_position_packet(callsign)

      if position_packet do
        # Merge position data from position packet into the latest packet
        %{packet | lat: position_packet.lat, lon: position_packet.lon, location: position_packet.location}
      else
        packet
      end
    else
      packet
    end
  end

  defp get_latest_position_packet(callsign) do
    import Ecto.Query
    # Get the most recent packet with valid position data
    alias Aprsme.Repo

    Repo.one(
      from(p in Aprsme.Packet,
        where: fragment("upper(?)", p.sender) == ^String.upcase(String.trim(callsign)),
        where: not is_nil(p.lat) and not is_nil(p.lon),
        order_by: [desc: p.received_at],
        limit: 1,
        select: %{p | lat: fragment("ST_Y(?)", p.location), lon: fragment("ST_X(?)", p.location)}
      )
    )
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

  # Safe float conversion that preserves nil for invalid coordinates
  defp to_float_safe(value), do: EncodingUtils.to_float(value)

  # Legacy float conversion that defaults to 0.0 for backward compatibility
  defp to_float(value), do: EncodingUtils.to_float(value) || 0.0

  defp format_timestamp_for_display(packet) do
    received_at = get_received_at(packet)

    if received_at do
      timestamp_dt = AprsmeWeb.TimeHelpers.to_datetime(received_at)

      if timestamp_dt do
        %{
          time_ago: AprsmeWeb.TimeHelpers.time_ago_in_words(timestamp_dt),
          formatted: Calendar.strftime(timestamp_dt, "%Y-%m-%d %H:%M:%S UTC"),
          timestamp: DateTime.to_iso8601(timestamp_dt)
        }
      else
        %{
          time_ago: gettext("Unknown"),
          formatted: "",
          timestamp: nil
        }
      end
    else
      %{
        time_ago: gettext("Unknown"),
        formatted: "",
        timestamp: nil
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
        sender, ssid, received_at, id, symbol_table_id, symbol_code
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
        Enum.map(result.rows, fn [sender, ssid, received_at, id, symbol_table_id, symbol_code] ->
          # Create a minimal packet struct for display
          packet = %Packet{
            id: id,
            sender: sender,
            ssid: ssid,
            received_at: received_at,
            symbol_table_id: symbol_table_id,
            symbol_code: symbol_code
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
      # Check Q-constructs map first (most common)
      Map.has_key?(@q_constructs, element) ->
        @q_constructs[element]

      # WIDE digipeaters
      String.starts_with?(element, "WIDE") ->
        decode_wide_element(element)

      # TRACE digipeaters
      String.starts_with?(element, "TRACE") ->
        decode_trace_element(element)

      # RELAY digipeaters
      String.starts_with?(element, "RELAY") ->
        decode_relay_element(element)

      # TCPIP digipeaters
      String.starts_with?(element, "TCPIP") ->
        decode_tcpip_element(element)

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

  defp decode_wide_element("WIDE1-1"), do: gettext("WIDE1-1 (Wide area digipeater, 1 hop)")
  defp decode_wide_element("WIDE2-1"), do: gettext("WIDE2-1 (Wide area digipeater, 2 hops)")
  defp decode_wide_element("WIDE3-1"), do: gettext("WIDE3-1 (Wide area digipeater, 3 hops)")
  defp decode_wide_element("WIDE4-1"), do: gettext("WIDE4-1 (Wide area digipeater, 4 hops)")
  defp decode_wide_element("WIDE5-1"), do: gettext("WIDE5-1 (Wide area digipeater, 5 hops)")
  defp decode_wide_element("WIDE6-1"), do: gettext("WIDE6-1 (Wide area digipeater, 6 hops)")
  defp decode_wide_element("WIDE7-1"), do: gettext("WIDE7-1 (WIDE area digipeater, 7 hops)")
  defp decode_wide_element("WIDE1-2"), do: gettext("WIDE1-2 (Wide area digipeater, 1 hop, 2nd attempt)")
  defp decode_wide_element("WIDE2-2"), do: gettext("WIDE2-2 (Wide area digipeater, 2 hops, 2nd attempt)")
  defp decode_wide_element(element), do: gettext("WIDE digipeater (%{element})", element: element)

  defp decode_trace_element("TRACE1-1"), do: gettext("TRACE1-1 (Trace digipeater, 1 hop)")
  defp decode_trace_element("TRACE2-1"), do: gettext("TRACE2-1 (Trace digipeater, 2 hops)")
  defp decode_trace_element("TRACE3-1"), do: gettext("TRACE3-1 (Trace digipeater, 3 hops)")
  defp decode_trace_element("TRACE4-1"), do: gettext("TRACE4-1 (Trace digipeater, 4 hops)")
  defp decode_trace_element("TRACE5-1"), do: gettext("TRACE5-1 (Trace digipeater, 5 hops)")
  defp decode_trace_element("TRACE6-1"), do: gettext("TRACE6-1 (Trace digipeater, 6 hops)")
  defp decode_trace_element("TRACE7-1"), do: gettext("TRACE7-1 (Trace digipeater, 7 hops)")
  defp decode_trace_element(element), do: gettext("TRACE digipeater (%{element})", element: element)

  defp decode_relay_element("RELAY"), do: gettext("RELAY (Relay digipeater)")
  defp decode_relay_element("RELAY-1"), do: gettext("RELAY-1 (Relay digipeater, 1 hop)")
  defp decode_relay_element("RELAY-2"), do: gettext("RELAY-2 (Relay digipeater, 2 hops)")
  defp decode_relay_element(element), do: gettext("RELAY digipeater (%{element})", element: element)

  defp decode_tcpip_element("TCPIP"), do: gettext("TCPIP (Internet gateway)")
  defp decode_tcpip_element("TCPIP*"), do: gettext("TCPIP* (Internet gateway, no forward)")
  defp decode_tcpip_element(element), do: gettext("TCPIP gateway (%{element})", element: element)
end
