defmodule Aprsme.Packets.PreparedQueries do
  @moduledoc """
  High-performance queries using prepared statements and efficient patterns.
  These queries are designed for frequently executed operations.
  """

  import Ecto.Query

  alias Aprsme.Packet
  alias Aprsme.Repo

  @doc """
  Get the latest packet for a callsign using a prepared statement.
  This is one of the most frequently called queries.
  """
  @spec get_latest_packet_for_callsign(String.t()) :: Packet.t() | nil
  def get_latest_packet_for_callsign(callsign) when is_binary(callsign) do
    normalized = String.upcase(String.trim(callsign))

    Repo.one(
      from(p in Packet,
        where: fragment("upper(?)", p.sender) == ^normalized,
        order_by: [desc: p.received_at],
        limit: 1,
        select: %{p | lat: fragment("ST_Y(?)", p.location), lon: fragment("ST_X(?)", p.location)}
      )
    )
  end

  @doc """
  Get the latest full packet for each callsign in a single query.
  Returns a list of `Packet` structs with lat/lon populated.
  Uses DISTINCT ON to get only the most recent packet per callsign.
  """
  @spec get_latest_packets_for_callsigns(list(String.t())) :: [Packet.t()]
  def get_latest_packets_for_callsigns([]), do: []

  def get_latest_packets_for_callsigns(callsigns) when is_list(callsigns) do
    normalized = Enum.map(callsigns, &String.upcase(String.trim(&1)))

    Repo.all(
      from(p in Packet,
        where: fragment("upper(?)", p.sender) in ^normalized,
        distinct: fragment("upper(?)", p.sender),
        order_by: [asc: fragment("upper(?)", p.sender), desc: p.received_at],
        select: %{p | lat: fragment("ST_Y(?)", p.location), lon: fragment("ST_X(?)", p.location)}
      )
    )
  end

  @doc """
  Get latest positions for multiple callsigns in a single query.
  Returns a list of `%{callsign: String.t(), lat: float(), lng: float()}` maps.
  Uses DISTINCT ON to get only the most recent packet per callsign.
  """
  @spec get_latest_positions_for_callsigns(list(String.t())) :: [map()]
  def get_latest_positions_for_callsigns([]), do: []

  def get_latest_positions_for_callsigns(callsigns) when is_list(callsigns) do
    normalized = Enum.map(callsigns, &String.upcase(String.trim(&1)))

    Repo.all(
      from(p in Packet,
        where: fragment("upper(?)", p.sender) in ^normalized,
        where: not is_nil(p.location),
        distinct: fragment("upper(?)", p.sender),
        order_by: [asc: fragment("upper(?)", p.sender), desc: p.received_at],
        select: %{callsign: p.sender, lat: fragment("ST_Y(?)", p.location), lng: fragment("ST_X(?)", p.location)}
      )
    )
  end

  @doc """
  Get recent packets within bounds using prepared statement.
  """
  @spec get_recent_packets_in_bounds(list(), integer()) :: [Packet.t()]
  def get_recent_packets_in_bounds([north, south, east, west] = _bounds, hours_back \\ 1) do
    time_ago = DateTime.add(DateTime.utc_now(), -hours_back * 3600, :second)

    Repo.all(
      from(p in Packet,
        where: p.has_position == true,
        where: p.received_at >= ^time_ago,
        where: fragment("ST_Y(?) BETWEEN ? AND ?", p.location, ^south, ^north),
        where: fragment("ST_X(?) BETWEEN ? AND ?", p.location, ^west, ^east),
        order_by: [desc: p.received_at],
        limit: 500,
        select: %{p | lat: fragment("ST_Y(?)", p.location), lon: fragment("ST_X(?)", p.location)}
      )
    )
  end

  @doc """
  Check if callsign has weather packets using prepared statement.
  """
  @spec has_weather_packets?(String.t()) :: boolean()
  def has_weather_packets?(callsign) when is_binary(callsign) do
    normalized = String.upcase(String.trim(callsign))

    query =
      from(p in Packet,
        where: fragment("upper(?)", p.sender) == ^normalized,
        where:
          not is_nil(p.temperature) or
            not is_nil(p.humidity) or
            not is_nil(p.pressure) or
            not is_nil(p.wind_speed),
        limit: 1,
        select: true
      )

    Repo.exists?(query)
  end

  @doc """
  Check which callsigns from a list have weather packets.
  Returns a MapSet of callsigns (uppercased) that have weather data.
  Single query replaces N individual has_weather_packets? calls.
  """
  @spec weather_callsigns(list(String.t())) :: MapSet.t(String.t())
  def weather_callsigns(callsigns) when is_list(callsigns) do
    if callsigns == [] do
      MapSet.new()
    else
      normalized = Enum.map(callsigns, &String.upcase(String.trim(&1)))

      from(p in Packet,
        where: fragment("upper(?)", p.sender) in ^normalized,
        where: p.has_weather == true,
        distinct: fragment("upper(?)", p.sender),
        select: fragment("upper(?)", p.sender)
      )
      |> Repo.all()
      |> MapSet.new()
    end
  end

  @doc """
  Get nearby stations using KNN (K-nearest neighbors) search.
  Uses the <-> operator for efficient spatial queries.
  """
  @spec get_nearby_stations_knn(float(), float(), String.t() | nil, map()) :: [map()]
  def get_nearby_stations_knn(lat, lon, exclude_callsign \\ nil, opts \\ %{}) do
    limit = Map.get(opts, :limit, 10)
    hours_back = Map.get(opts, :hours_back, 1)
    cutoff_time = DateTime.add(DateTime.utc_now(), -hours_back * 3600, :second)

    # Build point for KNN search
    point = %Geo.Point{coordinates: {lon, lat}, srid: 4326}

    base_query =
      from(p in Packet,
        where: p.has_position == true,
        where: p.received_at >= ^cutoff_time,
        where: not is_nil(p.location),
        distinct: p.base_callsign,
        order_by: [
          asc: p.base_callsign,
          desc: p.received_at
        ]
      )

    query =
      if exclude_callsign do
        from(p in base_query,
          where: p.sender != ^exclude_callsign
        )
      else
        base_query
      end

    # Subquery to get most recent packet per callsign, then order by distance
    subquery =
      from(p in subquery(query),
        order_by: fragment("? <-> ?", p.location, ^point),
        limit: ^limit,
        select: %{
          callsign: p.sender,
          base_callsign: p.base_callsign,
          lat: fragment("ST_Y(?)", p.location),
          lon: fragment("ST_X(?)", p.location),
          distance: fragment("ST_Distance(?::geography, ?::geography)", p.location, ^point),
          received_at: p.received_at,
          symbol_table_id: p.symbol_table_id,
          symbol_code: p.symbol_code,
          comment: p.comment
        }
      )

    Repo.all(subquery)
  end

  @doc """
  Get packet count for an area using prepared statement.
  """
  @spec get_packet_count_in_area(list(), integer()) :: non_neg_integer()
  def get_packet_count_in_area([north, south, east, west], hours_back \\ 24) do
    time_ago = DateTime.add(DateTime.utc_now(), -hours_back * 3600, :second)

    query =
      from(p in Packet,
        where: p.has_position == true,
        where: p.received_at >= ^time_ago,
        where: fragment("ST_Y(?) BETWEEN ? AND ?", p.location, ^south, ^north),
        where: fragment("ST_X(?) BETWEEN ? AND ?", p.location, ^west, ^east),
        select: count(p.id)
      )

    Repo.one(query) || 0
  end

  @doc """
  Get nearby weather stations within a radius.

  Returns weather stations within the specified radius (in miles) that have reported
  weather data within the time window. Results are ordered by distance (closest first)
  and deduplicated by base_callsign to avoid duplicate SSIDs.

  ## Parameters

    * `lat` - Latitude of center point
    * `lon` - Longitude of center point
    * `radius_miles` - Search radius in miles
    * `opts` - Options keyword list
      * `:hours` - Time window in hours (default: 6)
      * `:limit` - Maximum number of results (default: 50)

  ## Returns

  List of maps with fields:
    * `callsign` - Station callsign with SSID
    * `base_callsign` - Base callsign without SSID
    * `lat` - Latitude
    * `lon` - Longitude
    * `distance_miles` - Distance from center point in miles
    * `temperature` - Temperature in Fahrenheit (may be nil)
    * `humidity` - Humidity percentage (may be nil)
    * `pressure` - Atmospheric pressure in hPa (may be nil)
    * `wind_speed` - Wind speed in MPH (may be nil)
    * `wind_direction` - Wind direction in degrees (may be nil)
    * `wind_gust` - Wind gust in MPH (may be nil)
    * `rain_1h` - Rain in last hour in inches (may be nil)
    * `rain_24h` - Rain in last 24 hours in inches (may be nil)
    * `rain_since_midnight` - Rain since midnight in inches (may be nil)
    * `symbol_table_id` - APRS symbol table ID
    * `symbol_code` - APRS symbol code
    * `comment` - Station comment
    * `received_at` - Time packet was received
  """
  @spec get_nearby_weather_stations(float(), float(), float(), keyword()) :: [map()]
  def get_nearby_weather_stations(lat, lon, radius_miles, opts \\ []) do
    hours = Keyword.get(opts, :hours, 6)
    limit = Keyword.get(opts, :limit, 50)

    # Convert miles to meters for PostGIS (1 mile = 1609.34 meters)
    radius_meters = radius_miles * 1609.34
    cutoff_time = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)

    # Build point for spatial query
    point = %Geo.Point{coordinates: {lon, lat}, srid: 4326}

    # Query: Find weather stations within radius and time window
    # Use DISTINCT ON base_callsign to deduplicate SSIDs (gets most recent)
    query =
      from(p in Packet,
        where: p.has_weather == true,
        where: p.has_position == true,
        where: p.received_at >= ^cutoff_time,
        where:
          fragment(
            "ST_DWithin(?::geography, ?::geography, ?)",
            p.location,
            ^point,
            ^radius_meters
          ),
        distinct: p.base_callsign,
        order_by: [asc: p.base_callsign, desc: p.received_at]
      )

    # Subquery to get most recent per base_callsign, then order by distance
    subquery =
      from(p in subquery(query),
        order_by: fragment("ST_Distance(?::geography, ?::geography)", p.location, ^point),
        limit: ^limit,
        select: %{
          callsign: p.sender,
          base_callsign: p.base_callsign,
          lat: fragment("ST_Y(?)", p.location),
          lon: fragment("ST_X(?)", p.location),
          distance_miles:
            fragment(
              "ST_Distance(?::geography, ?::geography) / 1609.34",
              p.location,
              ^point
            ),
          temperature: p.temperature,
          humidity: p.humidity,
          pressure: p.pressure,
          wind_speed: p.wind_speed,
          wind_direction: p.wind_direction,
          wind_gust: p.wind_gust,
          rain_1h: p.rain_1h,
          rain_24h: p.rain_24h,
          rain_since_midnight: p.rain_since_midnight,
          symbol_table_id: p.symbol_table_id,
          symbol_code: p.symbol_code,
          comment: p.comment,
          received_at: p.received_at
        }
      )

    Repo.all(subquery)
  end
end
