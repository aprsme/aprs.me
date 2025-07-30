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
end
