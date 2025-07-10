defmodule Aprsme.Packets.QueryBuilder do
  @moduledoc """
  Query builder functions for composing Packet queries.
  Provides reusable, composable functions for common query patterns.
  """

  import Ecto.Query

  alias Aprsme.Packet

  @doc """
  Filters query by time range. Supports start_time, end_time, and hours_back options.

  ## Options
    * `:start_time` - DateTime to filter packets after
    * `:end_time` - DateTime to filter packets before
    * `:hours_back` - Alternative to start_time, filters packets from X hours ago
  """
  @spec with_time_range(Ecto.Query.t(), map() | keyword()) :: Ecto.Query.t()
  def with_time_range(query, opts) when is_list(opts) do
    with_time_range(query, Map.new(opts))
  end

  def with_time_range(query, opts) when is_map(opts) do
    query
    |> maybe_filter_start_time(opts[:start_time] || opts["start_time"])
    |> maybe_filter_end_time(opts[:end_time] || opts["end_time"])
    |> maybe_filter_hours_back(opts[:hours_back] || opts["hours_back"])
  end

  @doc """
  Filters query to only include packets with position data.
  """
  @spec with_position(Ecto.Query.t()) :: Ecto.Query.t()
  def with_position(query) do
    from p in query, where: p.has_position == true
  end

  @doc """
  Filters query by exact callsign match (case-insensitive).
  """
  @spec for_callsign(Ecto.Query.t(), String.t()) :: Ecto.Query.t()
  def for_callsign(query, callsign) when is_binary(callsign) do
    normalized = String.upcase(String.trim(callsign))
    from p in query, where: ilike(p.sender, ^normalized)
  end

  @doc """
  Filters query by base callsign (without SSID).
  """
  @spec for_base_callsign(Ecto.Query.t(), String.t()) :: Ecto.Query.t()
  def for_base_callsign(query, base_callsign) when is_binary(base_callsign) do
    from p in query, where: p.base_callsign == ^base_callsign
  end

  @doc """
  Orders query by received_at descending (most recent first).
  """
  @spec recent_first(Ecto.Query.t()) :: Ecto.Query.t()
  def recent_first(query) do
    from p in query, order_by: [desc: p.received_at]
  end

  @doc """
  Orders query by received_at ascending (oldest first).
  """
  @spec chronological(Ecto.Query.t()) :: Ecto.Query.t()
  def chronological(query) do
    from p in query, order_by: [asc: p.received_at]
  end

  @doc """
  Filters query to only weather packets.
  """
  @spec weather_only(Ecto.Query.t()) :: Ecto.Query.t()
  def weather_only(query) do
    from p in query,
      where: p.data_type == "weather" or (p.symbol_table_id == "/" and p.symbol_code == "_")
  end

  @doc """
  Applies pagination with limit and optional offset.
  """
  @spec paginate(Ecto.Query.t(), integer(), integer()) :: Ecto.Query.t()
  def paginate(query, limit, offset \\ 0) when is_integer(limit) and is_integer(offset) do
    from p in query, limit: ^limit, offset: ^offset
  end

  @doc """
  Adds PostGIS coordinate extraction to select clause.
  """
  @spec with_coordinates(Ecto.Query.t()) :: Ecto.Query.t()
  def with_coordinates(query) do
    from p in query,
      select: %{p | lat: fragment("ST_Y(?)", p.location), lon: fragment("ST_X(?)", p.location)}
  end

  @doc """
  Filters by region if provided in options.
  """
  @spec maybe_filter_region(Ecto.Query.t(), map() | keyword()) :: Ecto.Query.t()
  def maybe_filter_region(query, opts) when is_list(opts) do
    maybe_filter_region(query, Map.new(opts))
  end

  def maybe_filter_region(query, %{region: region}) when not is_nil(region) do
    from p in query, where: p.region == ^region
  end

  def maybe_filter_region(query, _), do: query

  @doc """
  Filters by map bounds using bounding box coordinates.

  ## Example
      
      within_bounds(query, [-74.0, 40.0, -73.0, 41.0])
      # [west, south, east, north]
  """
  @spec within_bounds(Ecto.Query.t(), list(number())) :: Ecto.Query.t()
  def within_bounds(query, [west, south, east, north])
      when is_number(west) and is_number(south) and is_number(east) and is_number(north) do
    from p in query,
      where: p.has_position == true,
      where: p.lat >= ^south and p.lat <= ^north,
      where: p.lon >= ^west and p.lon <= ^east
  end

  def within_bounds(query, _), do: query

  @doc """
  Common query composition for recent position packets.

  ## Options
    * `:limit` - Number of packets to return (default: 100)
    * `:start_time` - Filter packets after this time
    * `:end_time` - Filter packets before this time
    * `:region` - Filter by region
  """
  @spec recent_position_packets(map() | keyword()) :: Ecto.Query.t()
  def recent_position_packets(opts \\ %{}) do
    limit = opts[:limit] || opts["limit"] || 100

    Packet
    |> with_position()
    |> with_time_range(opts)
    |> maybe_filter_region(opts)
    |> recent_first()
    |> paginate(limit)
    |> with_coordinates()
  end

  @doc """
  Common query composition for callsign packet history.

  ## Options
    * `:limit` - Number of packets to return (default: 100)
    * `:start_time` - Filter packets after this time
    * `:end_time` - Filter packets before this time
  """
  @spec callsign_history(String.t(), map() | keyword()) :: Ecto.Query.t()
  def callsign_history(callsign, opts \\ %{}) do
    limit = opts[:limit] || opts["limit"] || 100

    Packet
    |> for_callsign(callsign)
    |> with_time_range(opts)
    |> recent_first()
    |> paginate(limit)
  end

  @doc """
  Common query composition for weather packets.

  ## Options
    * `:callsign` - Filter by specific callsign
    * `:limit` - Number of packets to return (default: 100)
    * `:start_time` - Filter packets after this time
    * `:end_time` - Filter packets before this time
  """
  @spec weather_packets(map() | keyword()) :: Ecto.Query.t()
  def weather_packets(opts \\ %{}) do
    limit = opts[:limit] || opts["limit"] || 100
    callsign = opts[:callsign] || opts["callsign"]

    query =
      Packet
      |> weather_only()
      |> with_time_range(opts)
      |> recent_first()
      |> paginate(limit)

    if callsign do
      for_callsign(query, callsign)
    else
      query
    end
  end

  # Private helper functions

  defp maybe_filter_start_time(query, nil), do: query

  defp maybe_filter_start_time(query, start_time) do
    from p in query, where: p.received_at >= ^start_time
  end

  defp maybe_filter_end_time(query, nil), do: query

  defp maybe_filter_end_time(query, end_time) do
    from p in query, where: p.received_at <= ^end_time
  end

  defp maybe_filter_hours_back(query, nil), do: query

  defp maybe_filter_hours_back(query, hours) when is_number(hours) do
    start_time = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)
    from p in query, where: p.received_at >= ^start_time
  end
end
