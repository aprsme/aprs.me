defmodule Aprsme.CachedQueries do
  @moduledoc """
  Caching layer for database queries to improve performance
  """

  alias Aprsme.Packet
  alias Aprsme.Packets
  alias Aprsme.Repo
  alias Ecto.Adapters.SQL

  # 1 minute for frequently changing data
  @cache_ttl_short to_timeout(minute: 1)
  # 1 minute for moderately changing data
  @cache_ttl_medium to_timeout(minute: 1)

  @doc """
  Get recent packets with caching
  """
  def get_recent_packets_cached(opts) do
    cache_key = generate_cache_key("recent_packets", opts)

    case Cachex.get(:query_cache, cache_key) do
      {:ok, result} when not is_nil(result) ->
        result

      _ ->
        result = Packets.get_recent_packets_optimized(opts)
        Cachex.put(:query_cache, cache_key, result, ttl: @cache_ttl_short)
        result
    end
  end

  @doc """
  Get weather packets with caching
  """
  def get_weather_packets_cached(callsign, start_time, end_time, opts) do
    cache_key = generate_cache_key("weather", {callsign, start_time, end_time, opts})

    case Cachex.get(:query_cache, cache_key) do
      {:ok, result} when not is_nil(result) ->
        result

      _ ->
        result = Packets.get_weather_packets(callsign, start_time, end_time, opts)
        Cachex.put(:query_cache, cache_key, result, ttl: @cache_ttl_medium)
        result
    end
  end

  @doc """
  Get packet count with caching.
  Now uses the efficient packet_counters table for O(1) performance.
  """
  def get_total_packet_count_cached do
    cache_key = "total_packet_count"

    case Cachex.get(:query_cache, cache_key) do
      {:ok, result} when not is_nil(result) ->
        result

      _ ->
        # This is now extremely fast due to the counter table
        result = Packets.get_total_packet_count()
        # Cache for only 5 seconds since the query is now instant
        Cachex.put(:query_cache, cache_key, result, ttl: to_timeout(second: 5))
        result
    end
  end

  @doc """
  Get latest packet for callsign with caching
  """
  def get_latest_packet_for_callsign_cached(callsign) do
    cache_key = generate_cache_key("latest_packet", callsign)

    case Cachex.get(:query_cache, cache_key) do
      {:ok, result} when not is_nil(result) ->
        result

      _ ->
        result = Packets.get_latest_packet_for_callsign(callsign)
        # Shorter TTL for latest packets as they change frequently
        Cachex.put(:query_cache, cache_key, result, ttl: @cache_ttl_short)
        result
    end
  end

  @doc """
  Get latest weather packet for callsign with caching.
  Uses the optimized query that checks recent data first.
  """
  def get_latest_weather_packet_cached(callsign) do
    cache_key = generate_cache_key("latest_weather_packet", callsign)

    case Cachex.get(:query_cache, cache_key) do
      {:ok, result} when not is_nil(result) ->
        result

      _ ->
        result = Packets.get_latest_weather_packet(callsign)
        # Cache for 5 minutes since weather updates are less frequent
        Cachex.put(:query_cache, cache_key, result, ttl: @cache_ttl_short)
        result
    end
  end

  @doc """
  Check if a callsign has weather packets with caching.
  Uses exact match for performance.
  """
  def has_weather_packets_cached?(callsign) do
    cache_key = generate_cache_key("has_weather_packets", callsign)

    case Cachex.get(:query_cache, cache_key) do
      {:ok, result} when not is_nil(result) ->
        result

      _ ->
        # Use exact match with proper index instead of ilike
        import Ecto.Query

        query =
          from p in Packet,
            where: p.sender == ^callsign,
            where:
              not is_nil(p.temperature) or not is_nil(p.humidity) or not is_nil(p.pressure) or
                not is_nil(p.wind_speed) or not is_nil(p.wind_direction) or not is_nil(p.rain_1h),
            limit: 1,
            select: true

        result = Repo.exists?(query)
        # Cache for 15 minutes
        Cachex.put(:query_cache, cache_key, result, ttl: @cache_ttl_medium)
        result
    end
  end

  @doc """
  Invalidate cache entries for a specific callsign
  """
  def invalidate_callsign_cache(callsign) do
    # Pattern-based cache invalidation
    patterns = [
      "latest_packet:#{callsign}",
      "latest_weather_packet:#{callsign}",
      "has_weather_packets:#{callsign}",
      "weather:#{callsign}:*"
    ]

    Enum.each(patterns, fn pattern ->
      Cachex.del(:query_cache, pattern)
    end)
  end

  @doc """
  Invalidate all cached queries
  """
  def invalidate_all_cache do
    Cachex.clear(:query_cache)
  end

  @doc """
  Get cache statistics
  """
  def get_cache_stats do
    {:ok, stats} = Cachex.stats(:query_cache)
    stats
  end

  @doc """
  Get path station positions with caching
  """
  def get_path_station_positions_cached(callsigns) when is_list(callsigns) do
    cache_key = generate_cache_key("path_stations", Enum.sort(callsigns))

    case Cachex.get(:query_cache, cache_key) do
      {:ok, result} when not is_nil(result) ->
        result

      _ ->
        query = """
        WITH latest_positions AS (
          SELECT DISTINCT ON (sender)
            sender,
            lat,
            lon,
            received_at
          FROM packets
          WHERE 
            sender = ANY($1::text[])
            AND lat IS NOT NULL
            AND lon IS NOT NULL
            AND received_at > NOW() - INTERVAL '7 days'
          ORDER BY sender, received_at DESC
        )
        SELECT sender, lat, lon
        FROM latest_positions
        ORDER BY array_position($1::text[], sender)
        """

        result =
          case SQL.query(Repo, query, [callsigns]) do
            {:ok, %{rows: rows}} ->
              Enum.map(rows, fn [callsign, lat, lon] ->
                %{
                  callsign: callsign,
                  lat: Decimal.to_float(lat),
                  lng: Decimal.to_float(lon)
                }
              end)

            {:error, _} ->
              []
          end

        # Cache for 5 minutes
        Cachex.put(:query_cache, cache_key, result, ttl: @cache_ttl_medium)
        result
    end
  end

  # Private helper functions

  defp generate_cache_key(prefix, data) do
    hash = :erlang.phash2(data)
    "#{prefix}:#{hash}"
  end
end
