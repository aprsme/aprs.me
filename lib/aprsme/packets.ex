defmodule Aprsme.Packets do
  @moduledoc """
  The Packets context.
  """

  @behaviour Aprsme.PacketsBehaviour

  import Ecto.Query, warn: false

  alias Aprs.Types.MicE
  alias Aprsme.BadPacket
  alias Aprsme.Packet
  alias Aprsme.Packets.QueryBuilder
  alias Aprsme.Repo
  alias AprsmeWeb.TimeUtils

  require Logger

  @doc """
  Stores a packet in the database.

  ## Parameters
    * `packet_data` - Map containing packet data to be stored
  """
  @spec store_packet(map()) :: {:ok, struct()} | {:error, :validation_error | :storage_exception}
  def store_packet(packet_data) do
    require Logger

    try do
      # First sanitize the input data
      sanitized_packet_data = Aprsme.EncodingUtils.sanitize_packet(packet_data)

      packet_attrs =
        sanitized_packet_data
        |> Packet.extract_additional_data(
          sanitized_packet_data[:raw_packet] || sanitized_packet_data["raw_packet"] || ""
        )
        |> normalize_packet_attrs()
        |> set_received_at()
        |> patch_lat_lon_from_data_extended()
        |> then(fn attrs ->
          {lat, lon} = extract_position(attrs)
          set_lat_lon(attrs, lat, lon)
        end)
        |> normalize_ssid()
        |> then(fn attrs ->
          device_identifier = Aprsme.DeviceParser.extract_device_identifier(packet_data)
          canonical_identifier = get_canonical_device_identifier(device_identifier)
          Map.put(attrs, :device_identifier, canonical_identifier)
        end)

      # require Logger
      # Logger.debug("Sanitized packet_attrs before insert: #{inspect(packet_attrs)}")
      # Set device_identifier to parsed value, fallback to destination if nil
      parsed_device_id = Aprsme.DeviceParser.extract_device_identifier(packet_data)
      device_id = parsed_device_id || Map.get(packet_attrs, :destination)
      packet_attrs = Map.put(packet_attrs, :device_identifier, device_id)

      # Logger.debug("Inserting packet with device_identifier=#{inspect(device_id)}, destination=#{inspect(Map.get(packet_attrs, :destination))}")
      insert_packet(packet_attrs, packet_data)
    rescue
      error ->
        Logger.error("Exception in store_packet for #{inspect(packet_data[:sender])}: #{inspect(error)}")

        store_bad_packet(packet_data, error)
        {:error, :storage_exception}
    end
  end

  defp normalize_packet_attrs(packet_data) do
    case_result =
      case packet_data do
        %Packet{} = packet ->
          packet
          |> Map.from_struct()
          |> Map.delete(:__meta__)

        %{} ->
          packet_data
      end

    case_result
    |> Aprsme.EncodingUtils.sanitize_packet()
    |> normalize_data_type()
  end

  defp normalize_data_type(attrs) do
    Aprsme.EncodingUtils.normalize_data_type(attrs)
  end

  defp set_received_at(attrs) do
    current_time = DateTime.truncate(DateTime.utc_now(), :microsecond)
    Map.put(attrs, :received_at, current_time)
  end

  defp patch_lat_lon_from_data_extended(attrs) do
    case attrs[:data_extended] do
      %{} = ext ->
        ext_map = if Map.has_key?(ext, :__struct__), do: Map.from_struct(ext), else: ext
        lat = extract_lat_from_ext_map(ext_map)
        lon = extract_lon_from_ext_map(ext_map)
        latd = to_decimal(lat)
        lond = to_decimal(lon)

        if not is_nil(latd) and not is_nil(lond) do
          attrs
          |> Map.put(:lat, latd)
          |> Map.put(:lon, lond)
        else
          attrs
        end

      _ ->
        attrs
    end
  end

  defp extract_lat_from_ext_map(ext_map) when is_map(ext_map) and not is_struct(ext_map) do
    ext_map[:latitude] || ext_map["latitude"] ||
      (Map.has_key?(ext_map, :position) &&
         (ext_map[:position][:latitude] || ext_map[:position]["latitude"])) ||
      (Map.has_key?(ext_map, "position") &&
         (ext_map["position"][:latitude] || ext_map["position"]["latitude"]))
  end

  defp extract_lat_from_ext_map(_), do: nil

  defp extract_lon_from_ext_map(ext_map) when is_map(ext_map) and not is_struct(ext_map) do
    ext_map[:longitude] || ext_map["longitude"] ||
      (Map.has_key?(ext_map, :position) &&
         (ext_map[:position][:longitude] || ext_map[:position]["longitude"])) ||
      (Map.has_key?(ext_map, "position") &&
         (ext_map["position"][:longitude] || ext_map["position"]["longitude"]))
  end

  defp extract_lon_from_ext_map(_), do: nil

  defp set_lat_lon(attrs, lat, lon) do
    round6 = fn
      nil ->
        nil

      %Decimal{} = d ->
        Decimal.round(d, 6)

      n when is_float(n) ->
        Float.round(n, 6)

      n when is_integer(n) ->
        n * 1.0

      n when is_binary(n) ->
        case Float.parse(n) do
          {f, _} -> Float.round(f, 6)
          :error -> nil
        end

      _ ->
        nil
    end

    attrs
    |> Map.put(:lat, round6.(lat))
    |> Map.put(:lon, round6.(lon))
  end

  defp normalize_ssid(attrs) do
    case Map.get(attrs, :ssid) do
      nil -> attrs
      ssid -> Map.put(attrs, :ssid, to_string(ssid))
    end
  end

  defp insert_packet(attrs, packet_data) do
    # Ensure data_extended is properly sanitized before insertion
    attrs =
      if attrs[:data_extended] do
        sanitized_extended = Aprsme.EncodingUtils.sanitize_data_extended(attrs[:data_extended])
        # Double-check all values are sanitized
        # Only do additional sanitization for plain maps, not structs
        sanitized_extended =
          if is_struct(sanitized_extended) do
            sanitized_extended
          else
            Enum.reduce(sanitized_extended, %{}, fn {k, v}, acc ->
              sanitized_value = if is_binary(v), do: Aprsme.EncodingUtils.sanitize_string(v), else: v
              Map.put(acc, k, sanitized_value)
            end)
          end

        Map.put(attrs, :data_extended, sanitized_extended)
      else
        attrs
      end

    # Debug log to see what we're trying to insert
    if attrs[:data_extended] do
      require Logger

      Logger.debug("Final data_extended before insert: #{inspect(attrs[:data_extended], binaries: :as_binaries)}")
    end

    case %Packet{} |> Packet.changeset(attrs) |> Repo.insert() do
      {:ok, packet} ->
        # Invalidate cache for this packet's callsign
        if Map.has_key?(attrs, :sender) do
          # Cache invalidation removed - no longer using CachedQueries
        end

        {:ok, packet}

      {:error, changeset} ->
        error_message =
          Enum.map_join(changeset.errors, ", ", fn {field, {msg, _}} -> "#{field}: #{msg}" end)

        store_bad_packet(packet_data, %{message: error_message, type: "ValidationError"})
        {:error, :validation_error}
    end
  end

  @doc """
  Stores a bad packet in the database.

  ## Parameters
    * `packet_data` - The original packet data that failed to parse
    * `error` - The error that occurred during parsing
  """
  @spec store_bad_packet(map() | String.t(), any()) ::
          {:ok, struct()} | {:error, Ecto.Changeset.t()}
  def store_bad_packet(packet_data, error) when is_binary(packet_data) do
    error_type =
      case error do
        %{type: type} -> type
        %{__struct__: struct} -> struct
        _ -> "UnknownError"
      end

    error_message =
      case error do
        %{message: message} -> message
        %{__struct__: _} -> Exception.message(error)
        _ -> inspect(error)
      end

    %BadPacket{}
    |> BadPacket.changeset(%{
      raw_packet: Aprsme.EncodingUtils.sanitize_string(packet_data),
      error_message: error_message,
      error_type: error_type,
      attempted_at: DateTime.utc_now()
    })
    |> Repo.insert()
  end

  def store_bad_packet(packet_data, error) when is_map(packet_data) do
    error_type =
      case error do
        %{type: type} -> type
        %{__struct__: struct} -> struct
        _ -> "UnknownError"
      end

    error_message =
      case error do
        %{message: message} -> message
        %{__struct__: _} -> Exception.message(error)
        _ -> inspect(error)
      end

    %BadPacket{}
    |> BadPacket.changeset(%{
      raw_packet: packet_data[:raw_packet] || packet_data["raw_packet"] || inspect(packet_data),
      error_message: error_message,
      error_type: error_type,
      attempted_at: DateTime.utc_now()
    })
    |> Repo.insert()
  end

  # Extracts position data from packet, checking various possible locations
  defp extract_position(packet_data) do
    # Check for lat/lon at top level
    if not is_nil(packet_data[:lat]) and not is_nil(packet_data[:lon]) do
      {to_float(packet_data.lat), to_float(packet_data.lon)}
    else
      extract_position_from_data_extended(packet_data[:data_extended])
    end
  end

  defp extract_position_from_data_extended(nil), do: {nil, nil}

  defp extract_position_from_data_extended(data_extended) when is_map(data_extended) do
    if has_standard_position?(data_extended) do
      extract_standard_position(data_extended)
    else
      extract_position_from_data_extended_case(data_extended)
    end
  end

  defp extract_position_from_data_extended(_), do: {nil, nil}

  defp has_standard_position?(data_extended) when is_map(data_extended) and not is_struct(data_extended) do
    not is_nil(data_extended[:latitude]) and not is_nil(data_extended[:longitude])
  end

  defp has_standard_position?(_), do: false

  defp extract_standard_position(data_extended) when is_map(data_extended) and not is_struct(data_extended) do
    {to_float(data_extended[:latitude]), to_float(data_extended[:longitude])}
  end

  defp extract_standard_position(_), do: {nil, nil}

  defp extract_position_from_data_extended_case(data_extended) do
    case data_extended do
      %{__struct__: MicE} = mic_e ->
        extract_position_from_mic_e_struct(mic_e)

      %{__struct__: Aprs.Types.ParseError} ->
        # ParseError structs don't contain position data and don't implement Access behavior
        {nil, nil}

      _ ->
        extract_position_from_mic_e(data_extended)
    end
  end

  defp extract_position_from_mic_e_struct(%{__struct__: MicE} = mic_e) do
    # Use Access behavior for MicE struct which implements it
    lat = mic_e[:latitude]
    lon = mic_e[:longitude]

    if is_number(lat) and is_number(lon) do
      {lat, lon}
    else
      {nil, nil}
    end
  end

  defp extract_position_from_mic_e(%{
         lat_degrees: lat_deg,
         lat_minutes: lat_min,
         lat_direction: lat_dir,
         lon_degrees: lon_deg,
         lon_minutes: lon_min,
         lon_direction: lon_dir
       })
       when is_number(lat_deg) and is_number(lat_min) and is_number(lon_deg) and is_number(lon_min) do
    lat = apply_direction(lat_deg + lat_min / 60.0, lat_dir, :south)
    lon = apply_direction(lon_deg + lon_min / 60.0, lon_dir, :west)
    {lat, lon}
  end

  defp extract_position_from_mic_e(_data_extended), do: {nil, nil}

  defp apply_direction(value, direction, negative_direction) when direction == negative_direction, do: -value
  defp apply_direction(value, _direction, _negative_direction), do: value

  defp get_canonical_device_identifier(device_identifier) do
    case Aprsme.DeviceIdentification.lookup_device_by_identifier(device_identifier) do
      %{identifier: canonical_id} -> canonical_id
      nil -> device_identifier
    end
  end

  @doc """
  Gets packets for replay.

  ## Parameters
    * `opts` - Map of options for filtering and pagination:
      * `:lat` - Latitude for center point filtering
      * `:lon` - Longitude for center point filtering
      * `:radius` - Radius in kilometers for filtering
      * `:callsign` - Filter by callsign
      * `:region` - Filter by region
      * `:start_time` - Start time for replay (DateTime)
      * `:end_time` - End time for replay (DateTime)
      * `:limit` - Maximum number of packets to return
      * `:page` - Page number for pagination
  """
  @impl true
  def get_packets_for_replay(opts \\ %{}) do
    limit = Map.get(opts, :limit, 1000)
    bounds = Map.get(opts, :bounds)

    query =
      from(p in Packet)
      |> QueryBuilder.with_position()
      |> QueryBuilder.with_time_range(opts)
      |> QueryBuilder.maybe_filter_region(opts)
      |> maybe_filter_by_callsign(opts)
      |> maybe_filter_by_bounds(bounds)
      |> QueryBuilder.chronological()
      |> QueryBuilder.paginate(limit)
      |> QueryBuilder.with_coordinates()

    Repo.all(query)
  end

  defp maybe_filter_by_callsign(query, %{callsign: callsign}) when not is_nil(callsign) do
    QueryBuilder.for_callsign(query, callsign)
  end

  defp maybe_filter_by_callsign(query, _), do: query

  defp maybe_filter_by_bounds(query, bounds) when is_list(bounds) and length(bounds) == 4 do
    QueryBuilder.within_bounds(query, bounds)
  end

  defp maybe_filter_by_bounds(query, _), do: query

  @doc """
  Gets historical packet count for a map area.
  """
  @impl true
  @spec get_historical_packet_count(map()) :: non_neg_integer()
  def get_historical_packet_count(opts \\ %{}) do
    bounds = Map.get(opts, :bounds)

    query =
      from(p in Packet)
      |> QueryBuilder.with_position()
      |> QueryBuilder.with_time_range(opts)
      |> QueryBuilder.maybe_filter_region(opts)
      |> maybe_filter_by_callsign(opts)
      |> maybe_filter_by_bounds(bounds)
      |> select(count())

    case Repo.one(query) do
      nil -> 0
      count when is_integer(count) -> count
      _ -> 0
    end
  rescue
    _ -> 0
  end

  @doc """
  Gets recent packets for initial map load.
  This uses an efficient query pattern for the most common use case.
  """
  @impl true
  @spec get_recent_packets(map()) :: [struct()]
  def get_recent_packets(opts \\ %{}) do
    # Use hours_back from opts if provided, otherwise default to 24 hours
    hours_back = Map.get(opts, :hours_back, 24)
    time_ago = DateTime.add(DateTime.utc_now(), -hours_back * 3600, :second)
    limit = Map.get(opts, :limit, 200)
    offset = Map.get(opts, :offset, 0)

    # Build base query with time and position filters
    base_query =
      from(p in Packet,
        where: p.has_position == true,
        where: p.received_at >= ^time_ago
      )

    # Apply spatial and other filters BEFORE limiting
    # This ensures we get the most recent packets within the specified bounds
    bounds = Map.get(opts, :bounds)

    query =
      base_query
      |> QueryBuilder.maybe_filter_region(opts)
      |> maybe_filter_by_callsign(opts)
      |> maybe_filter_by_bounds(bounds)
      |> order_by(desc: :received_at)
      |> limit(^limit)
      |> offset(^offset)
      |> QueryBuilder.with_coordinates()

    Repo.all(query)
  end

  @doc """
  Gets the closest stations to a given point.
  Uses PostGIS spatial indexes for efficient querying.
  Returns only the most recent packet per callsign, ordered by distance.
  """
  @impl true
  @spec get_nearby_stations(float(), float(), String.t() | nil, map()) :: [struct()]
  def get_nearby_stations(lat, lon, exclude_callsign \\ nil, opts \\ %{}) do
    limit = Map.get(opts, :limit, 10)
    hours_back = Map.get(opts, :hours_back, 1)
    cutoff_time = TimeUtils.hours_ago(hours_back)

    # Use named prepared statement for better performance (future optimization)
    # _statement_name = if exclude_callsign, do: "nearby_stations_exclude", else: "nearby_stations"

    # Use DISTINCT ON for better performance than window functions
    # First get most recent packet per callsign, then order by distance
    # Add spatial bounds to use indexes effectively
    query = """
    SELECT * FROM (
      SELECT DISTINCT ON (p.base_callsign)
        p.*,
        ST_Distance(p.location::geography, ST_SetSRID(ST_MakePoint($2, $1), 4326)::geography) as distance
      FROM packets p
      WHERE p.has_position = true
        AND p.received_at >= $3
        AND p.location IS NOT NULL
        #{if exclude_callsign, do: "AND p.sender != $4", else: ""}
        AND ST_DWithin(
          p.location::geography,
          ST_SetSRID(ST_MakePoint($2, $1), 4326)::geography,
          100000  -- 100km radius for initial filtering
        )
      ORDER BY p.base_callsign, p.received_at DESC
    ) AS recent_packets
    ORDER BY distance ASC
    LIMIT #{if exclude_callsign, do: "$5", else: "$4"}
    """

    params =
      if exclude_callsign do
        [lat, lon, cutoff_time, exclude_callsign, limit]
      else
        [lat, lon, cutoff_time, limit]
      end

    case Repo.query(query, params) do
      {:ok, result} ->
        Enum.map(result.rows, &Repo.load(Packet, {result.columns, &1}))

      {:error, _} ->
        []
    end
  end

  @doc """
  Gets weather packets for a specific callsign within a time range.
  This is optimized for weather queries by filtering at the database level.
  """
  @impl true
  @spec get_weather_packets(String.t(), DateTime.t(), DateTime.t(), map()) :: [struct()]
  def get_weather_packets(callsign, start_time, end_time, opts \\ %{}) do
    opts =
      Map.merge(opts, %{
        callsign: callsign,
        start_time: start_time,
        end_time: end_time,
        limit: Map.get(opts, :limit, 500)
      })

    opts
    |> QueryBuilder.weather_packets()
    |> QueryBuilder.with_coordinates()
    |> Repo.all()
  end

  # Filter for weather packets at the database level

  @doc """
  Retrieves a continuous stream of stored packets for replay in chronological order.

  This function returns a Stream that can be used to process packets in chronological
  order, preserving the timing between packets.

  ## Parameters
    * `opts` - The same options as `get_packets_for_replay/1`
      * `:playback_speed` - Speed multiplier (1.0 = real-time, 2.0 = 2x speed, etc.)

  ## Returns
    * Stream of packets with timing information
  """
  @impl true
  def stream_packets_for_replay(opts \\ %{}) do
    packets = get_packets_for_replay(opts)
    playback_speed = Map.get(opts, :playback_speed, 1.0)

    # Return a stream that emits packets with their original timing
    Stream.unfold({packets, nil}, fn
      {[], _} ->
        nil

      {[packet | rest], nil} ->
        {{0, packet}, {rest, packet}}

      {[next | rest], prev} ->
        # Calculate delay between packets in milliseconds, then convert to seconds
        delay_ms = DateTime.diff(next.received_at, prev.received_at, :millisecond)
        adjusted_delay = delay_ms / (playback_speed * 1000)
        {{adjusted_delay, next}, {rest, next}}
    end)
  end

  @doc """
  Gets the total count of stored packets in the database.
  Uses the efficient packet_counters table with triggers for O(1) performance.
  """
  @spec get_total_packet_count() :: non_neg_integer()
  def get_total_packet_count do
    # Use the PostgreSQL function for instant count retrieval
    case Repo.query("SELECT get_packet_count()", []) do
      {:ok, %{rows: [[count]]}} when is_integer(count) ->
        count

      {:ok, %{rows: [[nil]]}} ->
        # Fallback to actual count if counter is not initialized
        Repo.one(from p in Packet, select: count(p.id)) || 0

      {:error, _} ->
        # Try the faster counter table directly as a fallback
        query = """
        SELECT count FROM packet_counters 
        WHERE counter_type = 'total_packets'
        LIMIT 1
        """

        case Repo.query(query, []) do
          {:ok, %{rows: [[count]]}} -> count
          _ -> 0
        end
    end
  rescue
    DBConnection.ConnectionError ->
      require Logger

      Logger.warning("Database connection error in get_total_packet_count, returning 0")
      0

    error ->
      require Logger

      Logger.error("Error getting total packet count: #{inspect(error)}")
      0
  end

  @doc """
  Gets the timestamp of the oldest stored packet in the database.
  Returns nil if no packets exist.
  """
  @spec get_oldest_packet_timestamp() :: DateTime.t() | nil
  def get_oldest_packet_timestamp do
    Repo.one(
      from p in Packet,
        select: min(p.received_at)
    )
  rescue
    _ -> nil
  end

  @doc """
  Configure packet retention policy.

  Packets are retained based on these rules:
  - Default retention is 365 days (1 year) (configurable via :packet_retention_days)
  - Returns the number of packets deleted
  """
  @impl true
  def clean_old_packets do
    retention_days = Application.get_env(:aprsme, :packet_retention_days, 365)
    cutoff_time = DateTime.add(DateTime.utc_now(), -retention_days * 86_400, :second)

    {deleted_count, _} = Repo.delete_all(from(p in Packet, where: p.received_at < ^cutoff_time))

    deleted_count
  end

  @doc """
  Clean packets older than a specific number of days.

  This function allows for more granular cleanup operations by specifying
  the exact age threshold for packet deletion.

  ## Parameters
  - `days` - Number of days to keep (packets older than this will be deleted)

  ## Returns
  - Number of packets deleted
  """
  @impl true
  @spec clean_packets_older_than(pos_integer()) :: {:ok, non_neg_integer()} | {:error, any()}
  def clean_packets_older_than(days) when is_integer(days) and days > 0 do
    cutoff_time = DateTime.add(DateTime.utc_now(), -days * 86_400, :second)

    {deleted_count, _} = Repo.delete_all(from(p in Packet, where: p.received_at < ^cutoff_time))

    {:ok, deleted_count}
  end

  # Helper to convert various types to float
  defp to_float(value), do: Aprsme.EncodingUtils.to_float(value)

  # Helper to convert various types to Decimal
  defp to_decimal(value), do: Aprsme.EncodingUtils.to_decimal(value)

  # Get packets from last hour only - used to initialize the map
  @spec get_last_hour_packets() :: [struct()]
  def get_last_hour_packets do
    %{hours_back: 1, limit: 500}
    |> QueryBuilder.recent_position_packets()
    |> QueryBuilder.chronological()
    |> Repo.all()
  rescue
    error ->
      Logger.error("Failed to get last hour packets: #{inspect(error)}")
      []
  end

  # @doc """
  # Spatial query functions for efficient location-based searches using PostGIS
  # """

  # Temporarily commented out PostGIS functions until PostGIS is properly configured

  # @doc """
  # Find packets within a given radius (in meters) of a point.
  # Uses PostGIS spatial indexes for efficient querying.
  # """
  # def find_packets_within_radius(lat, lon, radius_meters, opts \\ %{}) do
  #   point = %Geo.Point{coordinates: {lon, lat}, srid: 4326}

  #   base_query =
  #     Packet
  #     |> where([p], not is_nil(p.location))
  #     |> where([p], st_dwithin_in_meters(p.location, ^point, ^radius_meters))
  #     |> order_by([p], st_distance(p.location, ^point))

  #   base_query
  #   |> apply_common_filters(opts)
  #   |> maybe_limit(opts)
  #   |> Repo.all()
  # end

  # Additional PostGIS functions commented out for now...

  # Helper functions for spatial queries - commented out for now

  # defp apply_common_filters(query, opts) do
  #   query
  #   |> filter_by_time_range(opts)
  #   |> filter_by_callsign(opts)
  #   |> filter_by_data_type(opts)
  # end

  # defp filter_by_time_range(query, %{start_time: start_time, end_time: end_time}) do
  #   from p in query,
  #     where: p.received_at >= ^start_time and p.received_at <= ^end_time
  # end

  # defp filter_by_time_range(query, %{start_time: start_time}) do
  #   from p in query, where: p.received_at >= ^start_time
  # end

  # defp filter_by_time_range(query, %{end_time: end_time}) do
  #   from p in query, where: p.received_at <= ^end_time
  # end

  # defp filter_by_time_range(query, %{hours_back: hours}) do
  #   cutoff_time = DateTime.utc_now() |> DateTime.add(-hours, :hour)
  #   from p in query, where: p.received_at >= ^cutoff_time
  # end

  # defp filter_by_time_range(query, _), do: query

  # defp filter_by_data_type(query, %{data_type: data_type}) do
  #   from p in query, where: p.data_type == ^data_type
  # end

  # defp filter_by_data_type(query, _), do: query

  # defp maybe_limit(query, %{limit: limit}) when is_integer(limit) and limit > 0 do
  #   from p in query, limit: ^limit
  # end

  # defp maybe_limit(query, _), do: query

  # Calculate clustering distance based on zoom level
  # Higher zoom levels need smaller clustering distances
  # defp calculate_cluster_distance(zoom_level) when zoom_level >= 15, do: 100    # 100m
  # defp calculate_cluster_distance(zoom_level) when zoom_level >= 12, do: 500    # 500m
  # defp calculate_cluster_distance(zoom_level) when zoom_level >= 9, do: 2000    # 2km
  # defp calculate_cluster_distance(zoom_level) when zoom_level >= 6, do: 10000   # 10km
  # defp calculate_cluster_distance(_), do: 50000                                 # 50km

  @doc """
  Gets the most recent packet for a callsign regardless of type or age.
  This is used for API endpoints that need the latest packet from a source.
  """
  @spec get_latest_packet_for_callsign(String.t()) :: struct() | nil
  def get_latest_packet_for_callsign(callsign) when is_binary(callsign) do
    callsign
    |> QueryBuilder.callsign_history(%{limit: 1})
    |> QueryBuilder.with_coordinates()
    |> Repo.one()
  end

  @doc """
  Gets the most recent weather packet for a callsign.
  Looks for any packet containing weather data fields.
  """
  @spec get_latest_weather_packet(String.t()) :: struct() | nil
  def get_latest_weather_packet(callsign) when is_binary(callsign) do
    # Use proper index with short time window first
    case get_latest_weather_in_window(callsign, 24) do
      nil -> get_latest_weather_in_window(callsign, 168)
      packet -> packet
    end
  end

  @doc """
  Check if a callsign has any weather packets.
  """
  def has_weather_packets?(callsign) when is_binary(callsign) do
    import Ecto.Query

    query =
      from p in Packet,
        where: p.sender == ^callsign,
        where:
          not is_nil(p.temperature) or not is_nil(p.humidity) or not is_nil(p.pressure) or
            not is_nil(p.wind_speed) or not is_nil(p.wind_direction) or not is_nil(p.rain_1h),
        limit: 1,
        select: true

    Repo.exists?(query)
  end

  defp get_latest_weather_in_window(callsign, hours) do
    import Ecto.Query

    since = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)

    query =
      from p in Packet,
        where: p.sender == ^callsign,
        where: p.received_at > ^since,
        where:
          not is_nil(p.temperature) or not is_nil(p.humidity) or not is_nil(p.pressure) or
            not is_nil(p.wind_speed) or not is_nil(p.wind_direction) or not is_nil(p.rain_1h),
        order_by: [desc: p.received_at],
        limit: 1

    Repo.one(query)
  end
end
