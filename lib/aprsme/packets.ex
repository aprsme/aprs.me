defmodule Aprsme.Packets do
  @moduledoc """
  The Packets context.
  """

  @behaviour Aprsme.PacketsBehaviour

  import Ecto.Query, warn: false

  alias Aprs.Types.MicE
  alias Aprsme.BadPacket
  alias Aprsme.Packet
  alias Aprsme.Repo

  @doc """
  Stores a packet in the database.

  ## Parameters
    * `packet_data` - Map containing packet data to be stored
  """
  @spec store_packet(map()) :: {:ok, struct()} | {:error, :validation_error | :storage_exception}
  def store_packet(packet_data) do
    require Logger

    try do
      packet_attrs =
        packet_data
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
          matched_device = Aprsme.DeviceIdentification.lookup_device_by_identifier(device_identifier)
          canonical_identifier = if matched_device, do: matched_device.identifier, else: device_identifier
          Map.put(attrs, :device_identifier, canonical_identifier)
        end)
        |> sanitize_packet_strings()

      # require Logger
      # Logger.debug("Sanitized packet_attrs before insert: #{inspect(packet_attrs)}")
      packet_attrs = Map.new(packet_attrs, fn {k, v} -> {k, sanitize_packet_strings(v)} end)
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
    |> sanitize_packet_strings()
    |> normalize_data_type()
  end

  defp normalize_data_type(attrs) do
    case attrs do
      %{data_type: data_type} when is_atom(data_type) ->
        Map.put(attrs, :data_type, to_string(data_type))

      _ ->
        attrs
    end
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
    case %Packet{} |> Packet.changeset(attrs) |> Repo.insert() do
      {:ok, packet} ->
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
  def store_bad_packet(packet_data, error) do
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
      raw_packet: inspect(packet_data),
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

  defp extract_position_from_mic_e_struct(_), do: {nil, nil}

  defp extract_position_from_mic_e(data_extended) do
    if is_number(data_extended[:lat_degrees]) and is_number(data_extended[:lat_minutes]) and
         is_number(data_extended[:lon_degrees]) and is_number(data_extended[:lon_minutes]) do
      lat = data_extended[:lat_degrees] + data_extended[:lat_minutes] / 60.0
      lat = if data_extended[:lat_direction] == :south, do: -lat, else: lat

      lon = data_extended[:lon_degrees] + data_extended[:lon_minutes] / 60.0
      lon = if data_extended[:lon_direction] == :west, do: -lon, else: lon

      {lat, lon}
    else
      {nil, nil}
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
    base_query = from(p in Packet, order_by: [asc: p.received_at], where: p.has_position == true)

    query =
      base_query
      |> filter_by_time(opts)
      |> filter_by_region(opts)
      |> filter_by_callsign(opts)
      |> filter_by_map_bounds(opts)
      |> limit_results(opts)
      |> select_with_virtual_coordinates()

    Repo.all(query)
  end

  @doc """
  Gets historical packet count for a map area.
  """
  @impl true
  @spec get_historical_packet_count(map()) :: non_neg_integer()
  def get_historical_packet_count(opts \\ %{}) do
    base_query = from(p in Packet, select: count(p.id), where: p.has_position == true)

    query =
      base_query
      |> filter_by_time(opts)
      |> filter_by_region(opts)
      |> filter_by_callsign(opts)
      |> filter_by_map_bounds(opts)

    Repo.one(query)
  end

  # Adds a select clause to populate virtual lat/lon fields from PostGIS geometry.
  defp select_with_virtual_coordinates(query) do
    from p in query,
      select: %{p | lat: fragment("ST_Y(?)", p.location), lon: fragment("ST_X(?)", p.location)}
  end

  # Query building helpers
  # Handle both start_time and end_time
  defp filter_by_time(query, %{start_time: start_time, end_time: end_time}) do
    # Ensure we prioritize packets from the last hour
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    effective_start_time =
      if DateTime.before?(start_time, one_hour_ago), do: one_hour_ago, else: start_time

    from p in query,
      where: p.received_at >= ^effective_start_time and p.received_at <= ^end_time
  end

  # Handle only start_time
  defp filter_by_time(query, %{start_time: start_time}) do
    from p in query, where: p.received_at >= ^start_time
  end

  # Handle only end_time
  defp filter_by_time(query, %{end_time: end_time}) do
    from p in query, where: p.received_at <= ^end_time
  end

  # Default case
  defp filter_by_time(query, _), do: query

  @doc """
  Gets recent packets for the map view.
  This is used for initial map loading to show only recent packets.
  """
  @impl true
  @spec get_recent_packets(map()) :: [struct()]
  def get_recent_packets(opts \\ %{}) do
    # Always limit to the last hour
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    # Merge the one-hour limit with any other filters
    opts_with_time = Map.put(opts, :start_time, one_hour_ago)

    get_packets_for_replay(opts_with_time)
  end

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

  defp filter_by_region(query, %{region: region}) do
    from p in query, where: p.region == ^region
  end

  defp filter_by_region(query, _), do: query

  defp filter_by_callsign(query, %{callsign: callsign}) do
    # Use sender field for exact callsign matching
    from p in query, where: ilike(p.sender, ^callsign)
  end

  defp filter_by_callsign(query, _), do: query

  defp filter_by_map_bounds(query, %{bounds: [min_lon, min_lat, max_lon, max_lat]})
       when not is_nil(min_lon) and not is_nil(min_lat) and not is_nil(max_lon) and not is_nil(max_lat) do
    # Create a bounding box polygon for PostGIS spatial query
    bbox_wkt =
      "POLYGON((#{min_lon} #{min_lat}, #{max_lon} #{min_lat}, #{max_lon} #{max_lat}, #{min_lon} #{max_lat}, #{min_lon} #{min_lat}))"

    from p in query,
      where: p.has_position == true,
      where: not is_nil(p.location),
      where: fragment("ST_Within(?, ST_GeomFromText(?, 4326))", p.location, ^bbox_wkt)
  end

  defp filter_by_map_bounds(query, _), do: query

  defp limit_results(query, %{limit: limit, page: page}) when not is_nil(limit) and not is_nil(page) do
    offset = (page - 1) * limit
    from p in query, limit: ^limit, offset: ^offset
  end

  defp limit_results(query, %{limit: limit}) when not is_nil(limit) do
    from p in query, limit: ^limit
  end

  defp limit_results(query, _), do: query

  @doc """
  Gets the total count of stored packets in the database.
  """
  @spec get_total_packet_count() :: non_neg_integer()
  def get_total_packet_count do
    Repo.one(from p in Packet, select: count(p.id)) || 0
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
  @doc \"""
  Removes packets older than the specified number of days.

  ## Parameters
  - `days` - Number of days to keep (packets older than this will be deleted)

  ## Returns
  - Number of packets deleted
  """
  @impl true
  @spec clean_packets_older_than(pos_integer()) :: non_neg_integer()
  def clean_packets_older_than(days) when is_integer(days) and days > 0 do
    cutoff_time = DateTime.add(DateTime.utc_now(), -days * 86_400, :second)

    {deleted_count, _} = Repo.delete_all(from(p in Packet, where: p.received_at < ^cutoff_time))

    deleted_count
  end

  # Helper to convert various types to float
  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_integer(value), do: value * 1.0
  defp to_float(%Decimal{} = value), do: Decimal.to_float(value)

  defp to_float(value) when is_binary(value) do
    case Float.parse(value) do
      {float, _} -> float
      :error -> nil
    end
  end

  defp to_float(_), do: nil

  # Helper to convert various types to Decimal
  defp to_decimal(%Decimal{} = d), do: d
  defp to_decimal(f) when is_float(f), do: Decimal.from_float(f)
  defp to_decimal(i) when is_integer(i), do: Decimal.new(i)

  defp to_decimal(s) when is_binary(s) do
    case Decimal.parse(s) do
      {d, _} ->
        d

      :error ->
        case Float.parse(s) do
          {f, _} -> Decimal.from_float(f)
          :error -> nil
        end
    end
  end

  defp to_decimal(_), do: nil

  # Helper to sanitize all string fields in packet data before database storage
  defp sanitize_packet_strings(%DateTime{} = dt), do: dt
  defp sanitize_packet_strings(%NaiveDateTime{} = ndt), do: ndt
  defp sanitize_packet_strings(%_struct{} = struct), do: struct |> Map.from_struct() |> sanitize_packet_strings()
  defp sanitize_packet_strings(list) when is_list(list), do: Enum.map(list, &sanitize_packet_strings/1)

  defp sanitize_packet_strings(binary) when is_binary(binary) do
    s = Aprsme.EncodingUtils.sanitize_string(binary)
    if is_binary(s), do: s, else: ""
  end

  defp sanitize_packet_strings(other), do: other

  # Get packets from last hour only - used to initialize the map
  @spec get_last_hour_packets() :: [struct()]
  def get_last_hour_packets do
    one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)

    Packet
    |> where([p], p.has_position == true)
    |> where([p], p.received_at >= ^one_hour_ago)
    |> order_by([p], asc: p.received_at)
    |> limit(500)
    |> select_with_virtual_coordinates()
    |> Repo.all()
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
end
