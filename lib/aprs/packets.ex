defmodule Aprs.Packets do
  @moduledoc """
  The Packets context.
  """

  @behaviour Aprs.PacketsBehaviour

  import Ecto.Query, warn: false

  alias Aprs.BadPacket
  alias Aprs.EncodingUtils
  alias Aprs.Packet
  alias Aprs.Repo
  alias Parser.Types.MicE

  @doc """
  Stores a packet in the database.

  ## Parameters
    * `packet_data` - Map containing packet data to be stored
  """
  @spec store_packet(map()) :: {:ok, struct()} | {:error, :validation_error | :storage_exception}
  def store_packet(packet_data) do
    require Logger

    try do
      # Convert to map if it's a struct, or use as is if already a map
      packet_attrs =
        case packet_data do
          %Packet{} = packet ->
            packet
            |> Map.from_struct()
            |> Map.delete(:__meta__)

          %{} ->
            packet_data
        end

      # Sanitize all string fields to prevent UTF-8 encoding errors
      packet_attrs = sanitize_packet_strings(packet_attrs)

      # Convert data_type to string if it's an atom
      packet_attrs =
        case packet_attrs do
          %{data_type: data_type} when is_atom(data_type) ->
            Map.put(packet_attrs, :data_type, to_string(data_type))

          _ ->
            packet_attrs
        end

      # Make sure received_at is set with explicit UTC DateTime
      current_time = DateTime.truncate(DateTime.utc_now(), :microsecond)
      packet_attrs = Map.put(packet_attrs, :received_at, current_time)

      # Patch: If data_extended has latitude/longitude, set them as lat/lon as Decimal
      packet_attrs =
        case packet_attrs[:data_extended] do
          %{} = ext ->
            # Convert struct to map if needed
            ext_map = if Map.has_key?(ext, :__struct__), do: Map.from_struct(ext), else: ext

            lat =
              ext_map[:latitude] || ext_map["latitude"] ||
                (Map.has_key?(ext_map, :position) &&
                   (ext_map[:position][:latitude] || ext_map[:position]["latitude"])) ||
                (Map.has_key?(ext_map, "position") &&
                   (ext_map["position"][:latitude] || ext_map["position"]["latitude"]))

            lon =
              ext_map[:longitude] || ext_map["longitude"] ||
                (Map.has_key?(ext_map, :position) &&
                   (ext_map[:position][:longitude] || ext_map[:position]["longitude"])) ||
                (Map.has_key?(ext_map, "position") &&
                   (ext_map["position"][:longitude] || ext_map["position"]["longitude"]))

            latd = to_decimal(lat)
            lond = to_decimal(lon)

            if not is_nil(latd) and not is_nil(lond) do
              packet_attrs
              |> Map.put(:lat, latd)
              |> Map.put(:lon, lond)
            else
              packet_attrs
            end

          _ ->
            packet_attrs
        end

      # Extract position data with error handling
      {lat, lon} = extract_position(packet_attrs)

      # Helper to round to 6 decimal places
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

      # Set position fields if found (this will overwrite above if extract_position finds something different)
      packet_attrs =
        packet_attrs
        |> Map.put(:lat, round6.(lat))
        |> Map.put(:lon, round6.(lon))

      # Normalize ssid to string if present and not nil
      packet_attrs =
        case Map.get(packet_attrs, :ssid) do
          nil -> packet_attrs
          ssid -> Map.put(packet_attrs, :ssid, to_string(ssid))
        end

      # Insert the packet
      case %Packet{}
           |> Packet.changeset(packet_attrs)
           |> Repo.insert() do
        {:ok, packet} ->
          {:ok, packet}

        {:error, changeset} ->
          # Store validation errors as bad packets
          error_message =
            Enum.map_join(changeset.errors, ", ", fn {field, {msg, _}} -> "#{field}: #{msg}" end)

          store_bad_packet(packet_data, %{message: error_message, type: "ValidationError"})
          {:error, :validation_error}
      end
    rescue
      error ->
        Logger.error("Exception in store_packet for #{inspect(packet_data[:sender])}: #{inspect(error)}")

        store_bad_packet(packet_data, error)
        {:error, :storage_exception}
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
    # Standard position format
    if not is_nil(data_extended[:latitude]) and not is_nil(data_extended[:longitude]) do
      {to_float(data_extended[:latitude]), to_float(data_extended[:longitude])}
      # MicE packet format with components
    else
      case data_extended do
        %{__struct__: MicE} = mic_e ->
          lat = mic_e[:latitude]
          lon = mic_e[:longitude]

          if is_number(lat) and is_number(lon) do
            {lat, lon}
          else
            {nil, nil}
          end

        _ ->
          extract_position_from_mic_e(data_extended)
      end
    end
  end

  defp extract_position_from_data_extended(_), do: {nil, nil}

  defp extract_position_from_mic_e(%{__struct__: MicE} = mic_e) do
    if is_number(mic_e.lat_degrees) and is_number(mic_e.lat_minutes) and
         is_number(mic_e.lon_degrees) and is_number(mic_e.lon_minutes) do
      lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0
      lat = if mic_e.lat_direction == :south, do: -lat, else: lat

      lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0
      lon = if mic_e.lon_direction == :west, do: -lon, else: lon

      {lat, lon}
    else
      {nil, nil}
    end
  end

  defp extract_position_from_mic_e(_), do: {nil, nil}

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
    # Support both exact match and partial match
    # For exact match, check if callsign contains a hyphen (has SSID)
    if String.contains?(callsign, "-") do
      # Exact match for callsign with SSID
      [base_call, ssid] = String.split(callsign, "-", parts: 2)

      from p in query,
        where:
          ilike(p.base_callsign, ^base_call) and
            ((is_nil(p.ssid) and ^ssid == "0") or p.ssid == ^ssid)
    else
      # Match base callsign exactly, regardless of SSID
      from p in query, where: ilike(p.base_callsign, ^callsign)
    end
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
    Repo.one(from p in Packet, select: count(p.id))
  end

  @doc """
  Configure packet retention policy.

  Packets are retained based on these rules:
  - Default retention is 365 days (1 year) (configurable via :packet_retention_days)
  - Returns the number of packets deleted
  """
  def clean_old_packets do
    retention_days = Application.get_env(:aprs, :packet_retention_days, 365)
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
  defp sanitize_packet_strings(packet_attrs) when is_map(packet_attrs) do
    packet_attrs
    |> sanitize_field(:base_callsign, &sanitize_and_ensure_string/1)
    |> sanitize_field(:data_type, &sanitize_and_ensure_string/1)
    |> sanitize_field(:destination, &sanitize_and_ensure_string/1)
    |> sanitize_required_field(:information_field)
    |> sanitize_required_field(:path)
    |> sanitize_field(:sender, &sanitize_and_ensure_string/1)
    |> sanitize_field(:ssid, &sanitize_and_ensure_string/1)
    |> sanitize_field(:region, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:raw_packet, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:symbol_code, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:symbol_table_id, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:comment, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:timestamp, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:manufacturer, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:equipment_type, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:addressee, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:message_text, &EncodingUtils.sanitize_string/1)
    |> sanitize_field(:message_number, &EncodingUtils.sanitize_string/1)
    |> sanitize_data_extended_field()
  end

  # Helper to sanitize a field if it exists
  defp sanitize_field(map, key, sanitizer_func) do
    case Map.get(map, key) do
      nil -> map
      value -> Map.put(map, key, sanitizer_func.(value))
    end
  end

  # Helper to sanitize required fields, ensuring they exist and are not nil
  defp sanitize_required_field(map, key) do
    value = Map.get(map, key)
    sanitized_value = sanitize_required_string(value)
    Map.put(map, key, sanitized_value)
  end

  # Helper to sanitize and ensure required string fields are never nil
  defp sanitize_required_string(nil), do: ""
  defp sanitize_required_string(value), do: EncodingUtils.sanitize_string(value)

  # Helper to sanitize and ensure non-required string fields
  defp sanitize_and_ensure_string(nil), do: nil
  defp sanitize_and_ensure_string(value), do: EncodingUtils.sanitize_string(value)

  # Helper to sanitize the data_extended field
  defp sanitize_data_extended_field(packet_attrs) do
    case packet_attrs do
      %{data_extended: data_extended} when not is_nil(data_extended) ->
        sanitized_data_extended = EncodingUtils.sanitize_data_extended(data_extended)
        Map.put(packet_attrs, :data_extended, sanitized_data_extended)

      _ ->
        packet_attrs
    end
  end

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
