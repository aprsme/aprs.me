defmodule Aprsme.PacketConsumer do
  @moduledoc """
  GenStage consumer that batches APRS packets and inserts them into the database
  efficiently to reduce database load.
  """
  use GenStage

  alias Aprsme.Repo

  require Logger

  def start_link(opts \\ []) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    batch_size = opts[:batch_size] || 100
    batch_timeout = opts[:batch_timeout] || 1000

    # Start a timer for batch processing
    timer = Process.send_after(self(), :process_batch, batch_timeout)

    {:consumer,
     %{
       batch: [],
       batch_size: batch_size,
       batch_timeout: batch_timeout,
       timer: timer
     }}
  end

  @impl true
  def handle_events(events, _from, %{batch: batch, batch_size: batch_size} = state) do
    new_batch = batch ++ events

    if length(new_batch) >= batch_size do
      # Process the batch immediately
      process_batch(new_batch)
      {:noreply, [], %{state | batch: []}}
    else
      # Add to batch and wait for more
      {:noreply, [], %{state | batch: new_batch}}
    end
  end

  @impl true
  def handle_info(:process_batch, %{batch: batch, batch_timeout: timeout} = state) do
    if length(batch) > 0 do
      process_batch(batch)
    end

    # Start a new timer
    timer = Process.send_after(self(), :process_batch, timeout)
    {:noreply, [], %{state | batch: [], timer: timer}}
  end

  defp process_batch(packets) do
    require Logger

    start_time = System.monotonic_time(:millisecond)

    results =
      packets
      |> Enum.chunk_every(50)
      |> Enum.map(&process_chunk/1)

    {success_count, error_count} =
      Enum.reduce(results, {0, 0}, fn {success, error}, {total_success, total_error} ->
        {total_success + success, total_error + error}
      end)

    end_time = System.monotonic_time(:millisecond)
    duration = end_time - start_time

    :telemetry.execute(
      [
        :aprsme,
        :packet_pipeline,
        :batch
      ],
      %{count: length(packets), success: success_count, error: error_count, duration_ms: duration},
      %{}
    )

    Logger.info(
      "Processed batch of #{length(packets)} packets in #{duration}ms (success: #{success_count}, errors: #{error_count})"
    )
  end

  defp process_chunk(packets) do
    # Prepare packets for batch insertion
    packet_attrs =
      packets
      |> Enum.map(&prepare_packet_for_insert/1)
      # Ensure truncation here
      |> Enum.map(&truncate_datetimes_to_second/1)

    # Filter out invalid packets
    {valid_packets, invalid_packets} = Enum.split_with(packet_attrs, &valid_packet?/1)

    # Insert valid packets in batch
    case Repo.insert_all(Aprsme.Packet, valid_packets, returning: [:id]) do
      {:error, error} ->
        Logger.error("Batch insert failed: #{inspect(error)}")
        {0, length(packets)}

      {inserted_count, _} ->
        error_count = Enum.count(invalid_packets)
        {inserted_count, error_count}
    end
  end

  defp prepare_packet_for_insert(packet_data) do
    # Always set received_at timestamp to ensure consistency
    current_time = DateTime.truncate(DateTime.utc_now(), :microsecond)
    packet_data = Map.put(packet_data, :received_at, current_time)

    # Convert to map before storing to avoid struct conversion issues
    attrs = struct_to_map(packet_data)

    # Extract additional data from the parsed packet including raw packet
    attrs = Aprsme.Packet.extract_additional_data(attrs, attrs[:raw_packet] || "")

    # Normalize data_type to string if it's an atom
    attrs = normalize_data_type(attrs)

    # Apply the same processing as the original store_packet function
    attrs
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
    |> Map.put(:inserted_at, current_time)
    |> Map.put(:updated_at, current_time)
    |> Map.delete(:id)
    |> Map.delete("id")
    # Remove embedded field for batch insert
    |> Map.delete(:data_extended)
    |> normalize_numeric_types()
    |> sanitize_raw_packet()
    |> then(fn attrs -> Map.new(attrs, fn {k, v} -> {k, sanitize_packet_strings(v)} end) end)
    |> truncate_datetimes_to_second()
    # Explicitly remove raw_weather_data to prevent insert_all errors
    |> Map.delete(:raw_weather_data)
    |> Map.delete("raw_weather_data")
    # Create PostGIS geometry for location field
    |> create_location_geometry()
  rescue
    error ->
      Logger.error("Failed to prepare packet for batch insert: #{inspect(error)}")
      nil
  end

  # Create PostGIS geometry from lat/lon coordinates
  defp create_location_geometry(attrs) do
    lat = attrs[:lat]
    lon = attrs[:lon]

    if valid_coordinates?(lat, lon) do
      location = create_point(lat, lon)

      if location do
        Map.put(attrs, :location, location)
      else
        attrs
      end
    else
      attrs
    end
  end

  # Helper functions for coordinate validation and point creation
  defp valid_coordinates?(lat, lon) do
    lat = normalize_coordinate(lat)
    lon = normalize_coordinate(lon)

    is_number(lat) && is_number(lon) &&
      lat >= -90 && lat <= 90 &&
      lon >= -180 && lon <= 180
  end

  defp normalize_coordinate(%Decimal{} = decimal), do: Decimal.to_float(decimal)
  defp normalize_coordinate(coord), do: coord

  defp create_point(lat, lon)
       when (is_number(lat) or is_struct(lat, Decimal)) and (is_number(lon) or is_struct(lon, Decimal)) do
    lat = normalize_coordinate(lat)
    lon = normalize_coordinate(lon)

    if valid_coordinates?(lat, lon) do
      %Geo.Point{coordinates: {lon, lat}, srid: 4326}
    end
  end

  defp create_point(_, _), do: nil

  defp valid_packet?(nil), do: false
  defp valid_packet?(%{sender: sender}) when is_binary(sender) and byte_size(sender) > 0, do: true
  defp valid_packet?(_), do: false

  # Helper functions copied from Packets module for consistency
  defp normalize_packet_attrs(attrs) do
    attrs
    |> Map.put_new(:base_callsign, attrs[:sender])
    |> Map.put_new(:data_type, "unknown")
    |> Map.put_new(:destination, "")
    |> Map.put_new(:information_field, "")
    |> Map.put_new(:path, "")
    |> Map.put_new(:ssid, "")
    |> Map.put_new(:data_extended, %{})
  end

  defp set_received_at(attrs) do
    received_at = attrs[:received_at] || DateTime.utc_now()
    Map.put(attrs, :received_at, received_at)
  end

  defp patch_lat_lon_from_data_extended(attrs) do
    case attrs[:data_extended] do
      %{latitude: lat, longitude: lon} when not is_nil(lat) and not is_nil(lon) ->
        attrs
        |> Map.put(:lat, lat)
        |> Map.put(:lon, lon)
        |> Map.put(:has_position, true)

      _ ->
        attrs
    end
  end

  defp extract_position(packet_data) do
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
    lat = extract_lat_from_ext_map(data_extended)
    lon = extract_lon_from_ext_map(data_extended)
    {to_float(lat), to_float(lon)}
  end

  defp extract_lat_from_ext_map(ext_map) do
    ext_map[:latitude] || ext_map["latitude"] ||
      (Map.has_key?(ext_map, :position) &&
         (ext_map[:position][:latitude] || ext_map[:position]["latitude"])) ||
      (Map.has_key?(ext_map, "position") &&
         (ext_map["position"][:latitude] || ext_map["position"]["latitude"]))
  end

  defp extract_lon_from_ext_map(ext_map) do
    ext_map[:longitude] || ext_map["longitude"] ||
      (Map.has_key?(ext_map, :position) &&
         (ext_map[:position][:longitude] || ext_map[:position]["longitude"])) ||
      (Map.has_key?(ext_map, "position") &&
         (ext_map["position"][:longitude] || ext_map["position"]["longitude"]))
  end

  defp set_lat_lon(attrs, lat, lon) do
    round6 = fn
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
    |> Map.put(:has_position, not is_nil(lat) and not is_nil(lon))
  end

  defp normalize_ssid(attrs) do
    case Map.get(attrs, :ssid) do
      nil -> attrs
      ssid -> Map.put(attrs, :ssid, to_string(ssid))
    end
  end

  defp sanitize_packet_strings(%DateTime{} = dt), do: dt
  defp sanitize_packet_strings(%NaiveDateTime{} = ndt), do: ndt
  defp sanitize_packet_strings(%_struct{} = struct), do: struct |> Map.from_struct() |> sanitize_packet_strings()
  defp sanitize_packet_strings(list) when is_list(list), do: Enum.map(list, &sanitize_packet_strings/1)

  defp sanitize_packet_strings(binary) when is_binary(binary) do
    s = Aprsme.EncodingUtils.sanitize_string(binary)
    if is_binary(s), do: s, else: ""
  end

  defp sanitize_packet_strings(other), do: other

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

  defp normalize_data_type(%{data_type: data_type} = attrs) when is_atom(data_type) do
    %{attrs | data_type: to_string(data_type)}
  end

  defp normalize_data_type(attrs), do: attrs

  defp struct_to_map(%{__struct__: struct_type} = struct) do
    converted_map =
      struct
      |> Map.from_struct()
      |> Map.new(fn {k, v} -> {k, struct_to_map(v)} end)

    Map.put(converted_map, :__original_struct__, struct_type)
  end

  defp struct_to_map(value) when is_list(value) do
    Enum.map(value, &struct_to_map/1)
  end

  defp struct_to_map(value), do: value

  defp truncate_datetimes_to_second(%DateTime{} = dt), do: DateTime.truncate(dt, :second)
  defp truncate_datetimes_to_second({:ok, %DateTime{} = dt}), do: DateTime.truncate(dt, :second)

  defp truncate_datetimes_to_second(term) when is_map(term) and not is_struct(term) do
    Map.new(term, fn {k, v} -> {k, truncate_datetimes_to_second(v)} end)
  end

  defp truncate_datetimes_to_second(list) when is_list(list), do: Enum.map(list, &truncate_datetimes_to_second/1)
  defp truncate_datetimes_to_second(other), do: other

  defp normalize_numeric_types(attrs) do
    # Convert integer values to floats for float fields
    float_fields = [
      :temperature,
      :humidity,
      :wind_speed,
      :wind_gust,
      :pressure,
      :rain_1h,
      :rain_24h,
      :rain_since_midnight,
      :snow,
      :speed,
      :altitude
    ]

    Enum.reduce(float_fields, attrs, fn field, acc ->
      case Map.get(acc, field) do
        value when is_integer(value) -> Map.put(acc, field, value * 1.0)
        _ -> acc
      end
    end)
  end

  defp sanitize_raw_packet(attrs) do
    # Use the exact same sanitization approach as the original working code
    sanitize_packet_strings(attrs)
  end
end
