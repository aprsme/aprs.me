defmodule Aprsme.PacketConsumer do
  @moduledoc """
  GenStage consumer that batches APRS packets and inserts them into the database
  efficiently to reduce database load.
  """
  use GenStage

  alias Aprsme.LogSanitizer
  alias Aprsme.Repo

  require Logger

  def start_link(opts \\ []) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    batch_size = opts[:batch_size] || 100
    batch_timeout = opts[:batch_timeout] || 1000
    # Maximum batch size to prevent unbounded memory growth
    max_batch_size = opts[:max_batch_size] || 1000

    # Start a timer for batch processing
    timer = Process.send_after(self(), :process_batch, batch_timeout)

    {:consumer,
     %{
       batch: [],
       batch_size: batch_size,
       batch_timeout: batch_timeout,
       max_batch_size: max_batch_size,
       timer: timer
     }}
  end

  @impl true
  def handle_events(events, _from, %{batch: batch, batch_size: batch_size, max_batch_size: max_batch_size} = state) do
    new_batch = batch ++ events
    new_batch_length = length(new_batch)

    cond do
      new_batch_length >= max_batch_size ->
        # If batch exceeds maximum size, process immediately and drop excess
        {process_batch, drop_batch} = Enum.split(new_batch, max_batch_size)
        process_batch(process_batch)

        # Log warning about dropping packets (sanitized)
        if length(drop_batch) > 0 do
          Logger.warning("Dropped #{length(drop_batch)} packets due to batch size limit",
            batch_info:
              LogSanitizer.log_data(
                dropped_count: length(drop_batch),
                processed_count: length(process_batch),
                reason: "batch_size_limit_exceeded"
              )
          )
        end

        {:noreply, [], %{state | batch: []}}

      new_batch_length >= batch_size ->
        # Process the batch immediately
        process_batch(new_batch)
        {:noreply, [], %{state | batch: []}}

      true ->
        # Add to batch and wait for more
        {:noreply, [], %{state | batch: new_batch}}
    end
  end

  @impl true
  def handle_info(:process_batch, %{batch: batch, batch_timeout: timeout, max_batch_size: max_batch_size} = state) do
    if length(batch) > 0 do
      # Check if batch size is concerning
      if length(batch) > max_batch_size * 0.8 do
        Logger.warning("Batch size approaching limit",
          batch_status:
            LogSanitizer.log_data(
              current_size: length(batch),
              max_size: max_batch_size,
              utilization_percent: trunc(length(batch) / max_batch_size * 100)
            )
        )
      end

      process_batch(batch)
    end

    # Start a new timer
    timer = Process.send_after(self(), :process_batch, timeout)
    {:noreply, [], %{state | batch: [], timer: timer}}
  end

  defp process_batch(packets) do
    require Logger

    # Monitor memory usage before processing
    {memory_before, _} = :erlang.statistics(:runtime)
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

    # Monitor memory usage after processing
    {memory_after, _} = :erlang.statistics(:runtime)
    memory_diff = memory_after - memory_before

    # Force garbage collection if memory usage is high
    # 50MB threshold
    if memory_diff > 50_000 do
      :erlang.garbage_collect()

      Logger.warning("High memory usage detected, forced garbage collection",
        memory_info:
          LogSanitizer.log_data(
            memory_diff_bytes: memory_diff,
            batch_size: length(packets),
            gc_forced: true
          )
      )
    end

    :telemetry.execute(
      [
        :aprsme,
        :packet_pipeline,
        :batch
      ],
      %{
        count: length(packets),
        success: success_count,
        error: error_count,
        duration_ms: duration,
        memory_diff: memory_diff
      },
      %{}
    )

    Logger.info("Batch processing completed",
      batch_result:
        LogSanitizer.log_data(
          packet_count: length(packets),
          duration_ms: duration,
          success_count: success_count,
          error_count: error_count,
          memory_diff_bytes: memory_diff
        )
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
      Map.put(attrs, :device_identifier, device_identifier)
    end)
    |> sanitize_packet_strings()
    |> Map.put(:inserted_at, current_time)
    |> Map.put(:updated_at, current_time)
    |> Map.delete(:id)
    |> Map.delete("id")
    # Remove embedded field for batch insert
    |> Map.delete(:data_extended)
    |> normalize_numeric_types()
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
      nil ->
        nil

      n when is_float(n) ->
        Float.round(n, 6)
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

  defp sanitize_packet_strings(value), do: Aprsme.EncodingUtils.sanitize_packet_strings(value)

  defp to_float(value), do: Aprsme.EncodingUtils.to_float(value)

  defp normalize_data_type(attrs), do: Aprsme.EncodingUtils.normalize_data_type(attrs)

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
end
