defmodule Aprsme.PacketConsumer do
  @moduledoc """
  GenStage consumer that batches APRS packets and inserts them into the database
  efficiently to reduce database load.
  """
  use GenStage

  alias Aprs.Types.ParseError
  alias Aprsme.LogSanitizer
  alias Aprsme.Repo

  require Logger

  def start_link(opts \\ []) do
    # Allow unnamed consumers for pool usage
    name = opts[:name]

    if name do
      GenStage.start_link(__MODULE__, opts, name: name)
    else
      GenStage.start_link(__MODULE__, opts)
    end
  end

  @impl true
  def init(opts) do
    batch_size = opts[:batch_size] || 100
    batch_timeout = opts[:batch_timeout] || 1000
    # Maximum batch size to prevent unbounded memory growth
    max_batch_size = opts[:max_batch_size] || 1000

    # Start a timer for batch processing
    timer = Process.send_after(self(), :process_batch, batch_timeout)

    # Extract subscription options if provided
    subscribe_to = opts[:subscribe_to] || [{Aprsme.PacketProducer, max_demand: opts[:max_demand] || 250}]

    {:consumer,
     %{
       batch: [],
       batch_size: batch_size,
       batch_timeout: batch_timeout,
       max_batch_size: max_batch_size,
       timer: timer
     }, subscribe_to: subscribe_to}
  end

  @impl true
  def handle_events(events, _from, state) do
    handle_batch_update(events, state)
  end

  # Pattern matching for batch handling with optimized list operations
  defp handle_batch_update(events, %{batch: batch} = state) do
    # Optimize: Keep batch in reverse order for O(1) prepending
    # Only reverse when processing
    new_batch = Enum.reverse(events, batch)
    new_batch_length = batch_length(batch) + length(events)

    handle_batch_by_size(new_batch, new_batch_length, state)
  end

  # Pattern matching for different batch size scenarios
  defp handle_batch_by_size(batch, length, %{max_batch_size: max} = state) when length >= max do
    handle_oversized_batch(batch, max, state)
  end

  defp handle_batch_by_size(batch, length, %{batch_size: size} = state) when length >= size do
    handle_full_batch(batch, state)
  end

  defp handle_batch_by_size(batch, _length, state) do
    handle_partial_batch(batch, state)
  end

  # Handle oversized batch with pattern matching
  defp handle_oversized_batch(batch, max_size, state) do
    # Reverse once for processing
    reversed_batch = Enum.reverse(batch)
    {process_batch, drop_batch} = Enum.split(reversed_batch, max_size)

    process_batch(process_batch)
    log_dropped_packets(drop_batch, process_batch)

    new_state = reset_batch_timer(state)
    {:noreply, [], new_state}
  end

  # Handle full batch
  defp handle_full_batch(batch, state) do
    # Reverse once for processing
    process_batch(Enum.reverse(batch))

    new_state = reset_batch_timer(state)
    {:noreply, [], new_state}
  end

  # Handle partial batch
  defp handle_partial_batch(batch, state) do
    # Keep batch in reverse order
    {:noreply, [], %{state | batch: batch}}
  end

  # Helper functions with pattern matching
  defp reset_batch_timer(%{timer: nil} = state) do
    new_timer = Process.send_after(self(), :process_batch, state.batch_timeout)
    %{state | batch: [], timer: new_timer}
  end

  defp reset_batch_timer(%{timer: timer} = state) do
    Process.cancel_timer(timer)
    new_timer = Process.send_after(self(), :process_batch, state.batch_timeout)
    %{state | batch: [], timer: new_timer}
  end

  # Efficient batch length calculation (cached if possible)
  defp batch_length([]), do: 0
  defp batch_length(batch), do: length(batch)

  # Pattern matching for logging
  defp log_dropped_packets([], _), do: :ok

  defp log_dropped_packets(dropped, processed) do
    Logger.warning("Dropped #{length(dropped)} packets due to batch size limit",
      batch_info:
        LogSanitizer.log_data(
          dropped_count: length(dropped),
          processed_count: length(processed),
          reason: "batch_size_limit_exceeded"
        )
    )
  end

  @impl true
  # Pattern matching for empty batch
  def handle_info(:process_batch, %{batch: []} = state) do
    # Just restart the timer for empty batch
    new_state = start_batch_timer(state)
    {:noreply, [], new_state}
  end

  # Pattern matching for non-empty batch
  def handle_info(:process_batch, %{batch: batch} = state) when is_list(batch) do
    check_batch_utilization(batch, state.max_batch_size)
    # Remember to reverse the batch since we keep it in reverse order
    process_batch(Enum.reverse(batch))

    new_state = start_batch_timer(%{state | batch: []})
    {:noreply, [], new_state}
  end

  # Helper to start batch timer
  defp start_batch_timer(%{batch_timeout: timeout} = state) do
    timer = Process.send_after(self(), :process_batch, timeout)
    %{state | timer: timer}
  end

  # Pattern matching for batch utilization check
  defp check_batch_utilization(batch, max_size) when length(batch) > max_size * 0.8 do
    Logger.warning("Batch size approaching limit",
      batch_status:
        LogSanitizer.log_data(
          current_size: length(batch),
          max_size: max_size,
          utilization_percent: trunc(length(batch) / max_size * 100)
        )
    )
  end

  defp check_batch_utilization(_, _), do: :ok

  defp process_batch(packets) do
    require Logger

    # Monitor memory usage before processing (only this process)
    {:memory, memory_before} = Process.info(self(), :memory)
    start_time = System.monotonic_time(:millisecond)

    # Chunk size optimized for PostgreSQL work_mem=16MB
    # Using smaller batches to reduce memory pressure
    chunk_size = Application.get_env(:aprsme, :packet_pipeline)[:batch_size] || 100

    # Use Stream for memory-efficient processing
    # Process and reduce in one pass to avoid materializing the entire list
    {success_count, error_count} =
      packets
      |> Stream.chunk_every(chunk_size)
      |> Stream.map(&process_chunk/1)
      |> Enum.reduce({0, 0}, fn {success, error}, {total_success, total_error} ->
        {total_success + success, total_error + error}
      end)

    end_time = System.monotonic_time(:millisecond)
    duration = end_time - start_time

    # Monitor memory usage after processing (only this process)
    {:memory, memory_after} = Process.info(self(), :memory)
    memory_diff = memory_after - memory_before

    # Get current process memory info
    process_info = Process.info(self(), [:memory, :heap_size, :total_heap_size])

    # Force garbage collection if memory usage is high
    # 50MB threshold for memory diff (per process)
    # 100MB threshold for process memory
    if memory_diff > 52_428_800 or process_info[:memory] > 104_857_600 do
      :erlang.garbage_collect()

      Logger.warning("High memory usage detected, forced garbage collection",
        memory_info:
          LogSanitizer.log_data(
            memory_diff_bytes: memory_diff,
            process_memory: process_info[:memory],
            heap_size: process_info[:heap_size],
            total_heap_size: process_info[:total_heap_size],
            batch_size: length(packets),
            gc_forced: true
          )
      )
    end

    # Only do minor GC after very large batches to prevent memory accumulation
    # This should rarely trigger with batch_size of 100
    if length(packets) > 1000 do
      :erlang.garbage_collect(self(), type: :minor)
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

    # Logger.info("Batch processing completed",
    #   batch_result:
    #     LogSanitizer.log_data(
    #       packet_count: length(packets),
    #       duration_ms: duration,
    #       success_count: success_count,
    #       error_count: error_count,
    #       memory_diff_bytes: memory_diff
    #     )
    # )
  end

  defp process_chunk(packets) do
    # Use Stream for memory-efficient packet preparation
    packet_stream =
      packets
      |> Stream.map(&prepare_packet_for_insert/1)
      |> Stream.map(&truncate_datetimes_to_second/1)
      |> Stream.reject(&is_nil/1)

    # Separate valid and invalid packets using Stream
    {valid_packets, invalid_count} =
      Enum.reduce(packet_stream, {[], 0}, fn packet, {valid_acc, invalid_acc} ->
        if valid_packet?(packet) do
          {[packet | valid_acc], invalid_acc}
        else
          {valid_acc, invalid_acc + 1}
        end
      end)

    # Reverse to maintain order
    valid_packets = Enum.reverse(valid_packets)

    # Insert valid packets in batch
    # Optimized for PostgreSQL with synchronous_commit=off
    insert_opts = [
      # Don't return IDs for better performance
      returning: false,
      # Skip conflicts to avoid blocking
      on_conflict: :nothing,
      # Increased timeout for large batches
      timeout: 60_000,
      # Use placeholders for better performance with large batches
      placeholders: length(valid_packets) > 100
    ]

    case Repo.insert_all(Aprsme.Packet, valid_packets, insert_opts) do
      {:error, error} ->
        Logger.error("Batch insert failed: #{inspect(error)}")

        # Log sample packet to help debug field issues
        if length(valid_packets) > 0 do
          sample_packet = List.first(valid_packets)
          Logger.error("Sample packet fields: #{inspect(Map.keys(sample_packet))}")
          Logger.error("Sample packet data: #{inspect(sample_packet)}")
        end

        {0, length(packets)}

      {inserted_count, _} ->
        # Broadcast successfully inserted packets to StreamingPacketsPubSub
        broadcast_packets_async(valid_packets)

        {inserted_count, invalid_count}
    end
  end

  # Broadcast packets asynchronously to avoid blocking
  defp broadcast_packets_async(packets) do
    Task.start(fn ->
      try do
        cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

        Enum.each(packets, fn packet_attrs ->
          # Convert back to a format suitable for broadcasting
          packet = %{
            sender: packet_attrs[:sender],
            latitude: packet_attrs[:lat],
            longitude: packet_attrs[:lon],
            received_at: packet_attrs[:received_at],
            data_type: packet_attrs[:data_type],
            altitude: packet_attrs[:altitude],
            speed: packet_attrs[:speed],
            course: packet_attrs[:course],
            comment: packet_attrs[:comment]
          }

          if cluster_enabled do
            # Use cluster distributor to broadcast to all nodes
            Aprsme.Cluster.PacketDistributor.distribute_packet(packet)
          else
            # Normal single-node broadcasting
            Aprsme.StreamingPacketsPubSub.broadcast_packet(packet)
            # Also broadcast to SpatialPubSub for viewport-based filtering
            Aprsme.SpatialPubSub.broadcast_packet(packet)
          end
        end)
      rescue
        error ->
          Logger.error("Failed to broadcast packets asynchronously: #{inspect(error)}")
      end
    end)
  end

  defp prepare_packet_for_insert(packet_data) do
    # Always set received_at timestamp to ensure consistency
    current_time = DateTime.truncate(DateTime.utc_now(), :microsecond)
    packet_data = Map.put(packet_data, :received_at, current_time)

    # Convert to map before storing to avoid struct conversion issues
    attrs = struct_to_map(packet_data)

    # Extract additional data from the parsed packet including raw packet
    attrs = Aprsme.Packet.extract_additional_data(attrs, attrs[:raw_packet] || "")

    # Detect and set item/object fields
    attrs = detect_item_or_object(attrs)

    # Sanitize packet data to prevent database field overflow
    attrs = Aprsme.PacketSanitizer.sanitize_packet(attrs)

    # Normalize data_type to string if it's an atom
    attrs = normalize_data_type(attrs)

    # Apply the same processing as the original store_packet function
    attrs
    |> convert_coordinate_field_names()
    |> convert_field_names()
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
    # Create PostGIS geometry for location field BEFORE filtering
    |> create_location_geometry()
    # Remove all non-schema fields - do this LAST to ensure all processing is done
    |> remove_non_schema_fields()
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

  # Helper function to remove fields that exist in parser output but not in database schema
  defp remove_non_schema_fields(attrs) do
    # Use whitelist approach - only keep fields that are in our schema
    Aprsme.PacketFieldWhitelist.filter_fields(attrs)
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

  # Detect if packet is an item or object and set appropriate fields
  defp detect_item_or_object(attrs) do
    cond do
      # Check for itemname field from parser
      Map.has_key?(attrs, :itemname) or Map.has_key?(attrs, "itemname") ->
        item_name = Map.get(attrs, :itemname) || Map.get(attrs, "itemname")

        attrs
        |> Map.put(:item_name, item_name)
        |> Map.put(:is_item, true)
        |> Map.delete(:itemname)
        |> Map.delete("itemname")

      # Check for object data type and extract object name from data_extended
      attrs[:data_type] == "object" or attrs["data_type"] == "object" ->
        object_name = extract_object_name(attrs)

        attrs
        |> Map.put(:object_name, object_name)
        |> Map.put(:is_object, true)

      # Check information field for object format (starts with ;)
      is_binary(attrs[:information_field]) and String.starts_with?(attrs[:information_field], ";") ->
        # Extract object name from information field
        object_name = extract_object_name_from_info_field(attrs[:information_field])

        attrs
        |> Map.put(:object_name, object_name)
        |> Map.put(:is_object, true)

      true ->
        attrs
    end
  end

  defp extract_object_name(attrs) do
    case attrs[:data_extended] do
      %{name: name} when is_binary(name) -> name
      %{"name" => name} when is_binary(name) -> name
      _ -> nil
    end
  end

  defp extract_object_name_from_info_field(info_field) do
    # Object format: ;OBJECTNAM*DDHHMMz...
    case Regex.run(~r/^;([A-Z0-9 ]{9})\*/, info_field) do
      [_, name] -> String.trim(name)
      _ -> nil
    end
  end

  # Convert field names that don't match our schema
  defp convert_field_names(attrs) do
    attrs
    |> then(fn a ->
      # Convert aprs_messaging? to aprs_messaging
      case Map.get(a, :aprs_messaging?) || Map.get(a, "aprs_messaging?") do
        nil ->
          a

        value ->
          a
          |> Map.put(:aprs_messaging, value)
          |> Map.delete(:aprs_messaging?)
          |> Map.delete("aprs_messaging?")
      end
    end)
    |> then(fn a ->
      # Convert timestamp from integer to string if needed
      case Map.get(a, :timestamp) do
        timestamp when is_integer(timestamp) ->
          Map.put(a, :timestamp, Integer.to_string(timestamp))

        _ ->
          a
      end
    end)
  end

  # Convert latitude/longitude to lat/lon if present at top level
  defp convert_coordinate_field_names(attrs) do
    attrs
    |> then(fn a ->
      case Map.get(a, :latitude) do
        nil -> a
        lat -> a |> Map.put(:lat, lat) |> Map.delete(:latitude)
      end
    end)
    |> then(fn a ->
      case Map.get(a, :longitude) do
        nil -> a
        lon -> a |> Map.put(:lon, lon) |> Map.delete(:longitude)
      end
    end)
  end

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

  defp struct_to_map(%{__struct__: ParseError} = error) do
    # Handle ParseError specially to avoid Access behavior issues
    %{
      error_code: error.error_code,
      message: error.message,
      __original_struct__: ParseError
    }
  end

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
  defp truncate_datetimes_to_second({:error, _reason}), do: nil

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
      :altitude,
      :posresolution,
      :rain_midnight
    ]

    Enum.reduce(float_fields, attrs, fn field, acc ->
      case Map.get(acc, field) do
        value when is_integer(value) -> Map.put(acc, field, value * 1.0)
        _ -> acc
      end
    end)
  end
end
