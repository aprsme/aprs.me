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
    # Use dynamic batch sizing from system monitor
    initial_batch_size = Aprsme.SystemMonitor.get_recommended_batch_size()
    batch_timeout = opts[:batch_timeout] || 500
    # Maximum batch size to prevent unbounded memory growth
    max_batch_size = opts[:max_batch_size] || 2000

    # Start a timer for batch processing
    timer = Process.send_after(self(), :process_batch, batch_timeout)

    # Schedule periodic batch size adjustment
    Process.send_after(self(), :adjust_batch_size, 5_000)

    {:consumer,
     %{
       batch: [],
       batch_size: initial_batch_size,
       batch_timeout: batch_timeout,
       max_batch_size: max_batch_size,
       timer: timer,
       last_adjustment: System.monotonic_time(:millisecond)
     }}
  end

  @impl true
  def handle_events(events, _from, %{batch: batch, batch_size: _batch_size, max_batch_size: max_batch_size} = state) do
    # Get current recommended batch size
    current_batch_size = Aprsme.SystemMonitor.get_recommended_batch_size()
    state = %{state | batch_size: current_batch_size}

    # Debug logging
    Logger.debug(
      "PacketConsumer received #{length(events)} events, current batch: #{length(batch)}, batch_size threshold: #{current_batch_size}"
    )

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

      # Process immediately if we reach 80% of target batch size to improve responsiveness
      new_batch_length >= current_batch_size * 0.8 ->
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

  @impl true
  def handle_info(:adjust_batch_size, state) do
    # Get current system metrics and recommended batch size
    new_batch_size = Aprsme.SystemMonitor.get_recommended_batch_size()

    if new_batch_size != state.batch_size do
      Logger.info("Adjusting batch size based on system load",
        batch_adjustment:
          LogSanitizer.log_data(
            old_size: state.batch_size,
            new_size: new_batch_size,
            reason: "system_load_adaptation"
          )
      )
    end

    # Schedule next adjustment
    Process.send_after(self(), :adjust_batch_size, 5_000)

    {:noreply, [], %{state | batch_size: new_batch_size}}
  end

  defp process_batch(packets) do
    require Logger

    # Monitor memory usage before processing
    {memory_before, _} = :erlang.statistics(:runtime)
    start_time = System.monotonic_time(:millisecond)

    # Use fixed batch size for consistent performance
    batch_size = 200

    Logger.debug("Processing batch of #{length(packets)} packets with insert chunk size: #{batch_size}")

    results =
      packets
      |> Enum.chunk_every(batch_size)
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
    # Get current timestamp once for the entire batch
    current_time = DateTime.truncate(DateTime.utc_now(), :second)
    start_time = System.monotonic_time(:millisecond)

    # Prepare packets for batch insertion with optimized processing
    {valid_packets, invalid_count} = prepare_packets_batch(packets, current_time)

    # Skip database operation if no valid packets
    if Enum.empty?(valid_packets) do
      {0, invalid_count}
    else
      # Use simple insert options for reliability
      insert_options = [
        returning: false,
        on_conflict: :nothing,
        timeout: 15_000
      ]

      # Insert valid packets in batch
      result = Repo.insert_all(Aprsme.Packet, valid_packets, insert_options)

      # Record performance metrics for optimization
      end_time = System.monotonic_time(:millisecond)
      _duration = end_time - start_time

      case result do
        {:error, error} ->
          Logger.error("Batch insert failed: #{inspect(error)}")
          {0, length(packets)}

        {inserted_count, _} ->
          {inserted_count, invalid_count}
      end
    end
  end

  # Optimized batch preparation with reduced allocations and processing
  defp prepare_packets_batch(packets, current_time) do
    packets
    |> Enum.reduce({[], 0}, fn packet_data, {valid_acc, invalid_count} ->
      case prepare_packet_for_insert_fast(packet_data, current_time) do
        nil -> {valid_acc, invalid_count + 1}
        attrs -> {[attrs | valid_acc], invalid_count}
      end
    end)
    |> then(fn {valid_packets, invalid_count} -> {Enum.reverse(valid_packets), invalid_count} end)
  end

  # Fast packet preparation with minimal processing overhead
  defp prepare_packet_for_insert_fast(packet_data, current_time) do
    # Convert to map efficiently
    attrs = if is_struct(packet_data), do: Map.from_struct(packet_data), else: packet_data

    # Essential processing only - skip expensive operations
    attrs
    |> Map.put(:received_at, current_time)
    |> Map.put(:inserted_at, current_time)
    |> Map.put(:updated_at, current_time)
    |> extract_essential_fields()
    |> create_location_geometry_fast()
    |> validate_essential_fields()
  rescue
    # Return nil for invalid packets
    _error -> nil
  end

  # Extract only essential fields for INSERT performance
  defp extract_essential_fields(attrs) do
    # Get device identifier efficiently
    device_identifier = Aprsme.DeviceParser.extract_device_identifier(attrs)

    # Extract position efficiently
    {lat, lon} = extract_position_fast(attrs)

    %{
      sender: get_required_field(attrs, :sender),
      destination: get_field(attrs, :destination),
      path: get_field(attrs, :path),
      information_field: get_field(attrs, :information_field),
      data_type: normalize_data_type_fast(get_field(attrs, :data_type)),
      base_callsign: extract_base_callsign_fast(get_required_field(attrs, :sender)),
      ssid: extract_ssid_fast(get_required_field(attrs, :sender)),
      lat: lat,
      lon: lon,
      has_position: lat != nil and lon != nil,
      received_at: attrs[:received_at],
      inserted_at: attrs[:inserted_at],
      updated_at: attrs[:updated_at],
      device_identifier: device_identifier,
      raw_packet: get_field(attrs, :raw_packet),
      symbol_code: get_field(attrs, :symbol_code),
      symbol_table_id: get_field(attrs, :symbol_table_id),
      comment: get_field(attrs, :comment),
      region: get_field(attrs, :region)
    }
  end

  # Fast position extraction with minimal processing
  defp extract_position_fast(attrs) do
    cond do
      attrs[:lat] && attrs[:lon] -> {attrs[:lat], attrs[:lon]}
      attrs["lat"] && attrs["lon"] -> {attrs["lat"], attrs["lon"]}
      true -> {nil, nil}
    end
  end

  # Fast data type normalization
  defp normalize_data_type_fast(data_type) when is_atom(data_type), do: Atom.to_string(data_type)
  defp normalize_data_type_fast(data_type), do: data_type

  # Fast callsign parsing
  defp extract_base_callsign_fast(sender) when is_binary(sender) do
    case String.split(sender, "-", parts: 2) do
      [base | _] -> base
      _ -> sender
    end
  end

  defp extract_base_callsign_fast(_), do: nil

  defp extract_ssid_fast(sender) when is_binary(sender) do
    case String.split(sender, "-", parts: 2) do
      [_, ssid] -> ssid
      _ -> nil
    end
  end

  defp extract_ssid_fast(_), do: nil

  # Fast field access with fallbacks
  defp get_required_field(attrs, key) do
    attrs[key] || attrs[Atom.to_string(key)] || ""
  end

  defp get_field(attrs, key) do
    attrs[key] || attrs[Atom.to_string(key)]
  end

  # Fast location geometry creation (only if needed)
  defp create_location_geometry_fast(%{lat: lat, lon: lon} = attrs) when is_number(lat) and is_number(lon) do
    Map.put(attrs, :location, %Geo.Point{coordinates: {lon, lat}, srid: 4326})
  end

  defp create_location_geometry_fast(attrs), do: attrs

  # Fast validation - only check critical fields
  defp validate_essential_fields(%{sender: sender} = attrs) when sender != nil and sender != "", do: attrs
  defp validate_essential_fields(_), do: nil
end
