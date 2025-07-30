defmodule AprsmeWeb.MapLive.HistoricalLoader do
  @moduledoc """
  Handles progressive loading of historical APRS packets.
  """

  import Phoenix.Component, only: [assign: 3]

  alias Aprsme.Packets
  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.MapLive.DataBuilder
  alias Phoenix.LiveView
  alias Phoenix.LiveView.Socket

  @max_historical_packets 5000

  # Viewport-based loading limits by zoom level
  @zoom_packet_limits %{
    # Very low zoom (world/continent view) - show only major stations
    (1..5) => 100,
    # Low zoom (country/state view) - show moderate amount
    (6..8) => 500,
    # Medium zoom (city view) - show more detail
    (9..11) => 1500,
    # High zoom (neighborhood view) - show most packets
    (12..14) => 3000,
    # Very high zoom (street view) - show all available
    (15..20) => 5000
  }

  @doc """
  Start progressive historical loading based on zoom level.
  """
  @spec start_progressive_historical_loading(Socket.t()) :: Socket.t()
  def start_progressive_historical_loading(socket) do
    require Logger

    Logger.debug(
      "start_progressive_historical_loading called with zoom: #{socket.assigns.map_zoom}, bounds: #{inspect(socket.assigns.map_bounds)}"
    )

    # If we're already loading, just update the generation to cancel old requests
    if socket.assigns[:historical_loading] do
      Logger.debug("Already loading historical data, cancelling previous load")
    end

    # Increment generation to invalidate any in-flight loads
    new_generation = socket.assigns.loading_generation + 1

    # Cancel any pending batch tasks
    socket = cancel_pending_loads(socket)

    # Add a failsafe timeout to prevent infinite loading
    Process.send_after(self(), {:historical_loading_timeout, new_generation}, 30_000)

    # For high zoom levels, load everything in one batch for maximum speed
    zoom = socket.assigns.map_zoom || 5

    if zoom >= 10 do
      # High zoom - load everything at once for maximum speed
      Logger.debug("High zoom (#{zoom}), loading in single batch")

      socket
      |> assign(:loading_batch, 0)
      |> assign(:total_batches, 1)
      |> assign(:historical_loading, true)
      |> assign(:loading_generation, new_generation)
      |> load_historical_batch(0, new_generation)
    else
      # Low zoom - use progressive loading to prevent overwhelming
      total_batches = calculate_batch_count_for_zoom(zoom)

      # Start with first batch
      socket =
        socket
        |> assign(:loading_batch, 0)
        |> assign(:total_batches, total_batches)
        |> assign(:historical_loading, true)
        |> assign(:loading_generation, new_generation)
        |> load_historical_batch(0, new_generation)

      # Schedule remaining batches with generation check
      batch_refs =
        Enum.map(1..(total_batches - 1), fn batch_index ->
          Process.send_after(self(), {:load_historical_batch, batch_index, new_generation}, batch_index * 50)
        end)

      socket = assign(socket, :pending_batch_tasks, batch_refs)

      socket
    end
  end

  @doc """
  Load a specific historical batch.
  """
  @spec load_historical_batch(Socket.t(), integer(), integer() | nil) :: Socket.t()
  def load_historical_batch(socket, batch_offset, generation) do
    # Check generation if provided
    if generation && generation != socket.assigns.loading_generation do
      # Stale request, return unchanged socket
      socket
    else
      do_load_historical_batch(socket, batch_offset)
    end
  end

  @doc """
  Cancel pending batch load tasks.
  """
  @spec cancel_pending_loads(Socket.t()) :: Socket.t()
  def cancel_pending_loads(socket) do
    # Cancel any pending batch load messages
    Enum.each(socket.assigns.pending_batch_tasks, &Process.cancel_timer/1)
    assign(socket, :pending_batch_tasks, [])
  end

  # Private functions

  defp get_packet_limit_for_zoom(zoom) do
    @zoom_packet_limits
    |> Enum.find(fn {range, _} -> zoom in range end)
    |> case do
      {_, limit} -> limit
      nil -> @max_historical_packets
    end
  end

  defp do_load_historical_batch(%{assigns: %{map_bounds: nil}} = socket, _batch_offset), do: socket

  defp do_load_historical_batch(%{assigns: %{map_bounds: bounds}} = socket, batch_offset) do
    bounds_list = [
      bounds.west,
      bounds.south,
      bounds.east,
      bounds.north
    ]

    # Calculate zoom-based batch size - higher zoom = smaller batches for faster response
    zoom = socket.assigns.map_zoom || 5
    batch_size = calculate_batch_size_for_zoom(zoom)
    offset = batch_offset * batch_size

    # Get total packet limit based on zoom level
    max_packets_for_zoom = get_packet_limit_for_zoom(zoom)

    # Check if we've reached the zoom-based limit
    if offset >= max_packets_for_zoom do
      finish_historical_loading(socket)
    else
      # Adjust batch size if it would exceed the limit
      adjusted_batch_size = min(batch_size, max_packets_for_zoom - offset)

      packets_module = Application.get_env(:aprsme, :packets_module, Packets)

      historical_packets =
        try do
          if packets_module == Packets do
            # Use cached queries for better performance
            # Include zoom level in cache key for better cache efficiency
            params = %{
              bounds: bounds_list,
              limit: adjusted_batch_size,
              offset: offset,
              zoom: zoom
            }

            # Add callsign filter if tracking
            params = add_callsign_filter(params, socket.assigns.tracked_callsign)

            # Use the historical_hours setting from the UI dropdown with validation
            historical_hours = SharedPacketUtils.parse_historical_hours(socket.assigns.historical_hours || "1")
            params = Map.put(params, :hours_back, historical_hours)

            Packets.get_recent_packets(params)
          else
            # Fallback for testing
            packets_module.get_recent_packets(%{
              bounds: bounds,
              limit: batch_size,
              offset: offset
            })
          end
        rescue
          error ->
            require Logger

            Logger.error("Error loading historical packets: #{inspect(error)}")
            # Return empty list and notify user
            send(self(), {:show_error, "Failed to load historical data. Please try again."})
            []
        end

      if Enum.any?(historical_packets) do
        # Process this batch and send to frontend
        packet_data_list =
          try do
            DataBuilder.build_packet_data_list(historical_packets)
          rescue
            e ->
              require Logger

              Logger.error("Error building packet data list: #{inspect(e)}")
              []
          end

        # If tracking a callsign and this is the first batch, also load RF path stations
        socket =
          if socket.assigns.tracked_callsign != "" and batch_offset == 0 do
            load_rf_path_stations(socket, historical_packets)
          else
            socket
          end

        process_loaded_packets(socket, historical_packets, packet_data_list, batch_offset)
      else
        # No packets found - handle this as a completed batch to prevent infinite loading
        require Logger

        Logger.debug(
          "No historical packets found for batch #{batch_offset}, hours_back: #{Map.get(socket.assigns, :historical_hours, "1")}, callsign: #{Map.get(socket.assigns, :tracked_callsign, "")}"
        )

        total_batches = socket.assigns.total_batches || 1
        is_final_batch = batch_offset >= total_batches - 1

        # Update progress for user feedback
        socket = assign(socket, :loading_batch, batch_offset + 1)

        # Mark loading as complete if this was the final batch or if we're doing single-batch loading
        socket = update_loading_status(socket, is_final_batch)

        # Process any pending bounds update if loading is complete
        handle_pending_bounds(socket, is_final_batch)
      end
    end
  end

  defp add_callsign_filter(params, ""), do: params
  defp add_callsign_filter(params, callsign), do: Map.put(params, :callsign, callsign)

  defp process_loaded_packets(socket, _historical_packets, [], _batch_offset), do: socket

  defp process_loaded_packets(socket, historical_packets, packet_data_list, batch_offset) do
    # Check zoom level to decide between heat map and markers
    total_batches = socket.assigns.total_batches || 4
    is_final_batch = batch_offset >= total_batches - 1

    socket = handle_zoom_based_display(socket, historical_packets, packet_data_list, is_final_batch, batch_offset)

    # Update progress for user feedback
    socket = assign(socket, :loading_batch, batch_offset + 1)

    # Mark loading as complete if this was the final batch
    socket = update_loading_status(socket, is_final_batch)

    # Process any pending bounds update if loading is complete
    handle_pending_bounds(socket, is_final_batch)
  end

  # Handle low zoom (heat map)
  defp handle_zoom_based_display(
         %{assigns: %{map_zoom: zoom}} = socket,
         historical_packets,
         _packet_data_list,
         is_final_batch,
         _batch_offset
       )
       when zoom <= 8 do
    # For heat maps, store historical packets and update heat map when all batches are loaded
    new_historical = add_packets_to_historical(socket.assigns.historical_packets, historical_packets)

    # Apply memory limit
    new_historical = apply_memory_limit(new_historical, @max_historical_packets)

    socket = assign(socket, :historical_packets, new_historical)

    # If this is the final batch, update the heat map
    if is_final_batch do
      send_heat_map_for_current_bounds(socket)
    else
      socket
    end
  end

  # Handle high zoom (markers)
  defp handle_zoom_based_display(socket, _historical_packets, packet_data_list, is_final_batch, batch_offset) do
    # Use LiveView's efficient push_event for incremental updates
    LiveView.push_event(socket, "add_historical_packets_batch", %{
      packets: packet_data_list,
      batch: batch_offset,
      is_final: is_final_batch
    })
  end

  defp add_packets_to_historical(existing_historical, new_packets) do
    Enum.reduce(new_packets, existing_historical, fn packet, acc ->
      key = get_packet_key(packet)
      Map.put(acc, key, packet)
    end)
  end

  defp get_packet_key(%{id: id}), do: to_string(id)
  defp get_packet_key(%{"id" => id}), do: to_string(id)

  defp apply_memory_limit(packets, limit) when map_size(packets) > limit do
    SharedPacketUtils.prune_oldest_packets(packets, limit)
  end

  defp apply_memory_limit(packets, _limit), do: packets

  defp update_loading_status(socket, true), do: assign(socket, :historical_loading, false)
  defp update_loading_status(socket, false), do: socket

  defp handle_pending_bounds(%{assigns: %{pending_bounds: pending}} = socket, true) when not is_nil(pending) do
    send(self(), {:process_pending_bounds})
    socket
  end

  defp handle_pending_bounds(socket, _), do: socket

  # Consolidated zoom-based loading parameters
  @spec get_loading_params_for_zoom(integer()) :: {batch_size :: integer(), batch_count :: integer()}
  # Very zoomed in - load everything at once, minimal batches
  defp get_loading_params_for_zoom(zoom) when zoom >= 15, do: {500, 2}
  # Moderately zoomed in
  defp get_loading_params_for_zoom(zoom) when zoom >= 12, do: {500, 3}
  # High zoom - still load a lot
  defp get_loading_params_for_zoom(zoom) when zoom >= 10, do: {500, 4}
  # Medium zoom
  defp get_loading_params_for_zoom(zoom) when zoom >= 8, do: {100, 4}
  # Zoomed out
  defp get_loading_params_for_zoom(zoom) when zoom >= 5, do: {75, 5}
  # Very zoomed out - smaller batches, more of them
  defp get_loading_params_for_zoom(_), do: {50, 5}

  @spec calculate_batch_size_for_zoom(integer()) :: integer()
  defp calculate_batch_size_for_zoom(zoom) do
    {batch_size, _} = get_loading_params_for_zoom(zoom)
    batch_size
  end

  @spec calculate_batch_count_for_zoom(integer()) :: integer()
  defp calculate_batch_count_for_zoom(zoom) do
    {_, batch_count} = get_loading_params_for_zoom(zoom)
    batch_count
  end

  # All packet data building functions have been moved to AprsmeWeb.MapLive.DataBuilder

  # Placeholder for heat map function - this should be moved to DisplayManager
  defp finish_historical_loading(socket) do
    socket
    |> assign(:historical_loading, false)
    |> assign(:loading_batch, socket.assigns.total_batches)
    |> then(fn s ->
      if s.assigns.pending_bounds do
        send(self(), {:process_pending_bounds})
      end

      s
    end)
  end

  defp send_heat_map_for_current_bounds(socket) do
    # This function should be moved to DisplayManager module
    socket
  end

  defp load_rf_path_stations(socket, packets) do
    # Extract unique RF path stations from all packets
    rf_path_stations =
      packets
      |> Enum.flat_map(fn packet ->
        path = Map.get(packet, :path, "")
        AprsmeWeb.MapLive.RfPath.parse_rf_path(path)
      end)
      |> Enum.uniq()
      |> Enum.reject(&(&1 == ""))
      # Limit to prevent too many queries
      |> Enum.take(20)

    if Enum.any?(rf_path_stations) do
      # Schedule loading of RF path station packets
      Process.send_after(self(), {:load_rf_path_station_packets, rf_path_stations}, 100)
    end

    socket
  end
end
