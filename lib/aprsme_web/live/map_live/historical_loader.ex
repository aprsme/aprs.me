defmodule AprsmeWeb.MapLive.HistoricalLoader do
  @moduledoc """
  Handles progressive loading of historical APRS packets.
  """

  import Phoenix.Component, only: [assign: 3]

  alias Aprsme.CachedQueries
  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.MapLive.DataBuilder
  alias Phoenix.LiveView
  alias Phoenix.LiveView.Socket

  @max_historical_packets 5000

  @doc """
  Start progressive historical loading based on zoom level.
  """
  @spec start_progressive_historical_loading(Socket.t()) :: Socket.t()
  def start_progressive_historical_loading(socket) do
    require Logger

    Logger.debug(
      "start_progressive_historical_loading called with zoom: #{socket.assigns.map_zoom}, bounds: #{inspect(socket.assigns.map_bounds)}"
    )

    # Increment generation to invalidate any in-flight loads
    new_generation = socket.assigns.loading_generation + 1

    # Cancel any pending batch tasks
    socket = cancel_pending_loads(socket)

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

  defp do_load_historical_batch(socket, batch_offset) do
    if socket.assigns.map_bounds do
      bounds = [
        socket.assigns.map_bounds.west,
        socket.assigns.map_bounds.south,
        socket.assigns.map_bounds.east,
        socket.assigns.map_bounds.north
      ]

      # Calculate zoom-based batch size - higher zoom = smaller batches for faster response
      zoom = socket.assigns.map_zoom || 5
      batch_size = calculate_batch_size_for_zoom(zoom)
      offset = batch_offset * batch_size

      packets_module = Application.get_env(:aprsme, :packets_module, Aprsme.Packets)

      historical_packets =
        try do
          if packets_module == Aprsme.Packets do
            # Use cached queries for better performance
            # Include zoom level in cache key for better cache efficiency
            params = %{
              bounds: bounds,
              limit: batch_size,
              offset: offset,
              zoom: zoom
            }

            # Add callsign filter if tracking
            params =
              if socket.assigns.tracked_callsign == "" do
                params
              else
                Map.put(params, :callsign, socket.assigns.tracked_callsign)
              end

            # Use the historical_hours setting from the UI dropdown with validation
            historical_hours = SharedPacketUtils.parse_historical_hours(socket.assigns.historical_hours || "1")
            params = Map.put(params, :hours_back, historical_hours)

            CachedQueries.get_recent_packets_cached(params)
          else
            # Fallback for testing
            packets_module.get_recent_packets_optimized(%{
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

        if Enum.any?(packet_data_list) do
          # Check zoom level to decide between heat map and markers
          total_batches = socket.assigns.total_batches || 4
          is_final_batch = batch_offset >= total_batches - 1

          socket =
            if socket.assigns.map_zoom <= 8 do
              # For heat maps, store historical packets and update heat map when all batches are loaded
              # Add packets to historical_packets assign
              new_historical =
                Enum.reduce(historical_packets, socket.assigns.historical_packets, fn packet, acc ->
                  key = if Map.has_key?(packet, :id), do: to_string(packet.id), else: to_string(packet["id"])
                  Map.put(acc, key, packet)
                end)

              # Apply memory limit
              new_historical =
                if map_size(new_historical) > @max_historical_packets do
                  SharedPacketUtils.prune_oldest_packets(new_historical, @max_historical_packets)
                else
                  new_historical
                end

              socket = assign(socket, :historical_packets, new_historical)

              # If this is the final batch, update the heat map
              if is_final_batch do
                send_heat_map_for_current_bounds(socket)
              else
                socket
              end
            else
              # Use LiveView's efficient push_event for incremental updates
              LiveView.push_event(socket, "add_historical_packets_batch", %{
                packets: packet_data_list,
                batch: batch_offset,
                is_final: is_final_batch
              })
            end

          # Update progress for user feedback
          socket = assign(socket, :loading_batch, batch_offset + 1)

          # Mark loading as complete if this was the final batch
          socket =
            if is_final_batch do
              assign(socket, :historical_loading, false)
            else
              socket
            end

          # Process any pending bounds update if loading is complete
          if is_final_batch && socket.assigns.pending_bounds do
            send(self(), {:process_pending_bounds})
          end

          socket
        else
          socket
        end
      else
        socket
      end
    else
      socket
    end
  end

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
  defp send_heat_map_for_current_bounds(socket) do
    # This function should be moved to DisplayManager module
    socket
  end
end
