defmodule AprsmeWeb.MapLive.PacketManager do
  @moduledoc """
  Optimized packet management for LiveView with reduced memory usage.

  This module provides efficient packet storage and retrieval optimized for
  LiveView performance by:
  - Storing minimal data in LiveView assigns
  - Using sliding window for packet management
  - Providing just-in-time data fetching
  - Batching operations for better performance
  """

  alias AprsmeWeb.MapLive.PacketStore

  # Maximum packets to keep in memory per category
  @max_visible_packets 1000
  @max_historical_packets 2000
  # Clean up when over limit by this amount
  @packet_cleanup_threshold 50

  @doc """
  Initialize packet management state for a socket.
  Returns minimal state optimized for memory usage.
  """
  def init_packet_state do
    %{
      visible_packet_ids: [],
      historical_packet_ids: [],
      packet_count: %{visible: 0, historical: 0},
      last_cleanup: System.monotonic_time(:millisecond)
    }
  end

  @doc """
  Add packets to visible storage with memory management.
  Returns updated state and packet IDs for broadcasting.
  """
  def add_visible_packets(%{visible_packet_ids: visible_ids, packet_count: packet_count} = packet_state, packets)
      when is_list(packets) do
    packet_ids = PacketStore.store_packets(packets)
    new_visible_ids = packet_ids ++ visible_ids

    {managed_ids, cleanup_needed} = apply_memory_limits(new_visible_ids, @max_visible_packets)

    updated_state = %{
      packet_state
      | visible_packet_ids: managed_ids,
        packet_count: Map.put(packet_count, :visible, length(managed_ids))
    }

    final_state = maybe_cleanup(updated_state, cleanup_needed, :visible)
    {final_state, packet_ids}
  end

  @doc """
  Add packets to historical storage with sliding window management.
  """
  def add_historical_packets(
        %{historical_packet_ids: historical_ids, packet_count: packet_count} = packet_state,
        packets
      )
      when is_list(packets) do
    packet_ids = PacketStore.store_packets(packets)
    new_historical_ids = packet_ids ++ historical_ids

    {managed_ids, cleanup_needed} = apply_memory_limits(new_historical_ids, @max_historical_packets)

    updated_state = %{
      packet_state
      | historical_packet_ids: managed_ids,
        packet_count: Map.put(packet_count, :historical, length(managed_ids))
    }

    final_state = maybe_cleanup(updated_state, cleanup_needed, :historical)
    {final_state, packet_ids}
  end

  @doc """
  Get visible packets for rendering. Only fetches data when needed.
  """
  def get_visible_packets(packet_state, limit \\ @max_visible_packets) do
    packet_ids = Enum.take(packet_state.visible_packet_ids, limit)
    packet_ids |> PacketStore.get_packets() |> Map.values()
  end

  @doc """
  Get historical packets for rendering.
  """
  def get_historical_packets(packet_state, limit \\ @max_historical_packets) do
    packet_ids = Enum.take(packet_state.historical_packet_ids, limit)
    packet_ids |> PacketStore.get_packets() |> Map.values()
  end

  @doc """
  Remove packets matching a predicate (e.g., expired packets).
  """
  def remove_packets_where(packet_state, predicate_fn) do
    # Get visible packets and apply predicate
    visible_packets = get_visible_packets(packet_state)
    {keep_visible, remove_visible} = Enum.split_with(visible_packets, predicate_fn)

    # Get historical packets and apply predicate
    historical_packets = get_historical_packets(packet_state)
    {keep_historical, remove_historical} = Enum.split_with(historical_packets, predicate_fn)

    # Remove from storage
    remove_ids = extract_packet_ids(remove_visible ++ remove_historical)
    PacketStore.remove_packets(remove_ids)

    # Update state with remaining IDs
    keep_visible_ids = extract_packet_ids(keep_visible)
    keep_historical_ids = extract_packet_ids(keep_historical)

    %{
      packet_state
      | visible_packet_ids: keep_visible_ids,
        historical_packet_ids: keep_historical_ids,
        packet_count: %{
          visible: length(keep_visible_ids),
          historical: length(keep_historical_ids)
        }
    }
  end

  @doc """
  Get current memory usage statistics.
  """
  def get_memory_stats(packet_state) do
    store_stats = PacketStore.get_stats()

    %{
      visible_count: packet_state.packet_count.visible,
      historical_count: packet_state.packet_count.historical,
      total_stored: store_stats.total_packets,
      memory_bytes: store_stats.memory_bytes,
      avg_packet_size:
        if(store_stats.total_packets > 0,
          do: store_stats.memory_bytes / store_stats.total_packets,
          else: 0
        )
    }
  end

  # Private functions

  defp apply_memory_limits(packet_ids, max_count) do
    if length(packet_ids) > max_count + @packet_cleanup_threshold do
      # Keep most recent packets
      managed_ids = Enum.take(packet_ids, max_count)
      {managed_ids, true}
    else
      {packet_ids, false}
    end
  end

  defp maybe_cleanup(state, false, _type), do: state

  defp maybe_cleanup(%{last_cleanup: last_cleanup} = state, true, _type) do
    current_time = System.monotonic_time(:millisecond)

    if current_time - last_cleanup > 30_000 do
      %{state | last_cleanup: current_time}
    else
      state
    end
  end

  defp extract_packet_ids(packets) do
    Enum.map(packets, fn packet ->
      packet[:id] || packet["id"] || generate_packet_id(packet)
    end)
  end

  defp generate_packet_id(packet) do
    # Generate deterministic ID from packet data
    sender = packet[:sender] || packet["sender"] || ""
    timestamp = packet[:received_at] || packet["received_at"] || DateTime.utc_now()
    "#{sender}_#{DateTime.to_unix(timestamp, :microsecond)}"
  end
end
