defmodule AprsmeWeb.MapLive.PacketStore do
  @moduledoc """
  Efficient packet storage for LiveView that stores only IDs in assigns
  and fetches full packet data on demand.
  """

  use GenServer

  require Logger

  @table_name :map_packet_store
  @ttl_ms to_timeout(hour: 2)
  @cleanup_interval_ms to_timeout(minute: 5)

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Store a packet and return its ID.
  """
  def store_packet(packet) do
    packet_id = generate_packet_id(packet)
    expires_at = System.monotonic_time(:millisecond) + @ttl_ms

    :ets.insert(@table_name, {packet_id, packet, expires_at})
    packet_id
  end

  @doc """
  Store multiple packets and return their IDs.
  """
  def store_packets(packets) when is_list(packets) do
    expires_at = System.monotonic_time(:millisecond) + @ttl_ms

    entries =
      Enum.map(packets, fn packet ->
        packet_id = generate_packet_id(packet)
        {packet_id, packet, expires_at}
      end)

    :ets.insert(@table_name, entries)
    Enum.map(entries, fn {id, _, _} -> id end)
  end

  @doc """
  Get a packet by ID.
  """
  def get_packet(packet_id) do
    case :ets.lookup(@table_name, packet_id) do
      [{^packet_id, packet, expires_at}] ->
        if System.monotonic_time(:millisecond) < expires_at do
          {:ok, packet}
        else
          # Expired, remove it
          :ets.delete(@table_name, packet_id)
          {:error, :not_found}
        end

      [] ->
        {:error, :not_found}
    end
  end

  @doc """
  Get multiple packets by IDs.
  """
  def get_packets(packet_ids) when is_list(packet_ids) do
    current_time = System.monotonic_time(:millisecond)

    packet_ids
    |> Enum.map(fn id ->
      case :ets.lookup(@table_name, id) do
        [{^id, packet, expires_at}] when current_time < expires_at ->
          {id, packet}

        _ ->
          nil
      end
    end)
    |> Enum.reject(&is_nil/1)
    |> Map.new()
  end

  @doc """
  Remove a packet by ID.
  """
  def remove_packet(packet_id) do
    :ets.delete(@table_name, packet_id)
  end

  @doc """
  Remove multiple packets by IDs.
  """
  def remove_packets(packet_ids) when is_list(packet_ids) do
    Enum.each(packet_ids, &:ets.delete(@table_name, &1))
  end

  @doc """
  Get statistics about the store.
  """
  def get_stats do
    %{
      total_packets: :ets.info(@table_name, :size),
      memory_bytes: :ets.info(@table_name, :memory) * :erlang.system_info(:wordsize)
    }
  end

  # GenServer callbacks

  @impl true
  def init(_opts) do
    # Create ETS table for packet storage
    :ets.new(@table_name, [
      :set,
      :public,
      :named_table,
      read_concurrency: true,
      write_concurrency: true
    ])

    # Schedule periodic cleanup
    schedule_cleanup()

    {:ok, %{}}
  end

  @impl true
  def handle_info(:cleanup, state) do
    cleanup_expired_packets()
    schedule_cleanup()
    {:noreply, state}
  end

  # Private functions

  defp generate_packet_id(packet) do
    # Generate a unique ID based on packet attributes
    sender = Map.get(packet, :sender, Map.get(packet, "sender", ""))
    timestamp = Map.get(packet, :received_at, Map.get(packet, "received_at", DateTime.utc_now()))

    # Create a deterministic ID
    :md5
    |> :crypto.hash("#{sender}_#{timestamp}")
    |> Base.encode16(case: :lower)
    |> binary_part(0, 16)
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @cleanup_interval_ms)
  end

  defp cleanup_expired_packets do
    current_time = System.monotonic_time(:millisecond)

    # Use match_delete for efficient bulk deletion
    match_spec = [{{:_, :_, :"$1"}, [{:<, :"$1", current_time}], [true]}]
    deleted = :ets.select_delete(@table_name, match_spec)

    if deleted > 0 do
      Logger.debug("Cleaned up #{deleted} expired packets from packet store")
    end
  end
end
