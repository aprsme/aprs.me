defmodule Aprsme.PacketProducer do
  @moduledoc """
  GenStage producer that handles incoming APRS packets and sends them to consumers
  for efficient batch processing.

  Uses `:queue` for O(1) enqueue/dequeue instead of lists, which avoids
  O(n) `Enum.take` on every buffer overflow during traffic bursts.
  """
  use GenStage

  require Logger

  def start_link(opts \\ []) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def submit_packet(packet_data) do
    GenStage.cast(__MODULE__, {:packet, packet_data})
  end

  @impl true
  def init(opts) do
    {:producer, %{demand: 0, buffer: :queue.new(), buffer_size: 0, max_buffer_size: opts[:max_buffer_size] || 1000}}
  end

  @impl true
  def handle_demand(incoming_demand, %{demand: demand} = state) do
    {events, new_buffer, remaining_demand} = dispatch_events(state.buffer, demand + incoming_demand)

    {:noreply, events,
     %{state | demand: remaining_demand, buffer: new_buffer, buffer_size: state.buffer_size - length(events)}}
  end

  @impl true
  def handle_cast(
        {:packet, packet_data},
        %{demand: demand, buffer: buffer, buffer_size: size, max_buffer_size: max_size} = state
      ) do
    if demand > 0 do
      {:noreply, [packet_data], %{state | demand: demand - 1}}
    else
      new_size = size + 1

      if new_size > max_size do
        dropped = new_size - max_size

        Logger.warning("Packet buffer full, dropping #{dropped} oldest packet(s)",
          buffer_size: max_size,
          dropped: dropped
        )

        :telemetry.execute(
          [:aprsme, :packet_producer, :buffer_overflow],
          %{dropped: dropped, buffer_size: max_size},
          %{}
        )

        # Drop oldest (front of queue), add new to back — O(1) amortized
        {_, trimmed} = :queue.out(buffer)
        {:noreply, [], %{state | buffer: :queue.in(packet_data, trimmed), buffer_size: max_size}}
      else
        {:noreply, [], %{state | buffer: :queue.in(packet_data, buffer), buffer_size: new_size}}
      end
    end
  end

  defp dispatch_events(buffer, demand) when demand > 0 do
    if :queue.is_empty(buffer) do
      {[], buffer, demand}
    else
      {front, remaining} = :queue.split(min(demand, :queue.len(buffer)), buffer)
      events = :queue.to_list(front)
      {events, remaining, demand - length(events)}
    end
  end

  defp dispatch_events(buffer, demand) do
    {[], buffer, demand}
  end
end
