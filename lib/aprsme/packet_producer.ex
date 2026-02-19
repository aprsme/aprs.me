defmodule Aprsme.PacketProducer do
  @moduledoc """
  GenStage producer that handles incoming APRS packets and sends them to consumers
  for efficient batch processing.
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
    {:producer, %{demand: 0, buffer: [], buffer_size: 0, max_buffer_size: opts[:max_buffer_size] || 1000}}
  end

  @impl true
  def handle_demand(incoming_demand, %{demand: demand, buffer: buffer} = state) do
    {events, remaining_buffer, remaining_demand} = dispatch_events(buffer, demand + incoming_demand)
    remaining_size = state.buffer_size - length(events)
    {:noreply, events, %{state | demand: remaining_demand, buffer: remaining_buffer, buffer_size: remaining_size}}
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

        {:noreply, [], %{state | buffer: [packet_data | Enum.take(buffer, max_size - 1)], buffer_size: max_size}}
      else
        {:noreply, [], %{state | buffer: [packet_data | buffer], buffer_size: new_size}}
      end
    end
  end

  defp dispatch_events(buffer, demand) when demand > 0 and buffer != [] do
    {events, remaining} = Enum.split(buffer, demand)
    {events, remaining, demand - length(events)}
  end

  defp dispatch_events(buffer, demand) do
    {[], buffer, demand}
  end
end
