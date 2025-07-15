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
    {:producer, %{demand: 0, buffer: [], max_buffer_size: opts[:max_buffer_size] || 1000}}
  end

  @impl true
  def handle_demand(incoming_demand, %{demand: demand, buffer: buffer} = state) do
    {events, remaining_buffer, remaining_demand} = dispatch_events(buffer, demand + incoming_demand)
    {:noreply, events, %{state | demand: remaining_demand, buffer: remaining_buffer}}
  end

  @impl true
  def handle_cast({:packet, packet_data}, %{demand: demand, buffer: buffer, max_buffer_size: max_size} = state) do
    if demand > 0 do
      # We have demand, send immediately
      {:noreply, [packet_data], %{state | demand: demand - 1}}
    else
      # No demand, buffer the packet
      new_buffer = [packet_data | buffer]

      buffer_size = length(new_buffer)

      if buffer_size > max_size do
        # Buffer is full, drop oldest packet
        Logger.warning("Packet buffer full, dropping oldest packet",
          buffer_status: %{
            current_size: buffer_size,
            max_size: max_size,
            dropped: 1
          }
        )

        {:noreply, [], %{state | buffer: Enum.take(new_buffer, max_size)}}
      else
        # Log when buffer is getting full
        if buffer_size > max_size * 0.8 do
          Logger.warning("Packet buffer approaching capacity",
            buffer_status: %{
              current_size: buffer_size,
              max_size: max_size,
              utilization: Float.round(buffer_size / max_size * 100, 1)
            }
          )
        end

        {:noreply, [], %{state | buffer: new_buffer}}
      end
    end
  end

  defp dispatch_events(buffer, demand) when demand > 0 and length(buffer) > 0 do
    {events, remaining} = Enum.split(buffer, demand)
    {events, remaining, demand - length(events)}
  end

  defp dispatch_events(buffer, demand) do
    {[], buffer, demand}
  end
end
