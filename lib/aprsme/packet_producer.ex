defmodule Aprsme.PacketProducer do
  @moduledoc """
  GenStage producer that handles incoming APRS packets and sends them to consumers
  for efficient batch processing.

  Uses `:queue` for O(1) enqueue/dequeue instead of lists, which avoids
  O(n) `Enum.take` on every buffer overflow during traffic bursts.

  Implements water-mark-based backpressure: when the buffer crosses the high
  water mark, signals Aprsme.Is to switch its TCP socket to passive mode.
  When demand drains the buffer below the low water mark, signals Is to resume.
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
    max_buffer_size = opts[:max_buffer_size] || 1000
    pipeline_config = Application.get_env(:aprsme, :packet_pipeline, [])
    high_ratio = Keyword.get(pipeline_config, :high_water_ratio, 0.8)
    low_ratio = Keyword.get(pipeline_config, :low_water_ratio, 0.3)

    {:producer,
     %{
       demand: 0,
       buffer: :queue.new(),
       buffer_size: 0,
       max_buffer_size: max_buffer_size,
       high_water_mark: trunc(max_buffer_size * high_ratio),
       low_water_mark: trunc(max_buffer_size * low_ratio),
       backpressure_active: false,
       is_monitor_ref: nil
     }}
  end

  @impl true
  def handle_demand(incoming_demand, %{demand: demand} = state) do
    {events, new_buffer, remaining_demand} = dispatch_events(state.buffer, demand + incoming_demand)
    new_buffer_size = state.buffer_size - length(events)

    new_state = %{state | demand: remaining_demand, buffer: new_buffer, buffer_size: new_buffer_size}
    new_state = maybe_deactivate_backpressure(new_state)

    {:noreply, events, new_state}
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
        new_state = %{state | buffer: :queue.in(packet_data, trimmed), buffer_size: max_size}
        new_state = maybe_activate_backpressure(new_state)
        {:noreply, [], new_state}
      else
        new_state = %{state | buffer: :queue.in(packet_data, buffer), buffer_size: new_size}
        new_state = maybe_activate_backpressure(new_state)
        {:noreply, [], new_state}
      end
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, _reason}, %{is_monitor_ref: ref} = state) do
    {:noreply, [], %{state | backpressure_active: false, is_monitor_ref: nil}}
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    {:noreply, [], state}
  end

  def handle_info(_msg, state) do
    {:noreply, [], state}
  end

  defp maybe_activate_backpressure(%{buffer_size: size, high_water_mark: high, backpressure_active: false} = state)
       when size >= high do
    case Process.whereis(Aprsme.Is) do
      nil ->
        state

      pid ->
        send(pid, {:backpressure, :activate})
        ref = Process.monitor(pid)

        :telemetry.execute(
          [:aprsme, :packet_producer, :backpressure],
          %{buffer_size: size},
          %{action: :activate}
        )

        %{state | backpressure_active: true, is_monitor_ref: ref}
    end
  end

  defp maybe_activate_backpressure(state), do: state

  defp maybe_deactivate_backpressure(%{buffer_size: size, low_water_mark: low, backpressure_active: true} = state)
       when size <= low do
    if state.is_monitor_ref, do: Process.demonitor(state.is_monitor_ref, [:flush])

    case Process.whereis(Aprsme.Is) do
      nil ->
        :ok

      pid ->
        send(pid, {:backpressure, :deactivate})
    end

    :telemetry.execute(
      [:aprsme, :packet_producer, :backpressure],
      %{buffer_size: size},
      %{action: :deactivate}
    )

    %{state | backpressure_active: false, is_monitor_ref: nil}
  end

  defp maybe_deactivate_backpressure(state), do: state

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
