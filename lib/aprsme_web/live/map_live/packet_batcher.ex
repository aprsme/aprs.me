defmodule AprsmeWeb.MapLive.PacketBatcher do
  @moduledoc """
  Batches incoming packets for efficient processing and rendering.
  Reduces the number of updates sent to the client by grouping packets.
  """

  use GenServer

  require Logger

  @batch_size 10
  # milliseconds
  @batch_timeout 100

  defstruct [:parent_pid, :buffer, :timer_ref]

  @doc """
  Start the packet batcher for a LiveView process.
  """
  def start_link(parent_pid) do
    GenServer.start_link(__MODULE__, parent_pid)
  end

  @doc """
  Add a packet to the batch queue.
  """
  def add_packet(batcher_pid, packet) do
    GenServer.cast(batcher_pid, {:add_packet, packet})
  end

  @doc """
  Force immediate processing of all buffered packets.
  """
  def flush(batcher_pid) do
    GenServer.cast(batcher_pid, :flush)
  end

  # GenServer callbacks

  @impl true
  def init(parent_pid) do
    # Monitor parent LiveView process
    Process.monitor(parent_pid)

    {:ok,
     %__MODULE__{
       parent_pid: parent_pid,
       buffer: [],
       timer_ref: nil
     }}
  end

  @impl true
  def handle_cast({:add_packet, packet}, state) do
    # Add packet to buffer
    new_buffer = [packet | state.buffer]

    # Cancel existing timer if any
    state = cancel_timer(state)

    # Check if we should process immediately or wait
    if length(new_buffer) >= @batch_size do
      # Process immediately
      process_batch(new_buffer, state.parent_pid)
      {:noreply, %{state | buffer: [], timer_ref: nil}}
    else
      # Set timer for batch timeout
      timer_ref = Process.send_after(self(), :batch_timeout, @batch_timeout)
      {:noreply, %{state | buffer: new_buffer, timer_ref: timer_ref}}
    end
  end

  @impl true
  def handle_cast(:flush, state) do
    state = cancel_timer(state)

    if state.buffer != [] do
      process_batch(state.buffer, state.parent_pid)
    end

    {:noreply, %{state | buffer: [], timer_ref: nil}}
  end

  @impl true
  def handle_info(:batch_timeout, state) do
    if state.buffer != [] do
      process_batch(state.buffer, state.parent_pid)
    end

    {:noreply, %{state | buffer: [], timer_ref: nil}}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) when pid == state.parent_pid do
    # Parent LiveView process died, stop the batcher
    {:stop, :normal, state}
  end

  # Private functions

  defp cancel_timer(%{timer_ref: nil} = state), do: state

  defp cancel_timer(%{timer_ref: ref} = state) do
    Process.cancel_timer(ref)
    %{state | timer_ref: nil}
  end

  defp process_batch(packets, parent_pid) do
    # Reverse to maintain chronological order
    packets = Enum.reverse(packets)

    # Send batch to parent LiveView
    send(parent_pid, {:packet_batch, packets})

    Logger.debug("Processing batch of #{length(packets)} packets")
  end
end
