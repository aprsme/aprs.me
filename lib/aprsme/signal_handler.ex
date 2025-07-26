defmodule Aprsme.SignalHandler do
  @moduledoc """
  Handles OS signals like SIGTERM for graceful shutdown.
  """

  use GenServer

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    # Install signal handler for SIGTERM
    case :os.set_signal(:sigterm, :handle) do
      :ok ->
        Logger.info("SIGTERM signal handler installed")

      error ->
        Logger.warning("Failed to install SIGTERM handler: #{inspect(error)}")
    end

    {:ok, %{}}
  end

  # Handle SIGTERM signal
  def handle_info({:signal, :sigterm}, state) do
    Logger.info("Received SIGTERM signal, initiating graceful shutdown...")

    # Trigger graceful shutdown
    spawn(fn ->
      Aprsme.ShutdownHandler.shutdown()
    end)

    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.debug("SignalHandler received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end
end
