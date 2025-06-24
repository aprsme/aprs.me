defmodule Aprsme.PacketPipelineSetup do
  @moduledoc """
  Handles the setup of the GenStage pipeline subscription after the supervisor starts.
  """
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    # Wait a moment for the pipeline to start, then set up the subscription
    Process.send_after(self(), :setup_subscription, 100)
    {:ok, %{}}
  end

  @impl true
  def handle_info(:setup_subscription, state) do
    # Set up the subscription between producer and consumer
    config = Application.get_env(:aprsme, :packet_pipeline, [])
    max_demand = config[:max_demand] || 50

    case GenStage.sync_subscribe(Aprsme.PacketConsumer, to: Aprsme.PacketProducer, max_demand: max_demand) do
      {:ok, _subscription} ->
        require Logger

        Logger.info("GenStage packet pipeline subscription established")
        {:noreply, state}

      {:error, reason} ->
        require Logger

        Logger.error("Failed to establish GenStage subscription: #{inspect(reason)}")
        # Retry after a delay
        Process.send_after(self(), :setup_subscription, 1000)
        {:noreply, state}
    end
  end
end
