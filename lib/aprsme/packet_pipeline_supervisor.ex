defmodule Aprsme.PacketPipelineSupervisor do
  @moduledoc """
  Supervisor for the GenStage pipeline that handles packet processing.
  """
  use Supervisor

  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    config = Application.get_env(:aprsme, :packet_pipeline, [])

    # Configure producer with correct buffer size from config
    producer_spec = {Aprsme.PacketProducer, max_buffer_size: config[:max_buffer_size] || 1000}

    # Use consumer pool for better throughput
    consumer_pool_spec = {Aprsme.PacketConsumerPool, num_consumers: config[:num_consumers] || 3}

    children = [producer_spec, consumer_pool_spec]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
