defmodule Aprsme.PacketConsumerPool do
  @moduledoc """
  Manages a pool of packet consumers for parallel processing.
  Each consumer subscribes to the producer with its own demand.
  """
  use Supervisor

  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    config = Application.get_env(:aprsme, :packet_pipeline, [])

    # Number of parallel consumers (default to 3 for better throughput)
    num_consumers = opts[:num_consumers] || config[:num_consumers] || 3

    # Each consumer gets a portion of the max demand
    max_demand_per_consumer = div(config[:max_demand] || 250, num_consumers)

    children =
      for index <- 1..num_consumers do
        %{
          id: {Aprsme.PacketConsumer, index},
          start:
            {Aprsme.PacketConsumer, :start_link,
             [
               [
                 batch_size: config[:batch_size] || 100,
                 batch_timeout: config[:batch_timeout] || 1000,
                 max_demand: max_demand_per_consumer,
                 subscribe_to: [{Aprsme.PacketProducer, max_demand: max_demand_per_consumer}]
               ]
             ]}
        }
      end

    Supervisor.init(children, strategy: :one_for_one)
  end
end
