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

    children = [
      {Aprsme.PacketProducer, max_buffer_size: config[:max_buffer_size] || 1000},
      {Aprsme.PacketConsumer, batch_size: config[:batch_size] || 100, batch_timeout: config[:batch_timeout] || 1000}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
