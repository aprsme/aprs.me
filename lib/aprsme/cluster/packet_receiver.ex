defmodule Aprsme.Cluster.PacketReceiver do
  @moduledoc """
  Receives distributed packets from the cluster leader on non-leader nodes.
  Ensures all nodes can serve real-time updates even though only the leader
  processes APRS packets.
  """
  use GenServer

  alias Aprsme.Cluster.LeaderElection
  alias Aprsme.Cluster.PacketDistributor

  require Logger

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    # Subscribe to distributed packets
    PacketDistributor.subscribe()

    Logger.info("Started packet receiver on node #{node()}")

    {:ok, %{}}
  end

  @impl true
  def handle_info({:distributed_packet, packet}, state) do
    # Only process if we're not the leader (leader already processed locally)
    if !LeaderElection.leader?() do
      PacketDistributor.handle_distributed_packet({:distributed_packet, packet})
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
