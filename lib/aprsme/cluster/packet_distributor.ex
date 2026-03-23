defmodule Aprsme.Cluster.PacketDistributor do
  @moduledoc """
  Distributes APRS packets from the leader node to all cluster members.
  This ensures all nodes can serve real-time updates via LiveView while
  only the leader maintains the APRS-IS connection.
  """
  use GenServer

  alias Aprsme.Cluster.LeaderElection

  @pubsub_topic "cluster:packets"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def distribute_packet(packet) do
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    if cluster_enabled and LeaderElection.leader_cached?() do
      Phoenix.PubSub.broadcast(
        Aprsme.PubSub,
        @pubsub_topic,
        {:distributed_packet, packet}
      )
    end
  end

  def subscribe do
    Phoenix.PubSub.subscribe(Aprsme.PubSub, @pubsub_topic)
  end

  def handle_distributed_packet({:distributed_packet, packet}) do
    Aprsme.StreamingPacketsPubSub.broadcast_packet(packet)
    Aprsme.SpatialPubSub.broadcast_packet(packet)

    :ok
  end

  @impl true
  def init(_opts) do
    Phoenix.PubSub.subscribe(Aprsme.PubSub, @pubsub_topic)
    {:ok, %{}}
  end

  @impl true
  def terminate(_reason, _state) do
    Phoenix.PubSub.unsubscribe(Aprsme.PubSub, @pubsub_topic)
    :ok
  end
end
