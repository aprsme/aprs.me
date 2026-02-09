defmodule Aprsme.Cluster.PacketDistributor do
  @moduledoc """
  Distributes APRS packets from the leader node to all cluster members.
  This ensures all nodes can serve real-time updates via LiveView while
  only the leader maintains the APRS-IS connection.
  """
  require Logger

  @pubsub_topic "cluster:packets"

  def distribute_packet(packet) do
    # Only distribute if clustering is enabled and we're the leader
    cluster_enabled = Application.get_env(:aprsme, :cluster_enabled, false)

    if cluster_enabled and Aprsme.Cluster.LeaderElection.leader?() do
      # Broadcast to all nodes including self
      Phoenix.PubSub.broadcast(
        Aprsme.PubSub,
        @pubsub_topic,
        {:distributed_packet, packet}
      )

      Logger.debug("Distributed packet #{packet.raw} to cluster")
    end
  end

  def subscribe do
    Phoenix.PubSub.subscribe(Aprsme.PubSub, @pubsub_topic)
  end

  def handle_distributed_packet({:distributed_packet, packet}) do
    # Broadcast to local LiveView clients
    Aprsme.StreamingPacketsPubSub.broadcast_packet(packet)

    # Update packet store for LiveView
    AprsmeWeb.MapLive.PacketStore.store_packet(packet)

    Logger.debug("Received distributed packet on node #{node()}")
  end
end
