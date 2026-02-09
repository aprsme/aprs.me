defmodule Aprsme.Cluster.PacketDistributorTest do
  use ExUnit.Case, async: false

  alias Aprsme.Cluster.LeaderElection
  alias Aprsme.Cluster.PacketDistributor

  @test_packet %{raw: "TEST>APRS:test packet", sender: "TEST"}

  setup do
    # Ensure cluster_enabled is false initially
    Application.put_env(:aprsme, :cluster_enabled, false)

    # Stop any running LeaderElection process
    if Process.whereis(LeaderElection) do
      try do
        GenServer.stop(LeaderElection, :normal, 100)
      catch
        :exit, _ -> :ok
      end
    end

    # Start LeaderElection in non-clustered mode
    {:ok, _} = LeaderElection.start_link([])
    # Wait for election to complete (100ms timer + processing)
    Process.sleep(300)

    on_exit(fn ->
      try do
        if Process.whereis(LeaderElection) do
          GenServer.stop(LeaderElection, :normal, 100)
        end
      catch
        :exit, _ -> :ok
      end

      # Reset cluster_enabled to false
      Application.put_env(:aprsme, :cluster_enabled, false)
    end)

    :ok
  end

  describe "distribute_packet/1" do
    test "does not distribute when cluster is disabled" do
      # cluster_enabled is false from setup
      PacketDistributor.subscribe()

      PacketDistributor.distribute_packet(@test_packet)

      refute_receive {:distributed_packet, _}, 200
    end

    test "distributes packet when cluster is enabled and node is leader" do
      Application.put_env(:aprsme, :cluster_enabled, true)
      # LeaderElection started in non-clustered mode becomes leader quickly
      Process.sleep(300)

      PacketDistributor.subscribe()

      PacketDistributor.distribute_packet(@test_packet)

      assert_receive {:distributed_packet, packet}, 500
      assert packet.raw == "TEST>APRS:test packet"
      assert packet.sender == "TEST"
    end
  end

  describe "subscribe/0" do
    test "subscribes to cluster:packets topic" do
      PacketDistributor.subscribe()

      Phoenix.PubSub.broadcast(
        Aprsme.PubSub,
        "cluster:packets",
        {:distributed_packet, @test_packet}
      )

      assert_receive {:distributed_packet, _}, 500
    end
  end

  describe "handle_distributed_packet/1" do
    test "processes a distributed packet without crashing" do
      # StreamingPacketsPubSub and PacketStore are already running in test
      result = PacketDistributor.handle_distributed_packet({:distributed_packet, @test_packet})

      # The function logs and returns :ok from Logger.debug
      assert result == :ok
    end
  end
end
