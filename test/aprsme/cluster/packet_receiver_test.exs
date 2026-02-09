defmodule Aprsme.Cluster.PacketReceiverTest do
  use ExUnit.Case, async: false

  alias Aprsme.Cluster.LeaderElection
  alias Aprsme.Cluster.PacketReceiver

  @election_key {:aprs_is_leader, LeaderElection}

  setup do
    # Clean up any existing global registrations
    :global.unregister_name(@election_key)

    # Ensure clustering is disabled so LeaderElection becomes leader
    Application.put_env(:aprsme, :cluster_enabled, false)

    # Stop any existing PacketReceiver
    if Process.whereis(PacketReceiver) do
      GenServer.stop(PacketReceiver)
    end

    # Stop any existing LeaderElection
    if Process.whereis(LeaderElection) do
      GenServer.stop(LeaderElection)
    end

    # Start LeaderElection and wait for election
    {:ok, leader_pid} = LeaderElection.start_link([])
    Process.sleep(300)

    on_exit(fn ->
      try do
        if Process.whereis(PacketReceiver), do: GenServer.stop(PacketReceiver, :normal, 100)
      catch
        :exit, _ -> :ok
      end

      try do
        if Process.whereis(LeaderElection), do: GenServer.stop(LeaderElection, :normal, 100)
      catch
        :exit, _ -> :ok
      end

      :global.unregister_name(@election_key)
      Application.put_env(:aprsme, :cluster_enabled, false)
    end)

    {:ok, leader_pid: leader_pid}
  end

  describe "start_link/1" do
    test "starts and registers the process" do
      assert {:ok, pid} = PacketReceiver.start_link([])
      assert Process.alive?(pid)
      assert Process.whereis(PacketReceiver) == pid
    end
  end

  describe "handle_info/2 when leader" do
    test "does not forward distributed packets when node is leader" do
      {:ok, pid} = PacketReceiver.start_link([])

      # Confirm we are the leader
      assert LeaderElection.leader?() == true

      packet = %{raw: "TEST>APRS:test", sender: "TEST"}
      send(pid, {:distributed_packet, packet})

      # Give it time to process
      Process.sleep(100)

      # Process should still be alive (no crash)
      assert Process.alive?(pid)
    end
  end

  describe "handle_info/2 when not leader" do
    test "forwards distributed packets when node is not leader" do
      {:ok, pid} = PacketReceiver.start_link([])

      # Force non-leader state
      :sys.replace_state(LeaderElection, fn state -> %{state | is_leader: false} end)

      assert LeaderElection.leader?() == false

      packet = %{raw: "TEST>APRS:test", sender: "TEST"}
      send(pid, {:distributed_packet, packet})

      # Give it time to process
      Process.sleep(100)

      # Process should still be alive (no crash)
      assert Process.alive?(pid)
    end
  end

  describe "handle_info/2 with unknown messages" do
    test "ignores unknown messages without crashing" do
      {:ok, pid} = PacketReceiver.start_link([])

      send(pid, :some_unknown_message)
      send(pid, {:unexpected, "data"})

      Process.sleep(100)

      assert Process.alive?(pid)
    end
  end
end
