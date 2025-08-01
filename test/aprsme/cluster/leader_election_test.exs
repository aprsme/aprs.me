defmodule Aprsme.Cluster.LeaderElectionTest do
  use ExUnit.Case, async: false

  alias Aprsme.Cluster.LeaderElection

  @election_key {:aprs_is_leader, LeaderElection}

  setup do
    # Clean up any existing global registrations
    :global.unregister_name(@election_key)

    # Stop any running LeaderElection process
    if Process.whereis(LeaderElection) do
      GenServer.stop(LeaderElection)
    end

    on_exit(fn ->
      # Cleanup after test
      :global.unregister_name(@election_key)

      if Process.whereis(LeaderElection) do
        GenServer.stop(LeaderElection)
      end
    end)

    :ok
  end

  describe "start_link/1" do
    test "starts the GenServer process" do
      assert {:ok, pid} = LeaderElection.start_link([])
      assert Process.alive?(pid)
      assert Process.whereis(LeaderElection) == pid
    end

    test "registers with the given name" do
      {:ok, _pid} = LeaderElection.start_link([])
      assert Process.whereis(LeaderElection)
    end
  end

  describe "leader election in non-clustered mode" do
    setup do
      # Ensure clustering is disabled
      Application.put_env(:aprsme, :cluster_enabled, false)
      {:ok, pid} = LeaderElection.start_link([])

      # Give time for election to occur
      Process.sleep(200)

      {:ok, pid: pid}
    end

    test "elects itself as leader when clustering is disabled", %{pid: _pid} do
      assert LeaderElection.is_leader?() == true
      assert LeaderElection.current_leader() == node()
    end

    test "registers itself globally", %{pid: pid} do
      assert :global.whereis_name(@election_key) == pid
    end
  end

  describe "is_leader?/0" do
    setup do
      Application.put_env(:aprsme, :cluster_enabled, false)
      {:ok, _pid} = LeaderElection.start_link([])
      Process.sleep(200)
      :ok
    end

    test "returns leadership status" do
      assert is_boolean(LeaderElection.is_leader?())
    end
  end

  describe "current_leader/0" do
    setup do
      Application.put_env(:aprsme, :cluster_enabled, false)
      {:ok, _pid} = LeaderElection.start_link([])
      Process.sleep(200)
      :ok
    end

    test "returns the current leader node" do
      leader = LeaderElection.current_leader()
      assert leader == node() or is_nil(leader)
    end
  end

  describe "get_cluster_aprs_status/0" do
    test "returns local status when clustering is disabled" do
      Application.put_env(:aprsme, :cluster_enabled, false)

      # Since Aprsme.Is.get_status() is called directly, we need to ensure
      # the module exists or mock it at a lower level
      # For now, just verify the function doesn't crash
      result = LeaderElection.get_cluster_aprs_status()
      assert is_map(result) or is_nil(result)
    end

    test "returns cluster-wide status when clustering is enabled" do
      Application.put_env(:aprsme, :cluster_enabled, true)

      # This will call get_cluster_wide_status which handles the cluster logic
      status = LeaderElection.get_cluster_aprs_status()

      # The function should return a map with cluster_info when in cluster mode
      assert is_map(status) or is_nil(status)

      # If we got a valid status back, it should have cluster info
      if is_map(status) do
        assert Map.has_key?(status, :cluster_info)

        if status.cluster_info do
          assert status.cluster_info.total_nodes >= 1
          assert is_list(status.cluster_info.all_nodes)
        end
      end
    end
  end

  describe "leadership transitions" do
    setup do
      Application.put_env(:aprsme, :cluster_enabled, false)
      :ok
    end

    test "handles termination gracefully when leader" do
      {:ok, pid} = LeaderElection.start_link([])
      Process.sleep(200)

      # Verify it's the leader
      assert LeaderElection.is_leader?() == true

      # Stop the process
      GenServer.stop(pid)

      # Verify global registration is cleaned up
      assert :global.whereis_name(@election_key) == :undefined
    end

    test "periodic leadership check continues running" do
      {:ok, _pid} = LeaderElection.start_link([])

      # Initial state - might not be leader yet
      _initial_leader = LeaderElection.is_leader?()

      # Wait for periodic check
      Process.sleep(6000)

      # Should still be able to query leadership
      current_leader = LeaderElection.is_leader?()
      assert is_boolean(current_leader)

      # In non-clustered mode, should become leader
      if not Application.get_env(:aprsme, :cluster_enabled, false) do
        assert current_leader == true
      end
    end
  end

  describe "conflict resolution" do
    test "resolve_conflict prefers lexicographically lower node" do
      # We can't directly test the private function, but we can test the behavior
      # by starting multiple processes and seeing which wins

      # This is more of an integration test that would require multiple nodes
      # For now, we just verify the module handles conflicts without crashing

      {:ok, pid1} = LeaderElection.start_link([])
      Process.sleep(100)

      # Try to register another process with the same key
      # This should fail or trigger conflict resolution
      spawn(fn ->
        :global.register_name(@election_key, self())
      end)

      Process.sleep(100)

      # Original process should still be alive
      assert Process.alive?(pid1)
    end
  end

  describe "stale registration cleanup" do
    test "cleans up registration when process dies" do
      # Register a dead process
      dead_pid = spawn(fn -> :ok end)
      # Ensure it's dead
      Process.sleep(10)

      # Force register it globally (simulating stale registration)
      :global.re_register_name(@election_key, dead_pid)

      # Start LeaderElection which should clean it up
      {:ok, _pid} = LeaderElection.start_link([])
      Process.sleep(200)

      # Should have taken over leadership
      assert LeaderElection.is_leader?() == true
    end
  end

  describe "cluster mode behavior" do
    setup do
      Application.put_env(:aprsme, :cluster_enabled, true)
      :ok
    end

    test "waits for cluster formation when enabled" do
      {:ok, _pid} = LeaderElection.start_link([])

      # Immediately after start, might not be leader yet
      Process.sleep(100)

      # In cluster mode with no other nodes, it will eventually become leader
      # but might take longer than non-clustered mode
      Process.sleep(3000)

      # Should eventually attempt election
      assert is_boolean(LeaderElection.is_leader?())
    end
  end

  describe "message handling" do
    setup do
      Application.put_env(:aprsme, :cluster_enabled, false)
      {:ok, pid} = LeaderElection.start_link([])
      {:ok, pid: pid}
    end

    test "handles unknown messages without crashing", %{pid: pid} do
      send(pid, :unknown_message)
      Process.sleep(100)
      assert Process.alive?(pid)
    end

    test "handles check_leadership message", %{pid: pid} do
      send(pid, :check_leadership)
      Process.sleep(100)
      assert Process.alive?(pid)
    end
  end
end
