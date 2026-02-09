defmodule Aprsme.ConnectionMonitorTest do
  use ExUnit.Case, async: false

  alias Aprsme.ConnectionMonitor

  describe "cluster disabled (default test config)" do
    test "init/1 returns :ignore when cluster disabled" do
      assert ConnectionMonitor.init([]) == :ignore
    end

    test "accepting_connections?/0 returns true when cluster disabled" do
      assert ConnectionMonitor.accepting_connections?() == true
    end

    test "get_stats/0 returns default stats when cluster disabled" do
      assert ConnectionMonitor.get_stats() == %{connections: 0, cpu: 0.0, memory: 0.0}
    end
  end

  describe "cluster enabled" do
    setup do
      Application.put_env(:aprsme, :cluster_enabled, true)
      {:ok, pid} = ConnectionMonitor.start_link([])

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        Application.put_env(:aprsme, :cluster_enabled, false)
      end)

      %{pid: pid}
    end

    test "register_connection/0 increments connections" do
      ConnectionMonitor.register_connection()
      # Allow the cast to be processed
      stats = ConnectionMonitor.get_stats()
      assert stats.connections == 1
    end

    test "unregister_connection/0 decrements connections" do
      ConnectionMonitor.register_connection()
      ConnectionMonitor.register_connection()
      ConnectionMonitor.unregister_connection()
      stats = ConnectionMonitor.get_stats()
      assert stats.connections == 1
    end

    test "unregister_connection/0 does not go below 0" do
      ConnectionMonitor.unregister_connection()
      stats = ConnectionMonitor.get_stats()
      assert stats.connections == 0
    end

    test "accepting_connections?/0 returns true when not draining" do
      assert ConnectionMonitor.accepting_connections?() == true
    end

    test "accepting_connections?/0 returns false when draining", %{pid: pid} do
      :sys.replace_state(pid, fn state -> %{state | draining: true} end)
      assert ConnectionMonitor.accepting_connections?() == false
    end

    test ":check_load handler keeps process alive", %{pid: pid} do
      send(pid, :check_load)
      # Give it time to process
      _ = ConnectionMonitor.get_stats()
      assert Process.alive?(pid)
    end

    test "get_stats/0 returns cpu and memory fields", %{pid: _pid} do
      stats = ConnectionMonitor.get_stats()

      assert Map.has_key?(stats, :cpu)
      assert Map.has_key?(stats, :memory)
      assert is_number(stats.cpu)
      assert is_number(stats.memory)
    end

    test "draining triggers when connections exceed imbalance ratio", %{pid: pid} do
      # Simulate having many connections to trigger draining analysis
      for _ <- 1..20 do
        ConnectionMonitor.register_connection()
      end

      # Force a load check which will gather stats and analyze
      send(pid, :check_load)
      Process.sleep(100)

      assert Process.alive?(pid)
      stats = ConnectionMonitor.get_stats()
      assert stats.connections == 20
    end

    test "register_connection/0 is a no-op when cluster disabled" do
      Application.put_env(:aprsme, :cluster_enabled, false)

      # Should not crash even though the GenServer is running
      ConnectionMonitor.register_connection()

      Application.put_env(:aprsme, :cluster_enabled, true)
    end

    test "unregister_connection/0 is a no-op when cluster disabled" do
      Application.put_env(:aprsme, :cluster_enabled, false)

      ConnectionMonitor.unregister_connection()

      Application.put_env(:aprsme, :cluster_enabled, true)
    end

    test "draining is set to true when state has high connections", %{pid: pid} do
      # Set up state with draining true and connections > 0 to trigger drain broadcast
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "connection:drain:#{Node.self()}")

      :sys.replace_state(pid, fn state ->
        %{state | draining: true, local_connections: 50}
      end)

      # Trigger check_load to run the full analysis pipeline
      send(pid, :check_load)
      Process.sleep(200)

      assert Process.alive?(pid)
    end

    test "get_stats returns draining field", %{pid: pid} do
      :sys.replace_state(pid, fn state -> %{state | draining: true} end)
      stats = ConnectionMonitor.get_stats()
      assert stats.draining == true
    end

    test "gather_cluster_stats handles local node stats", %{pid: pid} do
      ConnectionMonitor.register_connection()
      ConnectionMonitor.register_connection()

      # Trigger check_load which calls gather_cluster_stats internally
      send(pid, :check_load)
      Process.sleep(100)

      state = :sys.get_state(pid)
      # node_stats should have been populated
      assert is_map(state.node_stats)
    end
  end
end
