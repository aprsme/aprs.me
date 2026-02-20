defmodule AprsmeWeb.MapLive.PacketBatcherTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.MapLive.PacketBatcher

  setup do
    # Trap exits so linked batcher doesn't crash the test process
    Process.flag(:trap_exit, true)
    :ok
  end

  describe "start_link/1" do
    test "starts a batcher process" do
      {:ok, pid} = PacketBatcher.start_link(self())
      assert Process.alive?(pid)
      GenServer.stop(pid)
    end
  end

  describe "batching behavior" do
    test "delivers packets in batch after timeout" do
      {:ok, pid} = PacketBatcher.start_link(self())

      PacketBatcher.add_packet(pid, %{sender: "TEST-1"})
      PacketBatcher.add_packet(pid, %{sender: "TEST-2"})

      # Should receive batch after the 100ms timeout
      assert_receive {:packet_batch, packets}, 500
      assert length(packets) == 2
      assert Enum.at(packets, 0).sender == "TEST-1"
      assert Enum.at(packets, 1).sender == "TEST-2"

      GenServer.stop(pid)
    end

    test "delivers immediately when batch size reached" do
      {:ok, pid} = PacketBatcher.start_link(self())

      # Send 10 packets (batch size threshold)
      for i <- 1..10 do
        PacketBatcher.add_packet(pid, %{sender: "TEST-#{i}"})
      end

      # Should receive immediately (not waiting for timeout)
      assert_receive {:packet_batch, packets}, 200
      assert length(packets) == 10

      GenServer.stop(pid)
    end

    test "flush/1 delivers buffered packets immediately" do
      {:ok, pid} = PacketBatcher.start_link(self())

      PacketBatcher.add_packet(pid, %{sender: "TEST-1"})
      PacketBatcher.flush(pid)

      assert_receive {:packet_batch, [%{sender: "TEST-1"}]}, 200

      GenServer.stop(pid)
    end
  end

  describe "crash recovery" do
    test "batcher stops when parent dies" do
      # Start a temporary parent process that traps exits
      parent =
        spawn(fn ->
          Process.flag(:trap_exit, true)

          receive do
            :stop -> :ok
          end
        end)

      {:ok, batcher_pid} = PacketBatcher.start_link(parent)
      ref = Process.monitor(batcher_pid)

      # Kill the parent
      Process.exit(parent, :kill)

      # Batcher should stop
      assert_receive {:DOWN, ^ref, :process, ^batcher_pid, :normal}, 1000
    end
  end
end
