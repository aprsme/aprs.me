defmodule Aprsme.PacketProducerTest do
  use ExUnit.Case, async: true

  alias Aprsme.PacketProducer

  describe "init/1" do
    test "initializes with default max_buffer_size" do
      {:producer, state} = PacketProducer.init([])
      assert state.demand == 0
      assert state.buffer_size == 0
      assert state.max_buffer_size == 1000
    end

    test "initializes with custom max_buffer_size" do
      {:producer, state} = PacketProducer.init(max_buffer_size: 500)
      assert state.max_buffer_size == 500
    end

    test "initializes with empty :queue buffer" do
      {:producer, state} = PacketProducer.init([])
      assert :queue.is_queue(state.buffer)
      assert :queue.is_empty(state.buffer)
    end
  end

  describe "handle_cast {:packet, _} with demand > 0" do
    test "dispatches packet immediately when demand exists" do
      state = %{
        demand: 3,
        buffer: :queue.new(),
        buffer_size: 0,
        max_buffer_size: 1000,
        high_water_mark: 800,
        low_water_mark: 300,
        backpressure_active: false,
        is_monitor_ref: nil
      }

      {:noreply, events, new_state} = PacketProducer.handle_cast({:packet, %{sender: "TEST"}}, state)

      assert events == [%{sender: "TEST"}]
      assert new_state.demand == 2
    end
  end

  describe "handle_cast {:packet, _} with demand == 0" do
    test "buffers packet when no demand" do
      state = %{
        demand: 0,
        buffer: :queue.new(),
        buffer_size: 0,
        max_buffer_size: 1000,
        high_water_mark: 800,
        low_water_mark: 300,
        backpressure_active: false,
        is_monitor_ref: nil
      }

      {:noreply, [], new_state} = PacketProducer.handle_cast({:packet, %{sender: "TEST"}}, state)

      assert new_state.buffer_size == 1
      assert :queue.len(new_state.buffer) == 1
    end

    test "drops oldest packet when buffer overflows" do
      # Fill buffer to max
      buffer =
        Enum.reduce(1..3, :queue.new(), fn i, q ->
          :queue.in(%{sender: "PACKET#{i}"}, q)
        end)

      state = %{
        demand: 0,
        buffer: buffer,
        buffer_size: 3,
        max_buffer_size: 3,
        high_water_mark: 2,
        low_water_mark: 1,
        backpressure_active: false,
        is_monitor_ref: nil
      }

      {:noreply, [], new_state} =
        PacketProducer.handle_cast({:packet, %{sender: "NEW"}}, state)

      # Buffer size should still be max
      assert new_state.buffer_size == 3

      # Oldest packet (PACKET1) should be dropped, NEW should be present
      items = :queue.to_list(new_state.buffer)
      senders = Enum.map(items, & &1.sender)
      refute "PACKET1" in senders
      assert "NEW" in senders
      assert "PACKET2" in senders
      assert "PACKET3" in senders
    end
  end

  describe "handle_demand/2" do
    test "dispatches buffered packets in FIFO order" do
      buffer =
        Enum.reduce(1..5, :queue.new(), fn i, q ->
          :queue.in(%{sender: "P#{i}"}, q)
        end)

      state = %{
        demand: 0,
        buffer: buffer,
        buffer_size: 5,
        max_buffer_size: 1000,
        high_water_mark: 800,
        low_water_mark: 300,
        backpressure_active: false,
        is_monitor_ref: nil
      }

      {:noreply, events, new_state} = PacketProducer.handle_demand(3, state)

      # Should dispatch first 3 (FIFO order)
      assert length(events) == 3
      assert Enum.map(events, & &1.sender) == ["P1", "P2", "P3"]
      assert new_state.buffer_size == 2
      assert new_state.demand == 0
    end

    test "dispatches all available when demand exceeds buffer" do
      buffer = :queue.in(%{sender: "ONLY"}, :queue.new())

      state = %{
        demand: 0,
        buffer: buffer,
        buffer_size: 1,
        max_buffer_size: 1000,
        high_water_mark: 800,
        low_water_mark: 300,
        backpressure_active: false,
        is_monitor_ref: nil
      }

      {:noreply, events, new_state} = PacketProducer.handle_demand(5, state)

      assert events == [%{sender: "ONLY"}]
      assert new_state.buffer_size == 0
      assert new_state.demand == 4
    end

    test "stores demand when buffer is empty" do
      state = %{
        demand: 0,
        buffer: :queue.new(),
        buffer_size: 0,
        max_buffer_size: 1000,
        high_water_mark: 800,
        low_water_mark: 300,
        backpressure_active: false,
        is_monitor_ref: nil
      }

      {:noreply, [], new_state} = PacketProducer.handle_demand(10, state)

      assert new_state.demand == 10
    end

    test "accumulates demand" do
      state = %{
        demand: 5,
        buffer: :queue.new(),
        buffer_size: 0,
        max_buffer_size: 1000,
        high_water_mark: 800,
        low_water_mark: 300,
        backpressure_active: false,
        is_monitor_ref: nil
      }

      {:noreply, [], new_state} = PacketProducer.handle_demand(3, state)

      assert new_state.demand == 8
    end
  end
end
