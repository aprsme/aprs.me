defmodule Aprsme.StreamingPacketsPubSubTest do
  use ExUnit.Case, async: true

  alias Aprsme.StreamingPacketsPubSub

  describe "subscribe_to_bounds/2" do
    test "subscribes to packets within geographic bounds" do
      bounds = %{
        north: 40.0,
        south: 30.0,
        east: -70.0,
        west: -80.0
      }

      assert :ok = StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)
    end

    test "receives packets within subscribed bounds" do
      bounds = %{
        north: 40.0,
        south: 30.0,
        east: -70.0,
        west: -80.0
      }

      StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

      # Packet within bounds
      packet_in_bounds = %{
        sender: "K1ABC",
        latitude: 35.0,
        longitude: -75.0,
        received_at: DateTime.utc_now()
      }

      StreamingPacketsPubSub.broadcast_packet(packet_in_bounds)

      assert_receive {:streaming_packet, ^packet_in_bounds}, 1000
    end

    test "does not receive packets outside subscribed bounds" do
      bounds = %{
        north: 40.0,
        south: 30.0,
        east: -70.0,
        west: -80.0
      }

      StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

      # Packet outside bounds
      packet_outside_bounds = %{
        sender: "K2XYZ",
        # North of bounds
        latitude: 45.0,
        longitude: -75.0,
        received_at: DateTime.utc_now()
      }

      StreamingPacketsPubSub.broadcast_packet(packet_outside_bounds)

      refute_receive {:streaming_packet, _}, 500
    end

    test "handles multiple subscribers with different bounds" do
      subscriber1 =
        spawn(fn ->
          receive do
            _ -> :ok
          end
        end)

      subscriber2 =
        spawn(fn ->
          receive do
            _ -> :ok
          end
        end)

      bounds1 = %{north: 40.0, south: 30.0, east: -70.0, west: -80.0}
      bounds2 = %{north: 50.0, south: 40.0, east: -60.0, west: -70.0}

      StreamingPacketsPubSub.subscribe_to_bounds(subscriber1, bounds1)
      StreamingPacketsPubSub.subscribe_to_bounds(subscriber2, bounds2)

      # Packet in bounds1 only
      packet = %{
        sender: "K3TEST",
        latitude: 35.0,
        longitude: -75.0,
        received_at: DateTime.utc_now()
      }

      # Monitor to ensure processes are alive
      ref1 = Process.monitor(subscriber1)
      _ref2 = Process.monitor(subscriber2)

      StreamingPacketsPubSub.broadcast_packet(packet)

      # Verify subscriber1 gets the packet
      send(subscriber1, {:check, self()})
      assert_receive {:DOWN, ^ref1, :process, ^subscriber1, :normal}, 100

      # Verify subscriber2 is still alive (didn't receive packet)
      Process.alive?(subscriber2)
    end
  end

  describe "unsubscribe/1" do
    test "stops receiving packets after unsubscribe" do
      bounds = %{north: 40.0, south: 30.0, east: -70.0, west: -80.0}

      StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)
      StreamingPacketsPubSub.unsubscribe(self())

      packet = %{
        sender: "K4TEST",
        latitude: 35.0,
        longitude: -75.0,
        received_at: DateTime.utc_now()
      }

      StreamingPacketsPubSub.broadcast_packet(packet)

      refute_receive {:streaming_packet, _}, 500
    end
  end

  describe "list_subscribers/0" do
    test "returns all active subscribers with their bounds" do
      bounds = %{north: 40.0, south: 30.0, east: -70.0, west: -80.0}

      StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

      subscribers = StreamingPacketsPubSub.list_subscribers()

      assert Enum.any?(subscribers, fn {pid, sub_bounds} ->
               pid == self() && sub_bounds == bounds
             end)
    end
  end

  describe "performance" do
    test "handles high volume of packets efficiently" do
      bounds = %{north: 90.0, south: -90.0, east: 180.0, west: -180.0}
      StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

      packets =
        for i <- 1..1000 do
          %{
            sender: "K#{i}TEST",
            latitude: :rand.uniform() * 180 - 90,
            longitude: :rand.uniform() * 360 - 180,
            received_at: DateTime.utc_now()
          }
        end

      start_time = System.monotonic_time(:millisecond)

      Enum.each(packets, &StreamingPacketsPubSub.broadcast_packet/1)

      end_time = System.monotonic_time(:millisecond)
      elapsed = end_time - start_time

      # Should process 1000 packets in under 100ms
      assert elapsed < 100

      # Should receive all packets
      received_count =
        Enum.reduce(1..1000, 0, fn _, acc ->
          receive do
            {:streaming_packet, _} -> acc + 1
          after
            10 -> acc
          end
        end)

      assert received_count == 1000
    end
  end
end
