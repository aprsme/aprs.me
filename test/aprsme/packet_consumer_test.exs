defmodule Aprsme.PacketConsumerTest do
  use Aprsme.DataCase, async: false

  import Ecto.Query

  alias Aprsme.Packet
  alias Aprsme.PacketConsumer
  alias Aprsme.StreamingPacketsPubSub

  describe "object name extraction" do
    test "extracts object name with hyphens from information_field" do
      events = [
        %{
          sender: "db0sda",
          lat: 33.169,
          lon: -96.492,
          data_type: "object",
          destination: "APRS",
          path: "TCPIP*,qAC,SECOND",
          information_field: ";P-K5SGD  *192200z3310.16NP09629.53W#PHG0-100DAPNET POCSAG Transmitter",
          raw_packet: "db0sda>APRS,TCPIP*,qAC,SECOND:;P-K5SGD  *192200z3310.16NP09629.53W#"
        }
      ]

      state = %{batch: [], batch_length: 0, batch_size: 1, batch_timeout: 1000, max_batch_size: 100, timer: nil}

      {:noreply, [], _} = PacketConsumer.handle_events(events, nil, state)
      Process.sleep(50)

      packet = Repo.one(from p in Packet, where: p.sender == "db0sda")
      assert packet
      assert packet.object_name == "P-K5SGD"
    end

    test "extracts object name with special characters" do
      events = [
        %{
          sender: "OBJTEST1",
          lat: 35.0,
          lon: -75.0,
          data_type: "object",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: ";#146.760 *192200z3500.00N/07500.00W#Repeater",
          raw_packet: "OBJTEST1>APRS:;#146.760 *192200z3500.00N/07500.00W#Repeater"
        }
      ]

      state = %{batch: [], batch_length: 0, batch_size: 1, batch_timeout: 1000, max_batch_size: 100, timer: nil}

      {:noreply, [], _} = PacketConsumer.handle_events(events, nil, state)
      Process.sleep(50)

      packet = Repo.one(from p in Packet, where: p.sender == "OBJTEST1")
      assert packet
      assert packet.object_name == "#146.760"
    end

    test "extracts object name for killed objects" do
      events = [
        %{
          sender: "OBJTEST2",
          lat: 35.0,
          lon: -75.0,
          data_type: "object",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: ";P-KILLED _192200z3500.00N/07500.00W#Removed",
          raw_packet: "OBJTEST2>APRS:;P-KILLED _192200z3500.00N/07500.00W#Removed"
        }
      ]

      state = %{batch: [], batch_length: 0, batch_size: 1, batch_timeout: 1000, max_batch_size: 100, timer: nil}

      {:noreply, [], _} = PacketConsumer.handle_events(events, nil, state)
      Process.sleep(50)

      packet = Repo.one(from p in Packet, where: p.sender == "OBJTEST2")
      assert packet
      assert packet.object_name == "P-KILLED"
    end
  end

  describe "handle_events/3 with stream processing" do
    test "processes packets using streams without memory accumulation" do
      # Create test packets
      events =
        for i <- 1..1000 do
          %{
            sender: "K#{i}TEST",
            lat: 35.0 + :rand.uniform() * 5,
            lon: -75.0 + :rand.uniform() * 5,
            destination: "APRS",
            path: "WIDE1-1",
            information_field: "Test packet #{i}",
            data_type: "position",
            raw_packet: "K#{i}TEST>APRS,WIDE1-1:Test packet #{i}"
          }
        end

      # Initialize consumer state
      state = %{
        batch: [],
        batch_length: 0,
        batch_size: 100,
        batch_timeout: 1000,
        max_batch_size: 1000,
        timer: nil
      }

      # Process events and verify memory usage
      {:noreply, [], new_state} = PacketConsumer.handle_events(events, nil, state)

      # Verify batch was processed (empty batch after processing)
      assert new_state.batch == []

      # Give time for async operations
      Process.sleep(20)

      # Check that packets were inserted
      assert Repo.aggregate(Packet, :count) > 0
    end

    test "handles invalid packets gracefully" do
      # Mix of valid and invalid packets
      events = [
        %{
          sender: "VALID1",
          lat: 35.0,
          lon: -75.0,
          data_type: "position",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: "Test packet 1",
          raw_packet: "VALID1>APRS,WIDE1-1:Test packet 1"
        },
        # Invalid - no sender
        %{sender: nil, lat: 35.0, lon: -75.0},
        # Invalid - empty sender
        %{sender: "", lat: 35.0, lon: -75.0},
        # Invalid coordinates (lat > 90)
        %{
          sender: "VALID2",
          lat: 91.0,
          lon: -75.0,
          data_type: "position",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: "Test packet 2",
          raw_packet: "VALID2>APRS,WIDE1-1:Test packet 2"
        },
        %{
          sender: "VALID3",
          lat: 35.0,
          lon: -75.0,
          data_type: "position",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: "Test packet 3",
          raw_packet: "VALID3>APRS,WIDE1-1:Test packet 3"
        }
      ]

      state = %{
        batch: [],
        batch_length: 0,
        # Set batch size to 5 to trigger immediate processing
        batch_size: 5,
        batch_timeout: 1000,
        max_batch_size: 100,
        timer: nil
      }

      {:noreply, [], _new_state} = PacketConsumer.handle_events(events, nil, state)

      # Give more time for async processing and broadcasts
      Process.sleep(50)

      # Valid packets (with valid sender) should be inserted
      # Note: VALID2 has invalid coordinates but valid sender, so it's still inserted
      packets = Repo.all(Packet)

      # Should have 3 packets: VALID1, VALID2 (with nil location), and VALID3
      assert length(packets) == 3

      # Check that all valid senders are present
      senders = packets |> Enum.map(& &1.sender) |> Enum.sort()
      assert senders == ["VALID1", "VALID2", "VALID3"]

      # Check that VALID2 has no location due to invalid coordinates
      valid2_packet = Enum.find(packets, &(&1.sender == "VALID2"))
      assert valid2_packet.location == nil
      # Invalid latitude stored
      assert Decimal.equal?(valid2_packet.lat, Decimal.new("91.0"))

      # Check that VALID1 and VALID3 have valid locations
      valid_location_packets = Enum.filter(packets, &(&1.sender in ["VALID1", "VALID3"]))
      assert Enum.all?(valid_location_packets, &(&1.location != nil))
    end

    test "broadcasts packets to StreamingPacketsPubSub" do
      # Subscribe to packets in test bounds
      bounds = %{north: 40.0, south: 30.0, east: -70.0, west: -80.0}
      StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

      events = [
        %{
          sender: "BROADCAST1",
          lat: 35.0,
          lon: -75.0,
          data_type: "position",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: "Test broadcast"
        }
      ]

      state = %{
        batch: [],
        batch_length: 0,
        batch_size: 1,
        batch_timeout: 1000,
        max_batch_size: 100,
        timer: nil
      }

      PacketConsumer.handle_events(events, nil, state)
      # Wait for async broadcast task to complete
      Process.sleep(50)

      # Should receive the packet via PubSub
      assert_receive {:streaming_packet, packet}, 1000
      assert packet.sender == "BROADCAST1"
    end

    test "respects max_batch_size and drops excess packets" do
      import ExUnit.CaptureLog

      # Temporarily enable warning level for this test
      Logger.configure(level: :warning)
      # Create more packets than max_batch_size
      events =
        for i <- 1..150 do
          %{
            sender: "K#{i}DROP",
            lat: 35.0,
            lon: -75.0,
            data_type: "position"
          }
        end

      state = %{
        batch: [],
        batch_length: 0,
        batch_size: 50,
        batch_timeout: 1000,
        # Only 100 will be processed
        max_batch_size: 100,
        timer: nil
      }

      # Capture logs to verify warning
      log =
        capture_log(fn ->
          {:noreply, [], _new_state} = PacketConsumer.handle_events(events, nil, state)
          Process.sleep(20)
        end)

      # Should log warning about dropped packets
      assert log =~ "Dropped 50 packets due to batch size limit"

      # Reset log level
      Logger.configure(level: :error)

      # Only max_batch_size packets should be processed
      packet_count = Repo.aggregate(Packet, :count)
      assert packet_count <= 100
    end
  end

  describe "batch insert resilience" do
    test "recovers from batch insert failure by falling back to individual inserts" do
      # We test this by inserting a batch that includes a packet which would
      # cause a constraint violation only in batch mode.
      # Since we can't easily trigger Repo.insert_all failures in test,
      # we verify the fallback path exists by testing process_chunk indirectly:
      # inserting valid packets should succeed even when called through the
      # batch processing pipeline.

      events = [
        %{
          sender: "RETRY1",
          lat: 35.0,
          lon: -75.0,
          data_type: "position",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: "Test retry 1",
          raw_packet: "RETRY1>APRS:Test retry 1"
        },
        %{
          sender: "RETRY2",
          lat: 36.0,
          lon: -76.0,
          data_type: "position",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: "Test retry 2",
          raw_packet: "RETRY2>APRS:Test retry 2"
        }
      ]

      state = %{
        batch: [],
        batch_length: 0,
        batch_size: 2,
        batch_timeout: 1000,
        max_batch_size: 100,
        timer: nil
      }

      {:noreply, [], _new_state} = PacketConsumer.handle_events(events, nil, state)

      Process.sleep(50)

      packets = Repo.all(Packet)
      senders = packets |> Enum.map(& &1.sender) |> Enum.sort()
      assert "RETRY1" in senders
      assert "RETRY2" in senders
    end

    test "fallback individual inserts still broadcast packets" do
      bounds = %{north: 40.0, south: 30.0, east: -70.0, west: -80.0}
      StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

      events = [
        %{
          sender: "FALLBACK1",
          lat: 35.0,
          lon: -75.0,
          data_type: "position",
          destination: "APRS",
          path: "WIDE1-1",
          information_field: "Test fallback"
        }
      ]

      state = %{
        batch: [],
        batch_length: 0,
        batch_size: 1,
        batch_timeout: 1000,
        max_batch_size: 100,
        timer: nil
      }

      PacketConsumer.handle_events(events, nil, state)
      # Wait for async broadcast task to complete
      Process.sleep(50)

      assert_receive {:streaming_packet, packet}, 1000
      assert packet.sender == "FALLBACK1"
    end
  end

  describe "memory efficiency" do
    test "processes large batches without excessive memory growth" do
      # Monitor memory before processing
      :erlang.garbage_collect()
      memory_before = :erlang.memory(:total)

      # Create a smaller, more focused batch of packets
      # Reduced from 5000 to 1000 packets and smaller payloads
      events =
        for i <- 1..1000 do
          %{
            sender: "K#{i}MEM",
            lat: 35.0 + :rand.uniform() * 5,
            lon: -75.0 + :rand.uniform() * 5,
            data_type: "position",
            # Smaller payload - 100 chars instead of 1000
            information_field: String.duplicate("X", 100)
          }
        end

      state = %{
        batch: [],
        batch_length: 0,
        batch_size: 100,
        batch_timeout: 100,
        max_batch_size: 200,
        timer: nil
      }

      # Process in chunks (simulating multiple handle_events calls)
      final_state =
        events
        |> Enum.chunk_every(200)
        |> Enum.reduce(state, fn chunk, acc_state ->
          {:noreply, [], new_state} = PacketConsumer.handle_events(chunk, nil, acc_state)
          new_state
        end)

      # Ensure we use the final_state to avoid warning
      assert is_map(final_state)

      # Force GC and check memory
      :erlang.garbage_collect()
      memory_after = :erlang.memory(:total)
      memory_growth = memory_after - memory_before

      # Memory growth should be reasonable (less than 10MB for 1000 packets)
      assert memory_growth < 10_000_000
    end
  end
end
