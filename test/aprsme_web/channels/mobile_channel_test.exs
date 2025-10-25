defmodule AprsmeWeb.MobileChannelTest do
  use AprsmeWeb.ChannelCase

  alias Aprsme.Packet
  alias Aprsme.Repo
  alias AprsmeWeb.MobileUserSocket

  setup do
    # Create test packets with truncated timestamps (no microseconds)
    now = DateTime.truncate(DateTime.utc_now(), :second)

    {:ok, packet1} =
      Repo.insert(%Packet{
        sender: "W5ISP-9",
        base_callsign: "W5ISP",
        lat: Decimal.new("33.1225"),
        lon: Decimal.new("-96.124"),
        received_at: now,
        symbol_table_id: "/",
        symbol_code: ">",
        comment: "Test packet 1"
      })

    {:ok, packet2} =
      Repo.insert(%Packet{
        sender: "W5ISP-1",
        base_callsign: "W5ISP",
        lat: Decimal.new("33.15"),
        lon: Decimal.new("-96.10"),
        received_at: now,
        symbol_table_id: "/",
        symbol_code: ">",
        comment: "Test packet 2"
      })

    {:ok, packet3} =
      Repo.insert(%Packet{
        sender: "K5GVL-10",
        base_callsign: "K5GVL",
        lat: Decimal.new("33.20"),
        lon: Decimal.new("-96.15"),
        received_at: now,
        symbol_table_id: "/",
        symbol_code: "#",
        comment: "Test packet 3"
      })

    {:ok, socket} = connect(MobileUserSocket, %{})
    {:ok, _, socket} = subscribe_and_join(socket, "mobile:packets", %{})

    %{socket: socket, packet1: packet1, packet2: packet2, packet3: packet3}
  end

  describe "join" do
    test "successful join returns welcome message", %{socket: socket} do
      assert socket.assigns == %{}
    end
  end

  describe "subscribe_bounds" do
    test "subscribes to bounds successfully", %{socket: socket} do
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :ok, %{bounds: bounds, message: message}
      assert bounds.north == 33.2
      assert bounds.south == 33.0
      assert bounds.east == -96.0
      assert bounds.west == -96.2
      assert message == "Subscribed to packet stream"
    end

    test "rejects invalid bounds - north less than south", %{socket: socket} do
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.0,
          "south" => 33.2,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :error, %{message: "North must be greater than south"}
    end

    test "rejects invalid bounds - latitude out of range", %{socket: socket} do
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 91.0,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :error, %{message: "Latitude must be between -90 and 90"}
    end

    test "rejects invalid bounds - longitude out of range", %{socket: socket} do
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => 181.0,
          "west" => -96.2
        })

      assert_reply ref, :error, %{message: "Longitude must be between -180 and 180"}
    end

    test "accepts integer coordinates and converts to float", %{socket: socket} do
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 34,
          "south" => 33,
          "east" => -96,
          "west" => -97
        })

      assert_reply ref, :ok, %{bounds: bounds}
      assert is_float(bounds.north)
      assert is_float(bounds.south)
      assert is_float(bounds.east)
      assert is_float(bounds.west)
    end
  end

  describe "update_bounds" do
    test "updates bounds successfully when subscribed", %{socket: socket} do
      # First subscribe
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :ok, _

      # Then update
      ref =
        push(socket, "update_bounds", %{
          "north" => 33.3,
          "south" => 32.9,
          "east" => -95.9,
          "west" => -96.3
        })

      assert_reply ref, :ok, %{bounds: bounds, message: message}
      assert bounds.north == 33.3
      assert bounds.south == 32.9
      assert message == "Bounds updated"
    end

    test "rejects update when not subscribed", %{socket: socket} do
      ref =
        push(socket, "update_bounds", %{
          "north" => 33.3,
          "south" => 32.9,
          "east" => -95.9,
          "west" => -96.3
        })

      assert_reply ref, :error, %{message: "Not subscribed. Call subscribe_bounds first."}
    end

    test "rejects invalid bounds on update", %{socket: socket} do
      # First subscribe
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :ok, _

      # Try to update with invalid bounds
      ref =
        push(socket, "update_bounds", %{
          "north" => 32.9,
          "south" => 33.3,
          "east" => -95.9,
          "west" => -96.3
        })

      assert_reply ref, :error, %{message: "North must be greater than south"}
    end
  end

  describe "unsubscribe" do
    test "unsubscribes successfully when subscribed", %{socket: socket} do
      # First subscribe
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :ok, _

      # Then unsubscribe
      ref = push(socket, "unsubscribe", %{})
      assert_reply ref, :ok, %{message: "Unsubscribed from packet stream"}
    end

    test "returns message when not subscribed", %{socket: socket} do
      ref = push(socket, "unsubscribe", %{})
      assert_reply ref, :ok, %{message: "Not subscribed"}
    end
  end

  describe "search_callsign" do
    test "searches for callsign with default limit", %{socket: socket} do
      ref = push(socket, "search_callsign", %{"query" => "W5ISP"})

      assert_reply ref, :ok, %{results: results, count: count}
      assert is_list(results)
      assert count == length(results)
      # At least W5ISP-9 and W5ISP-1
      assert count >= 2
    end

    test "searches for specific SSID", %{socket: socket} do
      ref = push(socket, "search_callsign", %{"query" => "W5ISP-9"})

      assert_reply ref, :ok, %{results: results, count: count}
      assert count >= 1
      # Should find W5ISP-9
      assert Enum.any?(results, fn r -> r.callsign == "W5ISP-9" end)
    end

    test "searches with wildcard", %{socket: socket} do
      ref = push(socket, "search_callsign", %{"query" => "W5ISP*"})

      assert_reply ref, :ok, %{results: results, count: count}
      assert count >= 2
      # All results should start with W5ISP
      assert Enum.all?(results, fn r -> String.starts_with?(r.callsign, "W5ISP") end)
    end

    test "respects limit parameter", %{socket: socket} do
      ref = push(socket, "search_callsign", %{"query" => "W5ISP", "limit" => 1})

      assert_reply ref, :ok, %{results: _results, count: count}
      assert count <= 1
    end

    test "limits to max of 500", %{socket: socket} do
      ref = push(socket, "search_callsign", %{"query" => "W5ISP", "limit" => 1000})

      assert_reply ref, :ok, %{results: results}
      # Should not crash and should return results
      assert is_list(results)
    end

    test "normalizes callsign to uppercase", %{socket: socket} do
      ref = push(socket, "search_callsign", %{"query" => "w5isp"})

      assert_reply ref, :ok, %{results: _results, count: count}
      assert count >= 2
    end
  end

  describe "subscribe_callsign" do
    test "subscribes to callsign successfully", %{socket: socket} do
      ref = push(socket, "subscribe_callsign", %{"callsign" => "W5ISP-9"})

      assert_reply ref, :ok, %{callsign: callsign, message: message}
      assert callsign == "W5ISP-9"
      assert message == "Subscribed to callsign updates"
    end

    test "subscribes with custom hours_back", %{socket: socket} do
      ref = push(socket, "subscribe_callsign", %{"callsign" => "W5ISP-9", "hours_back" => 48})

      assert_reply ref, :ok, %{callsign: "W5ISP-9"}
    end

    test "limits hours_back to maximum of 168", %{socket: socket} do
      ref = push(socket, "subscribe_callsign", %{"callsign" => "W5ISP-9", "hours_back" => 200})

      assert_reply ref, :ok, %{callsign: "W5ISP-9"}
      # Should not crash even with hours_back > 168
    end

    test "normalizes callsign to uppercase", %{socket: socket} do
      ref = push(socket, "subscribe_callsign", %{"callsign" => "w5isp-9"})

      assert_reply ref, :ok, %{callsign: callsign}
      assert callsign == "W5ISP-9"
    end

    test "subscribes with wildcard pattern", %{socket: socket} do
      ref = push(socket, "subscribe_callsign", %{"callsign" => "W5ISP*"})

      assert_reply ref, :ok, %{callsign: "W5ISP*"}
    end
  end

  describe "unsubscribe_callsign" do
    test "unsubscribes from callsign successfully", %{socket: socket} do
      # First subscribe
      ref = push(socket, "subscribe_callsign", %{"callsign" => "W5ISP-9"})
      assert_reply ref, :ok, _

      # Then unsubscribe
      ref = push(socket, "unsubscribe_callsign", %{})
      assert_reply ref, :ok, %{message: "Unsubscribed from callsign updates"}
    end

    test "returns message when not tracking", %{socket: socket} do
      ref = push(socket, "unsubscribe_callsign", %{})
      assert_reply ref, :ok, %{message: "Not tracking any callsign"}
    end
  end

  describe "streaming packets" do
    test "receives streaming packet when subscribed to bounds", %{socket: socket} do
      # Subscribe to bounds
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :ok, _

      # Simulate streaming packet
      packet = %{
        sender: "TEST-1",
        lat: Decimal.new("33.1"),
        lon: Decimal.new("-96.1"),
        received_at: DateTime.truncate(DateTime.utc_now(), :second)
      }

      send(socket.channel_pid, {:streaming_packet, packet})

      # Should receive the packet
      assert_push "packet", pushed_packet
      assert pushed_packet.callsign == "TEST-1"
    end

    test "filters streaming packet by tracked callsign", %{socket: socket} do
      # Subscribe to callsign
      ref = push(socket, "subscribe_callsign", %{"callsign" => "W5ISP-9"})
      assert_reply ref, :ok, _

      # Clear historical packets from the mailbox
      Process.sleep(100)
      # Consume all pending messages in a loop
      fn ->
        receive do
          _ -> true
        after
          0 -> false
        end
      end
      |> Stream.repeatedly()
      |> Enum.take_while(& &1)

      # Simulate matching packet
      packet1 = %{
        sender: "W5ISP-9",
        lat: Decimal.new("33.1"),
        lon: Decimal.new("-96.1"),
        received_at: DateTime.truncate(DateTime.utc_now(), :second)
      }

      send(socket.channel_pid, {:streaming_packet, packet1})

      # Should receive the packet
      assert_push "packet", pushed_packet
      assert pushed_packet.callsign == "W5ISP-9"

      # Simulate non-matching packet
      packet2 = %{
        sender: "K5GVL-10",
        lat: Decimal.new("33.1"),
        lon: Decimal.new("-96.1"),
        received_at: DateTime.truncate(DateTime.utc_now(), :second)
      }

      send(socket.channel_pid, {:streaming_packet, packet2})

      # Should NOT receive the packet
      refute_push "packet", _
    end

    test "wildcard callsign matches multiple SSIDs", %{socket: socket} do
      # Subscribe to wildcard
      ref = push(socket, "subscribe_callsign", %{"callsign" => "W5ISP*"})
      assert_reply ref, :ok, _

      # Simulate packets with different SSIDs
      for ssid <- ["-1", "-9", "-15"] do
        packet = %{
          sender: "W5ISP#{ssid}",
          lat: Decimal.new("33.1"),
          lon: Decimal.new("-96.1"),
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        }

        send(socket.channel_pid, {:streaming_packet, packet})

        # Should receive all W5ISP-* packets
        assert_push "packet", pushed_packet
        assert String.starts_with?(pushed_packet.callsign, "W5ISP")
      end
    end
  end

  describe "packet data format" do
    test "builds mobile packet with all fields", %{socket: socket} do
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :ok, _

      packet = %{
        id: "test-id",
        sender: "W5ISP-9",
        lat: Decimal.new("33.1225"),
        lon: Decimal.new("-96.124"),
        received_at: DateTime.truncate(DateTime.utc_now(), :second),
        symbol_table_id: "/",
        symbol_code: ">",
        comment: "Test comment",
        altitude: 150,
        speed: 45,
        course: 180,
        path: "WIDE1-1,WIDE2-1"
      }

      send(socket.channel_pid, {:streaming_packet, packet})

      assert_push "packet", pushed_packet
      assert pushed_packet.id == "test-id"
      assert pushed_packet.callsign == "W5ISP-9"
      assert pushed_packet.lat == 33.1225
      assert pushed_packet.lng == -96.124
      assert pushed_packet.symbol_table_id == "/"
      assert pushed_packet.symbol_code == ">"
      assert pushed_packet.comment == "Test comment"
      assert pushed_packet.altitude == 150
      assert pushed_packet.speed == 45
      assert pushed_packet.course == 180
      assert pushed_packet.path == "WIDE1-1,WIDE2-1"
      assert is_binary(pushed_packet.timestamp)
    end

    test "omits nil fields from packet data", %{socket: socket} do
      ref =
        push(socket, "subscribe_bounds", %{
          "north" => 33.2,
          "south" => 33.0,
          "east" => -96.0,
          "west" => -96.2
        })

      assert_reply ref, :ok, _

      packet = %{
        id: "test-id",
        sender: "W5ISP-9",
        lat: Decimal.new("33.1225"),
        lon: Decimal.new("-96.124"),
        received_at: DateTime.truncate(DateTime.utc_now(), :second),
        symbol_table_id: "/",
        symbol_code: ">",
        comment: nil,
        altitude: nil,
        speed: nil,
        course: nil,
        path: nil
      }

      send(socket.channel_pid, {:streaming_packet, packet})

      assert_push "packet", pushed_packet
      refute Map.has_key?(pushed_packet, :comment)
      refute Map.has_key?(pushed_packet, :altitude)
      refute Map.has_key?(pushed_packet, :speed)
      refute Map.has_key?(pushed_packet, :course)
      refute Map.has_key?(pushed_packet, :path)
    end
  end
end
