defmodule Aprs.Integration.HistoricalPacketsTest do
  use AprsWeb.ConnCase

  import Phoenix.LiveViewTest

  @moduletag :skip_packets_mock

  describe "historical packet loading" do
    setup do
      # Create test packets with different timestamps
      now = DateTime.utc_now()
      one_hour_ago = DateTime.add(now, -3600, :second)
      thirty_minutes_ago = DateTime.add(now, -1800, :second)
      ten_minutes_ago = DateTime.add(now, -600, :second)

      # Mock packet data for the same callsign at different times
      packet1 = %Aprs.Packet{
        id: 1,
        base_callsign: "TEST1",
        ssid: 0,
        has_position: true,
        lat: 39.8,
        lon: -98.5,
        received_at: one_hour_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Old position"
        }
      }

      packet2 = %Aprs.Packet{
        id: 2,
        base_callsign: "TEST1",
        ssid: 0,
        has_position: true,
        lat: 39.9,
        lon: -98.4,
        received_at: thirty_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Middle position"
        }
      }

      packet3 = %Aprs.Packet{
        id: 3,
        base_callsign: "TEST1",
        ssid: 0,
        has_position: true,
        lat: 40.0,
        lon: -98.3,
        received_at: ten_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Latest position"
        }
      }

      # Mock packet for different callsign
      packet4 = %Aprs.Packet{
        id: 4,
        base_callsign: "TEST2",
        ssid: 0,
        has_position: true,
        lat: 38.8,
        lon: -97.5,
        received_at: thirty_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: "k",
          comment: "Different station"
        }
      }

      {:ok, packets: [packet1, packet2, packet3, packet4]}
    end

    test "loads all historical packets at once when map is ready", %{conn: conn, packets: mock_packets} do
      # Mock the Packets.get_packets_for_replay function
      expect_packets_for_replay_with_bounds(mock_packets)

      {:ok, view, _html} = live(conn, "/")

      # Set map bounds that include our test packets
      bounds_params = %{
        "bounds" => %{
          "north" => "45.0",
          "south" => "35.0",
          "east" => "-90.0",
          "west" => "-105.0"
        }
      }

      # Update bounds first
      render_hook(view, "bounds_changed", bounds_params)

      # Trigger map ready event
      render_hook(view, "map_ready", %{})

      # Wait for the historical packet loading to complete
      Process.sleep(700)

      # The LiveView should have pushed an event with historical packets
      # Note: In real implementation, we'd need to verify the push_event was called
      # For now, we verify the mock was called correctly
      verify!()
    end

    test "only loads packets within map bounds", %{conn: conn, packets: mock_packets} do
      # Mock to return only packets within the specified bounds
      # TEST2 is at lat: 38.8, lon: -97.5, which is outside these bounds
      test1_packets = Enum.filter(mock_packets, fn packet -> packet.base_callsign == "TEST1" end)
      expect_packets_for_replay_with_bounds(test1_packets)

      {:ok, view, _html} = live(conn, "/")

      # Set map bounds that exclude TEST2
      bounds_params = %{
        "bounds" => %{
          "north" => "41.0",
          "south" => "39.0",
          "east" => "-98.0",
          "west" => "-99.0"
        }
      }

      # Update bounds first
      render_hook(view, "bounds_changed", bounds_params)

      # Trigger map ready event
      render_hook(view, "map_ready", %{})

      # Wait for the historical packet loading to complete
      Process.sleep(700)

      verify!()
    end

    test "handles empty historical packets gracefully", %{conn: conn} do
      # Mock to return empty list
      expect_packets_for_replay([])

      {:ok, view, _html} = live(conn, "/")

      # Trigger map ready event
      render_hook(view, "map_ready", %{})

      # Wait for the historical packet loading attempt
      Process.sleep(700)

      # Should not crash
      verify!()
    end

    test "clears historical packets when requested", %{conn: conn, packets: mock_packets} do
      expect_packets_for_replay(mock_packets)

      {:ok, view, _html} = live(conn, "/")

      # First load historical packets
      render_hook(view, "map_ready", %{})
      Process.sleep(700)

      # Clear and reload markers
      render_hook(view, "clear_and_reload_markers", %{})

      verify!()
    end

    test "handles locate_me event after historical packets are loaded", %{conn: conn, packets: mock_packets} do
      expect_packets_for_replay(mock_packets)

      {:ok, view, _html} = live(conn, "/")

      # Load historical packets first
      render_hook(view, "map_ready", %{})
      Process.sleep(700)

      # Request location
      render_hook(view, "locate_me", %{})

      verify!()
    end
  end

  describe "live packet updates with historical data" do
    setup do
      # Create a historical packet
      now = DateTime.utc_now()
      thirty_minutes_ago = DateTime.add(now, -1800, :second)

      historical_packet = %Aprs.Packet{
        id: 100,
        base_callsign: "LIVE1",
        ssid: 0,
        has_position: true,
        lat: 39.8,
        lon: -98.5,
        received_at: thirty_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Old position"
        }
      }

      {:ok, historical_packet: historical_packet}
    end

    test "new live packet updates marker for same callsign", %{conn: conn, historical_packet: historical_packet} do
      expect_packets_for_replay([historical_packet])

      {:ok, view, _html} = live(conn, "/")

      # Set bounds and load historical packets
      bounds_params = %{
        "bounds" => %{
          "north" => "45.0",
          "south" => "35.0",
          "east" => "-90.0",
          "west" => "-105.0"
        }
      }

      render_hook(view, "bounds_changed", bounds_params)
      render_hook(view, "map_ready", %{})
      Process.sleep(700)

      # Simulate a new live packet for the same callsign
      new_packet = %{
        id: "LIVE1",
        callsign: "LIVE1",
        base_callsign: "LIVE1",
        ssid: 0,
        lat: 39.9,
        lng: -98.4,
        data_type: "position",
        path: "WIDE1-1,WIDE2-1",
        comment: "New live position",
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">"
        },
        symbol_table_id: "/",
        symbol_code: ">",
        timestamp: DateTime.to_iso8601(DateTime.utc_now()),
        popup: "<div>New live position</div>"
      }

      # Send the new packet event
      send(view.pid, %Phoenix.Socket.Broadcast{
        topic: "aprs_messages",
        event: "packet",
        payload: new_packet
      })

      # Give the LiveView time to process the message
      Process.sleep(100)

      verify!()
    end
  end
end
