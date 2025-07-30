defmodule AprsmeWeb.MapLive.TrackedCallsignOldPacketTest do
  use AprsmeWeb.ConnCase

  import Aprsme.PacketsFixtures
  import Phoenix.LiveViewTest

  describe "tracked callsign shows last packet regardless of age" do
    test "displays old packet for tracked callsign", %{conn: conn} do
      # Create a packet that's 30 days old
      thirty_days_ago = DateTime.add(DateTime.utc_now(), -30 * 24 * 60 * 60, :second)

      old_packet =
        packet_fixture(%{
          sender: "TEST-OLD",
          lat: Decimal.new("35.1234"),
          lon: Decimal.new("-97.5678"),
          raw_packet: "TEST-OLD>APRS:!3507.40N/09734.07W>Test old packet",
          received_at: thirty_days_ago,
          has_position: true
        })

      # Navigate to the tracked callsign URL
      {:ok, view, _html} = live(conn, "/TEST-OLD")

      # Wait for map to be ready
      send(view.pid, %{
        event: "map_ready",
        data: %{
          center: %{lat: 35.0, lng: -97.0},
          zoom: 10,
          bounds: %{
            north: 36.0,
            south: 34.0,
            east: -96.0,
            west: -98.0
          }
        }
      })

      # Allow time for the view to process
      :timer.sleep(100)

      # Check that the tracked callsign is set
      assert render(view) =~ "TEST-OLD"

      # The packet should be in the view's assigns
      # Since filter_packets_by_time_and_bounds_with_tracked always includes
      # the tracked callsign's latest packet, it should be present
      state = :sys.get_state(view.pid)
      socket = elem(state, 1).socket

      # Verify the tracked callsign is set
      assert socket.assigns.tracked_callsign == "TEST-OLD"

      # Verify the tracked latest packet is the old packet
      assert socket.assigns.tracked_latest_packet.id == old_packet.id
      assert socket.assigns.tracked_latest_packet.sender == "TEST-OLD"

      # Cleanup handled by test transaction
    end

    test "includes old packet even with 1-hour trail duration", %{conn: conn} do
      # Create a packet that's 48 hours old
      two_days_ago = DateTime.add(DateTime.utc_now(), -48 * 60 * 60, :second)

      old_packet =
        packet_fixture(%{
          sender: "OLD-STATION",
          lat: Decimal.new("40.7128"),
          lon: Decimal.new("-74.0060"),
          raw_packet: "OLD-STATION>APRS:!4042.77N/07400.36W>Old position",
          received_at: two_days_ago,
          has_position: true
        })

      # Navigate to the tracked callsign URL
      {:ok, view, _html} = live(conn, "/OLD-STATION")

      # Set trail duration to 1 hour (shortest option)
      send(view.pid, {:update_trail_duration, 1})

      # Wait for map to be ready
      send(view.pid, %{
        event: "map_ready",
        data: %{
          center: %{lat: 40.7, lng: -74.0},
          zoom: 10,
          bounds: %{
            north: 41.0,
            south: 40.0,
            east: -73.0,
            west: -75.0
          }
        }
      })

      :timer.sleep(100)

      # The old packet should still be tracked despite 1-hour trail setting
      state = :sys.get_state(view.pid)
      socket = elem(state, 1).socket

      assert socket.assigns.tracked_callsign == "OLD-STATION"
      # 1 hour trail duration
      assert socket.assigns.packet_age_threshold == 1
      assert socket.assigns.tracked_latest_packet.id == old_packet.id

      # Cleanup handled by test transaction
    end

    test "shows no packet message when callsign has no packets", %{conn: conn} do
      # Navigate to a callsign that doesn't exist
      {:ok, view, _html} = live(conn, "/NOPACKETS-1")

      # Check that the tracked callsign is set
      assert render(view) =~ "NOPACKETS-1"

      # But there should be no packet data
      state = :sys.get_state(view.pid)
      socket = elem(state, 1).socket

      assert socket.assigns.tracked_callsign == "NOPACKETS-1"
      assert socket.assigns.tracked_latest_packet == nil
    end
  end
end
