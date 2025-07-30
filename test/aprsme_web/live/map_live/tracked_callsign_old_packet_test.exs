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

    test "displays non-position packet for tracked callsign", %{conn: conn} do
      # Create a status packet without position data - this is the key test case
      non_position_packet =
        packet_fixture(%{
          sender: "TEST-STATUS",
          raw_packet: "TEST-STATUS>APRS:>Status message without position",
          received_at: DateTime.utc_now(),
          has_position: false,
          lat: nil,
          lon: nil,
          data_type: "status"
        })

      # Navigate to the tracked callsign URL
      {:ok, view, _html} = live(conn, "/TEST-STATUS")

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

      :timer.sleep(100)

      # Check that the tracked callsign is set
      assert render(view) =~ "TEST-STATUS"

      # The non-position packet should still be loaded
      state = :sys.get_state(view.pid)
      socket = elem(state, 1).socket

      assert socket.assigns.tracked_callsign == "TEST-STATUS"
      assert socket.assigns.tracked_callsign_latest_packet.id == non_position_packet.id
      assert socket.assigns.tracked_callsign_latest_packet.has_position == false
    end

    test "displays message packet without position for tracked callsign", %{conn: conn} do
      # Create a message packet without position data
      message_packet =
        packet_fixture(%{
          sender: "TEST-MSG",
          raw_packet: "TEST-MSG>APRS::W5ISP    :Hello there! This is a test message{001",
          # 1 hour old
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second),
          has_position: false,
          lat: nil,
          lon: nil,
          data_type: "message",
          aprs_messaging: true
        })

      # Navigate to the tracked callsign URL
      {:ok, view, _html} = live(conn, "/TEST-MSG")

      # The message packet should be tracked despite no position
      state = :sys.get_state(view.pid)
      socket = elem(state, 1).socket

      assert socket.assigns.tracked_callsign == "TEST-MSG"
      assert socket.assigns.tracked_callsign_latest_packet.id == message_packet.id
      assert socket.assigns.tracked_callsign_latest_packet.aprs_messaging == true
    end

    test "handles whitespace in callsign parameter", %{conn: conn} do
      # Create a packet for testing
      packet =
        packet_fixture(%{
          sender: "TEST-WS",
          raw_packet: "TEST-WS>APRS:>Test whitespace handling",
          has_position: false
        })

      # Navigate with extra whitespace
      {:ok, view, _html} = live(conn, "/  TEST-WS  ")

      # Should normalize to uppercase and trim whitespace
      state = :sys.get_state(view.pid)
      socket = elem(state, 1).socket

      # The callsign should be normalized (trimmed and uppercased)
      assert socket.assigns.tracked_callsign == "TEST-WS"
      assert socket.assigns.tracked_callsign_latest_packet.id == packet.id
    end

    test "handles lowercase callsign with non-position packet", %{conn: conn} do
      # Create a telemetry packet without position
      telemetry_packet =
        packet_fixture(%{
          sender: "TEST-TELEM",
          raw_packet: "TEST-TELEM>APRS:T#123,456,789,012,345,678,00000000",
          has_position: false,
          data_type: "telemetry"
        })

      # Navigate with lowercase callsign
      {:ok, view, _html} = live(conn, "/test-telem")

      state = :sys.get_state(view.pid)
      socket = elem(state, 1).socket

      # Should normalize to uppercase
      assert socket.assigns.tracked_callsign == "TEST-TELEM"
      assert socket.assigns.tracked_callsign_latest_packet.id == telemetry_packet.id
    end
  end
end
