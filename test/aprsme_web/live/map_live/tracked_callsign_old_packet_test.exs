defmodule AprsmeWeb.MapLive.TrackedCallsignOldPacketTest do
  use AprsmeWeb.ConnCase

  import Aprsme.PacketsFixtures
  import Phoenix.LiveViewTest

  describe "tracked callsign shows last packet regardless of age" do
    test "displays old packet for tracked callsign", %{conn: conn} do
      # Create a packet that's 30 days old
      thirty_days_ago = DateTime.add(DateTime.utc_now(), -30 * 24 * 60 * 60, :second)

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

      # Check that the tracked callsign is displayed
      assert render(view) =~ "TEST-OLD"
      # Verify tracking indicator is shown
      assert render(view) =~ "Tracking"
    end

    test "includes old packet even with 1-hour trail duration", %{conn: conn} do
      # Create a packet that's 48 hours old
      two_days_ago = DateTime.add(DateTime.utc_now(), -48 * 60 * 60, :second)

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

      # The old packet should still be tracked despite 1-hour trail setting
      assert render(view) =~ "OLD-STATION"
      assert render(view) =~ "Tracking"
    end

    test "shows no packet message when callsign has no packets", %{conn: conn} do
      # Navigate to a callsign that doesn't exist
      {:ok, view, _html} = live(conn, "/NOPACKETS-1")

      # Check that the tracked callsign is set
      assert render(view) =~ "NOPACKETS-1"
      assert render(view) =~ "Tracking"
    end

    test "displays non-position packet for tracked callsign", %{conn: conn} do
      # Create a status packet without position data - this is the key test case
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

      # Check that the tracked callsign is set
      assert render(view) =~ "TEST-STATUS"
      assert render(view) =~ "Tracking"
    end

    test "displays message packet without position for tracked callsign", %{conn: conn} do
      # Create a message packet without position data
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
      assert render(view) =~ "TEST-MSG"
      assert render(view) =~ "Tracking"
    end

    test "handles whitespace in callsign parameter", %{conn: conn} do
      # Create a packet for testing
      packet_fixture(%{
        sender: "TEST-WS",
        raw_packet: "TEST-WS>APRS:>Test whitespace handling",
        has_position: false
      })

      # Navigate with extra whitespace
      {:ok, view, _html} = live(conn, "/  TEST-WS  ")

      # Should normalize to uppercase and trim whitespace
      assert render(view) =~ "TEST-WS"
      assert render(view) =~ "Tracking"
    end

    test "handles lowercase callsign with non-position packet", %{conn: conn} do
      # Create a telemetry packet without position
      packet_fixture(%{
        sender: "TEST-TELEM",
        raw_packet: "TEST-TELEM>APRS:T#123,456,789,012,345,678,00000000",
        has_position: false,
        data_type: "telemetry"
      })

      # Navigate with lowercase callsign
      {:ok, view, _html} = live(conn, "/test-telem")

      # Should normalize to uppercase
      assert render(view) =~ "TEST-TELEM"
      assert render(view) =~ "Tracking"
    end
  end
end