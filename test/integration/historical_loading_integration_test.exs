defmodule AprsmeWeb.HistoricalLoadingIntegrationTest do
  @moduledoc """
  Integration tests for historical packet loading using Wallaby
  Tests the complete user experience including initial page load behavior
  """
  use ExUnit.Case, async: false
  use Wallaby.Feature

  import Aprsme.PacketsFixtures
  import Wallaby.Browser
  import Wallaby.Query

  describe "historical packet loading on page load" do
    @describetag :integration
    feature "displays historical packets immediately after map loads", %{session: session} do
      # Create test packets with known positions
      now = DateTime.utc_now()

      _packet1 =
        packet_fixture(%{
          sender: "INTTEST1",
          base_callsign: "INTTEST1",
          ssid: "0",
          lat: 40.7128,
          lon: -74.0060,
          received_at: DateTime.add(now, -20 * 60, :second),
          comment: "Integration test station 1",
          symbol_code: "k",
          symbol_table_id: "/"
        })

      _packet2 =
        packet_fixture(%{
          sender: "INTTEST2",
          base_callsign: "INTTEST2",
          ssid: "0",
          lat: 40.7580,
          lon: -73.9855,
          received_at: DateTime.add(now, -10 * 60, :second),
          comment: "Integration test station 2",
          symbol_code: "-",
          symbol_table_id: "/"
        })

      # Navigate to the map
      session
      |> visit("/")
      |> assert_has(css("#map", text: ""))

      # Wait for map to initialize and markers to appear
      # The map should automatically load historical packets
      Process.sleep(3000)

      # Check that markers are present on the map
      # Look for Leaflet marker elements
      assert_has(session, css(".leaflet-marker-icon"))

      # Verify at least 2 markers are present (our test packets)
      marker_count =
        session
        |> all(css(".leaflet-marker-icon"))
        |> length()

      assert marker_count >= 2, "Expected at least 2 markers, found #{marker_count}"

      # Click on a marker to verify it's one of our test packets
      click(session, css(".leaflet-marker-icon", at: 0))

      # Wait for popup to appear
      Process.sleep(1000)

      # Verify popup contains one of our test callsigns
      assert_has(session, css(".leaflet-popup-content", text: ~r/INTTEST[12]/))
    end

    feature "loads packets within specified historical time range", %{session: session} do
      now = DateTime.utc_now()

      # Create packets at different times
      _old_packet =
        packet_fixture(%{
          sender: "OLDTEST",
          base_callsign: "OLDTEST",
          ssid: "0",
          lat: 40.7128,
          lon: -74.0060,
          # 3 hours ago
          received_at: DateTime.add(now, -3 * 60 * 60, :second),
          comment: "Old packet - should not appear with hist=1"
        })

      _recent_packet =
        packet_fixture(%{
          sender: "RECENTTEST",
          base_callsign: "RECENTTEST",
          ssid: "0",
          lat: 40.7128,
          lon: -74.0060,
          # 30 minutes ago
          received_at: DateTime.add(now, -30 * 60, :second),
          comment: "Recent packet - should appear"
        })

      # Navigate to map with 1 hour historical range (default)
      session
      |> visit("/?hist=1")
      |> assert_has(css("#map"))

      # Wait for historical loading
      Process.sleep(3000)

      # Click on the marker (should be the recent one)
      click(session, css(".leaflet-marker-icon", at: 0))
      Process.sleep(1000)

      # Verify it's the recent packet, not the old one
      assert_has(session, css(".leaflet-popup-content", text: "RECENTTEST"))
      refute_has(session, css(".leaflet-popup-content", text: "OLDTEST"))

      # Now test with extended historical range
      session
      # 6 hours
      |> visit("/?hist=6")
      |> assert_has(css("#map"))

      Process.sleep(3000)

      # Now both packets should be visible
      marker_count =
        session
        |> all(css(".leaflet-marker-icon"))
        |> length()

      assert marker_count >= 2, "Expected at least 2 markers with 6-hour range"
    end

    feature "updates historical packets when bounds change", %{session: session} do
      now = DateTime.utc_now()

      # Create packets in different locations
      _nyc_packet =
        packet_fixture(%{
          sender: "NYC1",
          base_callsign: "NYC1",
          ssid: "0",
          lat: 40.7128,
          lon: -74.0060,
          received_at: DateTime.add(now, -30 * 60, :second),
          comment: "NYC packet"
        })

      _la_packet =
        packet_fixture(%{
          sender: "LA1",
          base_callsign: "LA1",
          ssid: "0",
          lat: 34.0522,
          lon: -118.2437,
          received_at: DateTime.add(now, -30 * 60, :second),
          comment: "LA packet"
        })

      # Start focused on NYC
      session
      |> visit("/?lat=40.7128&lng=-74.0060&z=10")
      |> assert_has(css("#map"))

      Process.sleep(3000)

      # Should see NYC packet
      click(session, css(".leaflet-marker-icon", at: 0))
      Process.sleep(1000)

      assert_has(session, css(".leaflet-popup-content", text: "NYC1"))

      # Close popup
      send_keys(session, [:escape])

      # Pan to LA (this would be done via map interaction in real usage)
      # For testing, we'll navigate to new URL
      visit(session, "/?lat=34.0522&lng=-118.2437&z=10")
      Process.sleep(3000)

      # Should now see LA packet instead
      click(session, css(".leaflet-marker-icon", at: 0))
      Process.sleep(1000)

      assert_has(session, css(".leaflet-popup-content", text: "LA1"))
    end
  end

  describe "historical loading with tracked callsigns" do
    @describetag :integration
    feature "loads all packets for tracked callsign regardless of bounds", %{session: session} do
      now = DateTime.utc_now()

      # Create packets for tracked station at different locations
      _packet1 =
        packet_fixture(%{
          sender: "TRACK1-9",
          base_callsign: "TRACK1",
          ssid: "9",
          lat: 40.7128,
          lon: -74.0060,
          received_at: DateTime.add(now, -45 * 60, :second),
          comment: "NYC position"
        })

      _packet2 =
        packet_fixture(%{
          sender: "TRACK1-9",
          base_callsign: "TRACK1",
          ssid: "9",
          lat: 41.8781,
          lon: -87.6298,
          received_at: DateTime.add(now, -30 * 60, :second),
          comment: "Chicago position"
        })

      _packet3 =
        packet_fixture(%{
          sender: "TRACK1-9",
          base_callsign: "TRACK1",
          ssid: "9",
          lat: 34.0522,
          lon: -118.2437,
          received_at: DateTime.add(now, -15 * 60, :second),
          comment: "LA position"
        })

      # Navigate to tracked callsign URL
      session
      |> visit("/TRACK1-9")
      |> assert_has(css("#map"))

      # Wait for map to load and center on latest position
      Process.sleep(3000)

      # Should see trail connecting all positions
      # Verify we have markers (latest position + trail points)
      marker_count =
        session
        |> all(css(".leaflet-marker-icon"))
        |> length()

      # Should have at least 1 marker for current position
      assert marker_count >= 1, "Expected markers for tracked station"

      # Verify polyline trail exists
      assert_has(session, css(".leaflet-pane .leaflet-overlay-pane polyline"))
    end
  end
end
