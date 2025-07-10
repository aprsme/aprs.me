defmodule AprsmeWeb.MapLive.OverlayRenderingTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.MapLive.PacketUtils

  describe "overlay symbol rendering in map" do
    test "W5MRC-15 with D& symbol generates correct HTML" do
      # Create a test packet with overlay symbol D&
      packet = %{
        id: 1,
        sender: "W5MRC-15",
        base_callsign: "W5MRC",
        ssid: "15",
        symbol_table_id: "D",
        symbol_code: "&",
        lat: 30.0,
        lon: -95.0,
        comment: "Test station",
        received_at: DateTime.utc_now(),
        data_type: "position"
      }

      # Process the packet through PacketUtils
      result = PacketUtils.build_packet_data(packet, true, "en-US")

      # Check that symbol_html was generated
      assert Map.has_key?(result, "symbol_html")
      symbol_html = result["symbol_html"]

      # Verify the overlay symbol is rendered correctly with overlay character on top
      # The overlay character (D) from table 2, base symbol (&) from table 1
      assert symbol_html =~
               "background-image: url(/aprs-symbols/aprs-symbols-128-2@2x.png), url(/aprs-symbols/aprs-symbols-128-1@2x.png)"

      # Verify the positions (overlay character D first at -96.0px -64.0px, then base & at -160.0px 0.0px)
      assert symbol_html =~ "background-position: -96.0px -64.0px, -160.0px 0.0px"

      # Verify callsign is included
      assert symbol_html =~ "W5MRC-15"
    end

    test "N# green star overlay symbol generates correct HTML" do
      # Create a test packet with overlay symbol N#
      packet = %{
        id: 2,
        sender: "TEST-1",
        base_callsign: "TEST",
        ssid: "1",
        symbol_table_id: "N",
        symbol_code: "#",
        lat: 35.0,
        lon: -100.0,
        comment: "Green star test",
        received_at: DateTime.utc_now(),
        data_type: "position"
      }

      # Process the packet
      result = PacketUtils.build_packet_data(packet, true, "en-US")
      symbol_html = result["symbol_html"]

      # Verify the overlay uses different sprite tables
      # Overlay character N from table 2, base # from table 1
      assert symbol_html =~
               "background-image: url(/aprs-symbols/aprs-symbols-128-2@2x.png), url(/aprs-symbols/aprs-symbols-128-1@2x.png)"

      # Verify the positions (overlay N first, then base #)
      assert symbol_html =~ "background-position: -416.0px -64.0px, -64.0px 0.0px"

      # Verify callsign
      assert symbol_html =~ "TEST-1"
    end

    test "normal symbol /_ generates correct HTML without overlay" do
      # Create a test packet with normal symbol /_
      packet = %{
        id: 3,
        sender: "WEATHER-1",
        base_callsign: "WEATHER",
        ssid: "1",
        symbol_table_id: "/",
        symbol_code: "_",
        lat: 40.0,
        lon: -105.0,
        comment: "Weather station",
        received_at: DateTime.utc_now(),
        data_type: "weather"
      }

      # Process the packet
      result = PacketUtils.build_packet_data(packet, true, "en-US")
      symbol_html = result["symbol_html"]

      # Verify it's a single background image (no overlay)
      assert symbol_html =~ "background-image: url(/aprs-symbols/aprs-symbols-128-0@2x.png)"
      assert not (symbol_html =~ "background-image: url(/aprs-symbols/aprs-symbols-128-0@2x.png), url")

      # Verify the position for _ symbol
      assert symbol_html =~ "background-position: -448.0px -96.0px"

      # Verify callsign
      assert symbol_html =~ "WEATHER-1"
    end
  end
end
