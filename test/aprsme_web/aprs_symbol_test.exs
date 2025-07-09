defmodule AprsmeWeb.AprsSymbolTest do
  use ExUnit.Case, async: true
  alias AprsmeWeb.AprsSymbol

  describe "overlay symbol rendering" do
    test "D& should show black diamond background from table 1" do
      # Test the get_sprite_info for overlay symbol D&
      sprite_info = AprsSymbol.get_sprite_info("D", "&")
      
      # Should use table 1 for the diamond background (alternate table)
      assert sprite_info.sprite_file == "/aprs-symbols/aprs-symbols-128-1@2x.png"
      
      # Calculate expected position for & (ASCII 38)
      # index = 38 - 33 = 5
      # column = 5 % 16 = 5
      # row = 5 / 16 = 0
      # x = -5 * 128 = -640
      # y = -0 * 128 = 0
      # scaled: x/4 = -160, y/4 = 0
      assert sprite_info.background_position == "-160.0px 0.0px"
    end

    test "N# should show green star background from table 1" do
      # Test the get_sprite_info for overlay symbol N#
      sprite_info = AprsSymbol.get_sprite_info("N", "#")
      
      # Should use table 1 for the green star background
      assert sprite_info.sprite_file == "/aprs-symbols/aprs-symbols-128-1@2x.png"
      
      # Calculate expected position for # (ASCII 35)
      # index = 35 - 33 = 2
      # column = 2 % 16 = 2
      # row = 2 / 16 = 0
      # x = -2 * 128 = -256
      # y = -0 * 128 = 0
      # scaled: x/4 = -64, y/4 = 0
      assert sprite_info.background_position == "-64.0px 0.0px"
    end

    test "overlay character sprite info for D" do
      # Test getting the overlay character sprite for letter D
      overlay_info = AprsSymbol.get_overlay_character_sprite_info("D")
      
      # Should use table 2 for overlay characters
      assert overlay_info.sprite_file == "/aprs-symbols/aprs-symbols-128-2@2x.png"
      
      # Calculate expected position for D (ASCII 68)
      # index = 68 - 33 = 35
      # column = 35 % 16 = 3
      # row = 35 / 16 = 2
      # x = -3 * 128 = -384
      # y = -2 * 128 = -256
      # scaled: x/4 = -96, y/4 = -64
      assert overlay_info.background_position == "-96.0px -64.0px"
    end

    test "overlay character sprite info for N" do
      # Test getting the overlay character sprite for letter N
      overlay_info = AprsSymbol.get_overlay_character_sprite_info("N")
      
      # Should use table 2 for overlay characters
      assert overlay_info.sprite_file == "/aprs-symbols/aprs-symbols-128-2@2x.png"
      
      # Calculate expected position for N (ASCII 78)
      # index = 78 - 33 = 45
      # column = 45 % 16 = 13
      # row = 45 / 16 = 2
      # x = -13 * 128 = -1664
      # y = -2 * 128 = -256
      # scaled: x/4 = -416, y/4 = -64
      assert overlay_info.background_position == "-416.0px -64.0px"
    end

    test "render_marker_html for overlay symbol D&" do
      html = AprsSymbol.render_marker_html("D", "&", "W5MRC-15", 32)
      
      # Should contain both background images (overlay first from table 2, base from table 1)
      assert html =~ "background-image: url(/aprs-symbols/aprs-symbols-128-2@2x.png), url(/aprs-symbols/aprs-symbols-128-1@2x.png)"
      
      # Should have both background positions (overlay first, then base)
      assert html =~ "background-position: -96.0px -64.0px, -160.0px 0.0px"
      
      # Should include the callsign
      assert html =~ "W5MRC-15"
    end

    test "render_marker_html for overlay symbol N#" do
      html = AprsSymbol.render_marker_html("N", "#", "TEST-1", 32)
      
      # Should contain both background images (overlay first from table 2, base from table 1)
      assert html =~ "background-image: url(/aprs-symbols/aprs-symbols-128-2@2x.png), url(/aprs-symbols/aprs-symbols-128-1@2x.png)"
      
      # Should have both background positions (overlay first, then base)
      assert html =~ "background-position: -416.0px -64.0px, -64.0px 0.0px"
      
      # Should include the callsign
      assert html =~ "TEST-1"
    end

    test "normal symbol /_" do
      # Test a normal symbol from primary table
      sprite_info = AprsSymbol.get_sprite_info("/", "_")
      
      assert sprite_info.sprite_file == "/aprs-symbols/aprs-symbols-128-0@2x.png"
      
      # Calculate expected position for _ (ASCII 95)
      # index = 95 - 33 = 62
      # column = 62 % 16 = 14
      # row = 62 / 16 = 3
      # x = -14 * 128 = -1792
      # y = -3 * 128 = -384
      # scaled: x/4 = -448, y/4 = -96
      assert sprite_info.background_position == "-448.0px -96.0px"
    end

    test "alternate table symbol \\#" do
      # Test a symbol from alternate table
      sprite_info = AprsSymbol.get_sprite_info("\\", "#")
      
      assert sprite_info.sprite_file == "/aprs-symbols/aprs-symbols-128-1@2x.png"
      
      # Calculate expected position for # (ASCII 35)
      # index = 35 - 33 = 2
      # column = 2 % 16 = 2
      # row = 2 / 16 = 0
      # x = -2 * 128 = -256
      # y = -0 * 128 = 0
      # scaled: x/4 = -64, y/4 = 0
      assert sprite_info.background_position == "-64.0px 0.0px"
    end

    test "get_overlay_base_table_id mapping" do
      # Test the table mapping for overlay base symbols
      assert AprsSymbol.get_overlay_base_table_id("#") == "1"  # Green star in alternate table
      assert AprsSymbol.get_overlay_base_table_id("&") == "1"  # Diamond in alternate table
      assert AprsSymbol.get_overlay_base_table_id("i") == "1"  # Black square in alternate table
      assert AprsSymbol.get_overlay_base_table_id(">") == "1"  # Arrow in alternate table
      assert AprsSymbol.get_overlay_base_table_id("^") == "1"  # Arrow in alternate table
      assert AprsSymbol.get_overlay_base_table_id("?") == "1"  # Default to alternate table
    end
  end
end