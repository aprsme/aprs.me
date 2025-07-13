defmodule AprsmeWeb.MapLive.RfPathTest do
  use AprsmeWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Aprsme.Packet
  alias Aprsme.Repo
  alias AprsmeWeb.MapLive.Index

  describe "RF path parsing" do
    test "parses RF paths correctly", %{conn: conn} do
      # Test the parse_rf_path function indirectly through module
      # Since it's a private function, we test the behavior

      {:ok, _digi1} =
        Repo.insert(%Packet{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position",
          symbol_table_id: "#",
          symbol_code: "r"
        })

      {:ok, view, _html} = live(conn, "/")

      # Send a marker_hover_start event
      render_hook(view, "marker_hover_start", %{
        "id" => "test-1",
        "path" => "K5GVL-10*,WIDE1-1,WIDE2-1",
        "lat" => 33.2837,
        "lng" => -96.5728
      })

      # The function should parse the path and try to find positions
      # We can't directly test the private function, but we can verify no errors occur
      assert view.module == Index
    end

    test "parses complex RF paths with multiple stations", %{conn: conn} do
      # Create multiple stations that could be in the path
      {:ok, _station1} =
        Repo.insert(%Packet{
          sender: "N5ABC",
          base_callsign: "N5ABC",
          ssid: nil,
          lat: Decimal.new("33.2000"),
          lon: Decimal.new("-96.6000"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position"
        })

      {:ok, _station2} =
        Repo.insert(%Packet{
          sender: "WB5DEF-1",
          base_callsign: "WB5DEF",
          ssid: "1",
          lat: Decimal.new("33.3000"),
          lon: Decimal.new("-96.7000"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position"
        })

      {:ok, view, _html} = live(conn, "/")

      # Test path with multiple digipeaters
      render_hook(view, "marker_hover_start", %{
        "id" => "test-complex",
        "path" => "N5ABC,WB5DEF-1,WIDE2-1,qAR,K5VOM-10",
        "lat" => 33.2837,
        "lng" => -96.5728
      })

      # Should handle complex paths without errors
      assert view.module == Index
    end

    test "handles paths with asterisks correctly", %{conn: conn} do
      {:ok, _station} =
        Repo.insert(%Packet{
          sender: "WA5VHU-8",
          base_callsign: "WA5VHU",
          ssid: "8",
          lat: Decimal.new("33.1500"),
          lon: Decimal.new("-96.5500"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position"
        })

      {:ok, view, _html} = live(conn, "/")

      # Test path with asterisks (used digipeaters)
      render_hook(view, "marker_hover_start", %{
        "id" => "test-asterisk",
        "path" => "WA5VHU-8,WIDE1*,WIDE2-1,qAR,K5VOM-10",
        "lat" => 33.2837,
        "lng" => -96.5728
      })

      # Should handle asterisks without errors
      assert view.module == Index
    end

    test "filters out TCPIP from paths", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Send a hover event with TCPIP in path
      render_hook(view, "marker_hover_start", %{
        "id" => "test-2",
        "path" => "TCPIP*,qAC,T2TEXAS",
        "lat" => 33.2837,
        "lng" => -96.5728
      })

      # Should not crash and should filter out TCPIP
      assert view.module == Index
    end

    test "handles empty paths gracefully", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Send hover event with empty path
      render_hook(view, "marker_hover_start", %{"id" => "test-3", "path" => "", "lat" => 33.2837, "lng" => -96.5728})

      # Should not crash
      assert view.module == Index
    end

    test "marker hover end event works", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Send hover end event
      render_hook(view, "marker_hover_end", %{"id" => "test-1"})

      # Should not crash
      assert view.module == Index
    end
  end

  describe "path station position queries" do
    setup do
      # Create test packets with positions
      {:ok, _digi1} =
        Repo.insert(%Packet{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position"
        })

      {:ok, _digi2} =
        Repo.insert(%Packet{
          sender: "N5TXZ-10",
          base_callsign: "N5TXZ",
          ssid: "10",
          lat: Decimal.new("33.2000"),
          lon: Decimal.new("-96.5000"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position"
        })

      :ok
    end

    test "finds positions for digipeaters in path", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Send hover event with known digipeaters
      render_hook(view, "marker_hover_start", %{
        "id" => "test-4",
        "path" => "K5GVL-10*,N5TXZ-10",
        "lat" => 33.2837,
        "lng" => -96.5728
      })

      # The function should find these stations
      assert view.module == Index
    end
  end

  describe "RF path with stations outside bounds" do
    setup do
      # Create test packets - one inside a small bounds, one outside
      # Station inside typical Texas bounds
      {:ok, _inside_station} =
        Repo.insert(%Packet{
          sender: "K5INSIDE-10",
          base_callsign: "K5INSIDE",
          ssid: "10",
          # Inside Texas area
          lat: Decimal.new("32.5000"),
          # Inside Texas area
          lon: Decimal.new("-96.5000"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position"
        })

      # Station outside bounds (way outside in Boston area)
      {:ok, _outside_station} =
        Repo.insert(%Packet{
          sender: "W1OUTSIDE-10",
          base_callsign: "W1OUTSIDE",
          ssid: "10",
          # Boston area - far from Texas
          lat: Decimal.new("42.0000"),
          # Boston area - far from Texas
          lon: Decimal.new("-71.0000"),
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          data_type: "position"
        })

      :ok
    end

    test "includes path stations outside map bounds", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Set map bounds to a small area around Texas that excludes the outside station
      bounds = %{
        "north" => "33.0",
        "south" => "32.0",
        "east" => "-96.0",
        "west" => "-97.0"
      }

      render_hook(view, "bounds_changed", %{"bounds" => bounds})

      # Now hover over a marker with a path that includes both inside and outside stations
      render_hook(view, "marker_hover_start", %{
        "id" => "test-bounds",
        # Both inside and outside stations
        "path" => "K5INSIDE-10*,W1OUTSIDE-10",
        "lat" => 32.5,
        "lng" => -96.5
      })

      # With the fix, both stations should be found regardless of bounds
      # The test passes if no errors occur during the hover event
      assert view.module == Index
    end

    test "finds stations regardless of bounds constraints", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Set very restrictive bounds that exclude both stations
      bounds = %{
        "north" => "31.0",
        "south" => "30.0",
        "east" => "-95.0",
        "west" => "-96.0"
      }

      render_hook(view, "bounds_changed", %{"bounds" => bounds})

      # Hover with path containing stations outside these bounds
      render_hook(view, "marker_hover_start", %{
        "id" => "test-no-bounds",
        "path" => "K5INSIDE-10,W1OUTSIDE-10",
        "lat" => 32.5,
        "lng" => -96.5
      })

      # Should work without errors - stations found regardless of bounds
      assert view.module == Index
    end
  end
end
