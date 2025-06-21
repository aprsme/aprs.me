defmodule AprsWeb.MapLive.IndexTest do
  use AprsWeb.ConnCase

  import Phoenix.LiveViewTest

  describe "Index" do
    test "renders map view", %{conn: conn} do
      {:ok, view, html} = live(conn, "/")

      assert html =~ "APRS Map"
      assert has_element?(view, "#aprs-map")
    end

    test "handles bounds_changed event with float values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Simulate JavaScript sending float values (the problematic case)
      bounds_params = %{
        "bounds" => %{
          "north" => 61.897577621605016,
          "south" => 5.441022303717974,
          "east" => -34.45312500000001,
          "west" => -161.54296875000003
        }
      }

      # This should not crash with ArgumentError: not a binary
      assert render_hook(view, "bounds_changed", bounds_params)
    end

    test "handles bounds_changed event with string values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Simulate JavaScript sending string values (expected case)
      bounds_params = %{
        "bounds" => %{
          "north" => "61.897577621605016",
          "south" => "5.441022303717974",
          "east" => "-34.45312500000001",
          "west" => "-161.54296875000003"
        }
      }

      assert render_hook(view, "bounds_changed", bounds_params)
    end

    test "handles bounds_changed event with mixed value types", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Test mixed types (float, string, integer)
      bounds_params = %{
        "bounds" => %{
          # float
          "north" => 61.897577621605016,
          # string
          "south" => "5.441022303717974",
          # integer
          "east" => -34,
          # string
          "west" => "-161.54296875000003"
        }
      }

      assert render_hook(view, "bounds_changed", bounds_params)
    end

    test "loads historical packets when map is ready", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Simulate map initialization which should trigger historical packet loading
      assert render_hook(view, "map_ready", %{})

      # Wait for the initialize_replay message to be processed
      Process.sleep(600)

      # The view should still be rendering without errors after loading historical packets
      assert render(view) =~ "aprs-map"
    end

    test "handles clear_and_reload_markers event", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # This should not crash and should work without replay functionality
      assert render_hook(view, "clear_and_reload_markers", %{})
    end

    test "sets correct page title", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      assert page_title(view) =~ "APRS Map"
    end

    test "handles map_ready event", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Simulate map initialization
      assert render_hook(view, "map_ready", %{})
    end
  end
end
