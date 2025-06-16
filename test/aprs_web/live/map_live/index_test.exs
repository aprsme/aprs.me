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

    test "handles adjust_replay_speed event with float values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Test with float speed value
      speed_params = %{"speed" => 1.5}
      assert render_hook(view, "adjust_replay_speed", speed_params)
    end

    test "handles adjust_replay_speed event with string values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Test with string speed value
      speed_params = %{"speed" => "2.0"}
      assert render_hook(view, "adjust_replay_speed", speed_params)
    end

    test "handles adjust_replay_speed event with integer values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Test with integer speed value
      speed_params = %{"speed" => 3}
      assert render_hook(view, "adjust_replay_speed", speed_params)
    end

    test "handles adjust_replay_speed event with integer string values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Test with integer string value (should work with improved Float.parse)
      speed_params = %{"speed" => "1"}
      assert render_hook(view, "adjust_replay_speed", speed_params)
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
