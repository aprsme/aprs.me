defmodule AprsmeWeb.MapLive.CallsignViewTest do
  use AprsmeWeb.ConnCase

  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  describe "CallsignView" do
    test "renders callsign view with valid callsign", %{conn: conn} do
      {:ok, view, html} = live(conn, "/W5ISP-9")

      assert html =~ "W5ISP-9"
      assert html =~ "Back to Map"
      assert html =~ "Packets"
      assert has_element?(view, "#aprs-map")
    end

    test "normalizes callsign to uppercase", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/w5isp-9")

      assert html =~ "W5ISP-9"
    end

    test "shows loading state when no packets found", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/TESTCALL-1")

      assert html =~ "Loading Historical Data"
      assert html =~ "Loading packet history for TESTCALL-1"
    end

    test "handles callsign without SSID", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/W5ISP")

      assert html =~ "W5ISP"
    end

    test "sets correct page title", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      assert page_title(view) =~ "APRS Map - W5ISP-9"
    end

    test "handles bounds_changed event with float values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

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
      {:ok, view, _html} = live(conn, "/W5ISP-9")

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
      {:ok, view, _html} = live(conn, "/W5ISP-9")

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
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      # Test with float speed value
      speed_params = %{"speed" => 1.5}
      assert render_hook(view, "adjust_replay_speed", speed_params)
    end

    test "handles adjust_replay_speed event with string values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      # Test with string speed value
      speed_params = %{"speed" => "2.0"}
      assert render_hook(view, "adjust_replay_speed", speed_params)
    end

    test "handles adjust_replay_speed event with integer values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      # Test with integer speed value
      speed_params = %{"speed" => 3}
      assert render_hook(view, "adjust_replay_speed", speed_params)
    end
  end
end
