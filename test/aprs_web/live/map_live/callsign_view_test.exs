defmodule AprsWeb.MapLive.CallsignViewTest do
  use AprsWeb.ConnCase

  import Phoenix.LiveViewTest

  describe "CallsignView" do
    test "renders callsign view with valid callsign", %{conn: conn} do
      {:ok, view, html} = live(conn, "/W5ISP-9")

      assert html =~ "📡 W5ISP-9"
      assert html =~ "Back to Map"
      assert html =~ "Packets"
      assert has_element?(view, "#aprs-map")
    end

    test "normalizes callsign to uppercase", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/w5isp-9")

      assert html =~ "📡 W5ISP-9"
    end

    test "shows empty state when no packets found", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/TESTCALL-1")

      assert html =~ "No Recent Packets"
      assert html =~ "No packets found for TESTCALL-1"
    end

    test "handles callsign without SSID", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/W5ISP")

      assert html =~ "📡 W5ISP"
    end

    test "has replay controls", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      assert has_element?(view, "button", "Start Replay")
    end

    test "can start and stop replay", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      # Start replay
      view |> element("button", "Start Replay") |> render_click()
      assert has_element?(view, "button", "Stop Replay")
      assert has_element?(view, "button", "Pause")

      # Stop replay
      view |> element("button", "Stop Replay") |> render_click()
      assert has_element?(view, "button", "Start Replay")
    end

    test "handles locate me button", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      assert has_element?(view, "button[title='Find my location']")

      # Click locate button - should trigger geolocation request
      view |> element("button[title='Find my location']") |> render_click()
      # Should not crash - actual geolocation testing would require JavaScript
    end

    test "sets correct page title", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")

      assert page_title(view) =~ "APRS Map - W5ISP-9"
    end
  end
end
