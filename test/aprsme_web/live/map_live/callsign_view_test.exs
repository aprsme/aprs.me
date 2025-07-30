defmodule AprsmeWeb.MapLive.CallsignViewTest do
  use AprsmeWeb.ConnCase

  import Phoenix.LiveViewTest

  describe "CallsignView displays map for callsign" do
    test "displays map with callsign in path", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP-9")
      assert has_element?(view, "#aprs-map")
      assert render(view) =~ "W5ISP-9"
    end

    test "handles lowercase callsigns", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/w5isp-9")
      assert has_element?(view, "#aprs-map")
      # Callsigns are normalized to uppercase
      assert render(view) =~ "W5ISP-9"
    end

    test "handles callsign without SSID", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/W5ISP")
      assert has_element?(view, "#aprs-map")
      assert render(view) =~ "W5ISP"
    end

    test "handles special characters in callsign", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/TEST-123")
      assert has_element?(view, "#aprs-map")
      assert render(view) =~ "TEST-123"
    end
  end
end
