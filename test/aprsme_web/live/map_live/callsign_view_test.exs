defmodule AprsmeWeb.MapLive.CallsignViewTest do
  use AprsmeWeb.ConnCase

  import Phoenix.ConnTest

  describe "CallsignView redirects" do
    test "redirects to main map with call parameter", %{conn: conn} do
      conn = get(conn, "/W5ISP-9")
      assert redirected_to(conn) == "/?call=W5ISP-9"
    end

    test "redirects lowercase callsigns", %{conn: conn} do
      conn = get(conn, "/w5isp-9")
      assert redirected_to(conn) == "/?call=w5isp-9"
    end

    test "redirects callsign without SSID", %{conn: conn} do
      conn = get(conn, "/W5ISP")
      assert redirected_to(conn) == "/?call=W5ISP"
    end

    test "redirects special characters in callsign", %{conn: conn} do
      conn = get(conn, "/TEST-123")
      assert redirected_to(conn) == "/?call=TEST-123"
    end
  end
end
