defmodule AprsmeWeb.Plugs.RemoteIpTest do
  use AprsmeWeb.ConnCase

  alias AprsmeWeb.Plugs.RemoteIp

  describe "call/2" do
    test "sets remote_ip from CF-Connecting-IP header", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-connecting-ip", "203.0.113.50")
        |> RemoteIp.call([])

      assert conn.remote_ip == {203, 0, 113, 50}
    end

    test "sets remote_ip from X-Forwarded-For when CF header missing", %{conn: conn} do
      conn =
        conn
        |> put_req_header("x-forwarded-for", "198.51.100.25, 10.0.0.1")
        |> RemoteIp.call([])

      assert conn.remote_ip == {198, 51, 100, 25}
    end

    test "prefers CF-Connecting-IP over X-Forwarded-For", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-connecting-ip", "203.0.113.50")
        |> put_req_header("x-forwarded-for", "198.51.100.25")
        |> RemoteIp.call([])

      assert conn.remote_ip == {203, 0, 113, 50}
    end

    test "handles IPv6 addresses", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-connecting-ip", "2001:db8::1")
        |> RemoteIp.call([])

      assert conn.remote_ip == {8193, 3512, 0, 0, 0, 0, 0, 1}
    end

    test "leaves remote_ip unchanged when no proxy headers present", %{conn: conn} do
      original_ip = conn.remote_ip

      conn = RemoteIp.call(conn, [])

      assert conn.remote_ip == original_ip
    end

    test "leaves remote_ip unchanged when header contains invalid IP", %{conn: conn} do
      original_ip = conn.remote_ip

      conn =
        conn
        |> put_req_header("cf-connecting-ip", "not-an-ip")
        |> RemoteIp.call([])

      assert conn.remote_ip == original_ip
    end

    test "trims whitespace from IP addresses", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-connecting-ip", " 203.0.113.50 ")
        |> RemoteIp.call([])

      assert conn.remote_ip == {203, 0, 113, 50}
    end

    test "takes first IP from X-Forwarded-For chain", %{conn: conn} do
      conn =
        conn
        |> put_req_header("x-forwarded-for", "203.0.113.50, 10.0.0.1, 172.16.0.1")
        |> RemoteIp.call([])

      assert conn.remote_ip == {203, 0, 113, 50}
    end
  end
end
