defmodule AprsmeWeb.Plugs.IPGeolocationTest do
  use AprsmeWeb.ConnCase

  alias AprsmeWeb.Plugs.IPGeolocation

  setup %{conn: conn} do
    conn =
      conn
      |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
      |> fetch_session()

    {:ok, conn: conn}
  end

  describe "call/2 with Cloudflare headers" do
    test "sets session from CF-IPLatitude and CF-IPLongitude headers on root GET", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-iplatitude", "37.7749")
        |> put_req_header("cf-iplongitude", "-122.4194")
        |> IPGeolocation.call([])

      geo = get_session(conn, :ip_geolocation)
      assert geo == %{"lat" => 37.7749, "lng" => -122.4194}
    end

    test "skips when CF headers are missing", %{conn: conn} do
      conn = IPGeolocation.call(conn, [])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "skips when only latitude header is present", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-iplatitude", "37.7749")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "skips when only longitude header is present", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-iplongitude", "-122.4194")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "works on non-root paths", %{conn: conn} do
      conn =
        conn
        |> Map.put(:request_path, "/about")
        |> put_req_header("cf-iplatitude", "37.7749")
        |> put_req_header("cf-iplongitude", "-122.4194")
        |> IPGeolocation.call([])

      geo = get_session(conn, :ip_geolocation)
      assert geo == %{"lat" => 37.7749, "lng" => -122.4194}
    end

    test "skips non-GET requests", %{conn: conn} do
      conn =
        conn
        |> Map.put(:method, "POST")
        |> put_req_header("cf-iplatitude", "37.7749")
        |> put_req_header("cf-iplongitude", "-122.4194")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "uses cached session data without re-reading headers", %{conn: conn} do
      geo_data = %{"lat" => 40.7128, "lng" => -74.006}

      conn =
        conn
        |> put_session(:ip_geolocation, geo_data)
        |> put_req_header("cf-iplatitude", "37.7749")
        |> put_req_header("cf-iplongitude", "-122.4194")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == geo_data
    end

    test "rejects latitude out of range", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-iplatitude", "91.0")
        |> put_req_header("cf-iplongitude", "-122.4194")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "rejects longitude out of range", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-iplatitude", "37.7749")
        |> put_req_header("cf-iplongitude", "181.0")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "rejects non-numeric header values", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-iplatitude", "not_a_number")
        |> put_req_header("cf-iplongitude", "-122.4194")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "handles boundary coordinate values", %{conn: conn} do
      conn =
        conn
        |> put_req_header("cf-iplatitude", "-90.0")
        |> put_req_header("cf-iplongitude", "180.0")
        |> IPGeolocation.call([])

      geo = get_session(conn, :ip_geolocation)
      assert geo == %{"lat" => -90.0, "lng" => 180.0}
    end
  end
end
