defmodule AprsmeWeb.Plugs.IPGeolocationTest do
  use AprsmeWeb.ConnCase

  alias AprsmeWeb.Plugs.IPGeolocation

  # Test IP address
  @test_ip {204, 110, 191, 254}

  setup %{conn: conn} do
    # Initialize session for all tests
    conn =
      conn
      |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
      |> fetch_session()

    {:ok, conn: conn}
  end

  describe "call/2" do
    @tag :external_api
    test "adds geolocation data to session when successful", %{conn: conn} do
      conn =
        conn
        |> Map.put(:remote_ip, @test_ip)
        |> IPGeolocation.call([])

      geo = get_session(conn, :ip_geolocation)
      assert is_map(geo)
      assert is_number(geo["lat"])
      assert is_number(geo["lng"])
      # Test IP should geolocate to somewhere in the US
      assert geo["lat"] >= 24 and geo["lat"] <= 49
      assert geo["lng"] >= -125 and geo["lng"] <= -66
    end

    test "skips geolocation for local IP addresses", %{conn: conn} do
      conn =
        conn
        |> Map.put(:remote_ip, {127, 0, 0, 1})
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "skips geolocation for private IP addresses", %{conn: _conn} do
      # Test various private IP ranges
      private_ips = [
        {10, 0, 0, 1},
        {172, 16, 0, 1},
        {192, 168, 1, 1}
      ]

      for ip <- private_ips do
        conn =
          build_conn()
          |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
          |> fetch_session()
          |> Map.put(:remote_ip, ip)
          |> IPGeolocation.call([])

        assert get_session(conn, :ip_geolocation) == nil
      end
    end

    @tag :external_api
    test "handles IPv6 addresses", %{conn: conn} do
      conn =
        conn
        |> Map.put(:remote_ip, {0x2001, 0x4860, 0x4860, 0, 0, 0, 0, 0x8888})
        |> IPGeolocation.call([])

      geo = get_session(conn, :ip_geolocation)
      assert is_map(geo)
      assert is_number(geo["lat"])
      assert is_number(geo["lng"])
    end

    @tag :external_api
    test "uses forwarded IP when behind proxy", %{conn: conn} do
      conn =
        conn
        |> Map.put(:remote_ip, {10, 0, 0, 1})
        |> put_req_header("x-forwarded-for", "204.110.191.254, 10.0.0.1")
        |> IPGeolocation.call([])

      geo = get_session(conn, :ip_geolocation)
      assert is_map(geo)
      assert is_number(geo["lat"])
      assert is_number(geo["lng"])
    end

    test "caches geolocation in session", %{conn: conn} do
      # Mock the geolocation data by setting it in session first
      geo_data = %{"lat" => 37.4056, "lng" => -122.0775}

      conn =
        conn
        |> put_session(:ip_geolocation, geo_data)
        |> Map.put(:remote_ip, @test_ip)
        |> IPGeolocation.call([])

      # Should return the cached data without making an API call
      assert get_session(conn, :ip_geolocation) == geo_data
    end

    test "skips non-root paths", %{conn: conn} do
      conn =
        conn
        |> Map.put(:remote_ip, @test_ip)
        |> Map.put(:request_path, "/about")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "skips non-GET requests", %{conn: conn} do
      conn =
        conn
        |> Map.put(:remote_ip, @test_ip)
        |> Map.put(:method, "POST")
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end
  end
end
