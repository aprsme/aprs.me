defmodule AprsmeWeb.Plugs.IPGeolocationTest do
  use AprsmeWeb.ConnCase
  use ExVCR.Mock, adapter: ExVCR.Adapter.Hackney

  alias AprsmeWeb.Plugs.IPGeolocation

  @test_ip {204, 110, 191, 254}  # Test IP address

  describe "call/2" do
    test "adds geolocation data to session when successful", %{conn: conn} do
      use_cassette "ip_geolocation_success" do
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
    end

    test "skips geolocation for local IP addresses", %{conn: conn} do
      conn =
        conn
        |> Map.put(:remote_ip, {127, 0, 0, 1})
        |> IPGeolocation.call([])

      assert get_session(conn, :ip_geolocation) == nil
    end

    test "handles API failure gracefully", %{conn: conn} do
      use_cassette "ip_geolocation_failure" do
        conn =
          conn
          |> Map.put(:remote_ip, {192, 0, 2, 1})
          |> IPGeolocation.call([])

        assert get_session(conn, :ip_geolocation) == nil
      end
    end

    test "validates latitude and longitude bounds", %{conn: conn} do
      use_cassette "ip_geolocation_invalid_coords" do
        conn =
          conn
          |> Map.put(:remote_ip, {192, 0, 2, 2})
          |> IPGeolocation.call([])

        assert get_session(conn, :ip_geolocation) == nil
      end
    end

    test "handles IPv6 addresses", %{conn: conn} do
      use_cassette "ip_geolocation_ipv6" do
        conn =
          conn
          |> Map.put(:remote_ip, {0x2001, 0x4860, 0x4860, 0, 0, 0, 0, 0x8888})
          |> IPGeolocation.call([])

        geo = get_session(conn, :ip_geolocation)
        assert is_map(geo)
        assert is_number(geo["lat"])
        assert is_number(geo["lng"])
      end
    end

    test "uses forwarded IP when behind proxy", %{conn: conn} do
      use_cassette "ip_geolocation_forwarded" do
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
    end

    test "caches geolocation in session", %{conn: conn} do
      use_cassette "ip_geolocation_cached" do
        # First request should call API
        conn =
          conn
          |> Map.put(:remote_ip, @test_ip)
          |> IPGeolocation.call([])
          |> fetch_session()

        geo1 = get_session(conn, :ip_geolocation)

        # Second request with same session should not call API
        conn2 =
          conn
          |> recycle()
          |> Map.put(:remote_ip, @test_ip)
          |> IPGeolocation.call([])

        geo2 = get_session(conn2, :ip_geolocation)
        
        # Should return the same cached data
        assert geo2 == geo1
      end
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