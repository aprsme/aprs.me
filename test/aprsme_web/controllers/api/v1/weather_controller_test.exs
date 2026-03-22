defmodule AprsmeWeb.Api.V1.WeatherControllerTest do
  use AprsmeWeb.ConnCase, async: false

  import Aprsme.PacketsFixtures

  describe "GET /api/v1/weather/nearby" do
    setup do
      # Create a weather station packet
      station =
        packet_fixture(%{
          sender: "TEST-1",
          raw_packet: "TEST-1>APRS,TCPIP*:@121045z3745.50N/12159.75W_090/000g000t068r000p000P000h50b10120",
          data_type: "weather",
          lat: Decimal.new("37.7583"),
          lon: Decimal.new("-121.9958"),
          has_weather: true,
          temperature: 68.0,
          humidity: 50.0,
          pressure: 1012.0,
          wind_speed: 0.0,
          wind_direction: 90,
          rain_1h: 0.0,
          rain_24h: 0.0,
          rain_since_midnight: 0.0
        })

      # Create a non-weather packet (should not appear)
      packet_fixture(%{
        sender: "TEST-2",
        data_type: "position",
        lat: Decimal.new("37.7583"),
        lon: Decimal.new("-121.9958"),
        has_weather: false
      })

      # Create a weather station outside radius
      packet_fixture(%{
        sender: "TEST-3",
        data_type: "weather",
        lat: Decimal.new("40.0"),
        lon: Decimal.new("-120.0"),
        has_weather: true,
        temperature: 75.0
      })

      %{station: station}
    end

    test "returns nearby weather stations with valid params", %{conn: conn, station: station} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "10"
        })

      assert %{
               "data" => [weather_station],
               "meta" => %{
                 "count" => 1,
                 "params" => %{
                   "latitude" => 37.7583,
                   "longitude" => -121.9958,
                   "radius_miles" => 10.0,
                   "hours" => 6,
                   "limit" => 50
                 }
               }
             } = json_response(conn, 200)

      assert weather_station["callsign"] == station.sender
      assert weather_station["distance_miles"] < 10.0
      assert weather_station["weather"]["temperature"] == 68.0
    end

    test "returns 400 when lat is missing", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lon" => "-121.9958",
          "radius" => "10"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 400)
      assert message =~ "lat"
    end

    test "returns 400 when lon is missing", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "radius" => "10"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 400)
      assert message =~ "lon"
    end

    test "returns 400 when radius is missing", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 400)
      assert message =~ "radius"
    end

    test "returns 422 when lat is out of range", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "91",
          "lon" => "-121.9958",
          "radius" => "10"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 422)
      assert message =~ "latitude"
      assert message =~ "-90"
      assert message =~ "90"
    end

    test "returns 422 when lon is out of range", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-181",
          "radius" => "10"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 422)
      assert message =~ "longitude"
      assert message =~ "-180"
      assert message =~ "180"
    end

    test "returns 422 when radius is zero", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "0"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 422)
      assert message =~ "radius"
      assert message =~ "greater than 0"
    end

    test "returns 422 when radius is negative", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "-1"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 422)
      assert message =~ "radius"
      assert message =~ "greater than 0"
    end

    test "returns 422 when radius exceeds maximum", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "1001"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 422)
      assert message =~ "radius"
      assert message =~ "1000"
    end

    test "accepts optional hours parameter", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "10",
          "hours" => "24"
        })

      assert %{"meta" => %{"params" => %{"hours" => 24}}} = json_response(conn, 200)
    end

    test "returns 422 when hours is out of range", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "10",
          "hours" => "169"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 422)
      assert message =~ "hours"
      assert message =~ "1"
      assert message =~ "168"
    end

    test "accepts optional limit parameter", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "10",
          "limit" => "25"
        })

      assert %{"meta" => %{"params" => %{"limit" => 25}}} = json_response(conn, 200)
    end

    test "returns 422 when limit is out of range", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "10",
          "limit" => "101"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 422)
      assert message =~ "limit"
      assert message =~ "1"
      assert message =~ "100"
    end

    test "uses default values when optional params not provided", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "10"
        })

      assert %{"meta" => %{"params" => params}} = json_response(conn, 200)
      assert params["hours"] == 6
      assert params["limit"] == 50
    end

    test "returns empty list when no weather stations in range", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "0.0",
          "lon" => "0.0",
          "radius" => "1"
        })

      assert %{
               "data" => [],
               "meta" => %{"count" => 0}
             } = json_response(conn, 200)
    end

    test "coerces string parameters to correct types", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "37.7583",
          "lon" => "-121.9958",
          "radius" => "10",
          "hours" => "12",
          "limit" => "25"
        })

      assert %{"meta" => %{"params" => params}} = json_response(conn, 200)
      assert is_float(params["latitude"])
      assert is_float(params["longitude"])
      assert is_float(params["radius_miles"])
      assert is_integer(params["hours"])
      assert is_integer(params["limit"])
    end

    test "handles invalid numeric strings", %{conn: conn} do
      conn =
        get(conn, ~p"/api/v1/weather/nearby", %{
          "lat" => "not_a_number",
          "lon" => "-121.9958",
          "radius" => "10"
        })

      assert %{"error" => %{"message" => message}} = json_response(conn, 400)
      assert message =~ "lat"
    end
  end
end
