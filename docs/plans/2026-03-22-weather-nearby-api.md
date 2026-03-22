# Weather Nearby API Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add GET /api/v1/weather/nearby endpoint to find weather stations within a radius of a lat/lon point

**Architecture:** Phoenix controller → PreparedQueries context function → PostGIS ST_DWithin query → JSON view. Uses existing spatial indexes and follows established API patterns.

**Tech Stack:** Phoenix, Ecto, PostGIS, ExUnit

---

## Task 1: Add Database Query Function

**Files:**
- Modify: `lib/aprsme/packets/prepared_queries.ex:211` (add new function at end)
- Test: `test/aprsme/packets/prepared_queries_test.exs` (new file)

**Step 1: Write the failing test**

Create `test/aprsme/packets/prepared_queries_test.exs`:

```elixir
defmodule Aprsme.Packets.PreparedQueriesTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.Packets.PreparedQueries
  alias Aprsme.Repo

  describe "get_nearby_weather_stations/4" do
    setup do
      # San Francisco center point
      center_lat = 37.7749
      center_lon = -122.4194

      # Station A: ~0.5 miles north, weather 1 hour ago ✓
      {:ok, station_a} =
        insert_weather_station("STATION-A", 37.7821, -122.4194, hours_ago: 1)

      # Station B: ~10 miles east, weather 2 hours ago ✓
      {:ok, station_b} =
        insert_weather_station("STATION-B", 37.7749, -122.2694, hours_ago: 2)

      # Station C: ~50 miles south, weather 1 hour ago ✗ (outside 25 mile radius)
      {:ok, _station_c} =
        insert_weather_station("STATION-C", 37.0749, -122.4194, hours_ago: 1)

      # Station D: ~5 miles south, weather 8 hours ago ✗ (outside 6 hour window)
      {:ok, _station_d} =
        insert_weather_station("STATION-D", 37.7200, -122.4194, hours_ago: 8)

      # Station E: ~5 miles north, no weather data ✗
      {:ok, _station_e} =
        insert_packet("STATION-E", 37.8300, -122.4194, has_weather: false, hours_ago: 1)

      %{
        center_lat: center_lat,
        center_lon: center_lon,
        station_a: station_a,
        station_b: station_b
      }
    end

    test "returns weather stations within radius", %{
      center_lat: lat,
      center_lon: lon,
      station_a: station_a,
      station_b: station_b
    } do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{})

      callsigns = Enum.map(results, & &1.callsign)
      assert station_a.sender in callsigns
      assert station_b.sender in callsigns
      assert length(results) == 2
    end

    test "excludes stations outside radius", %{center_lat: lat, center_lon: lon} do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{})

      callsigns = Enum.map(results, & &1.callsign)
      refute "STATION-C" in callsigns
    end

    test "excludes stations outside time window", %{center_lat: lat, center_lon: lon} do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{hours: 6})

      callsigns = Enum.map(results, & &1.callsign)
      refute "STATION-D" in callsigns
    end

    test "excludes stations without weather data", %{center_lat: lat, center_lon: lon} do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{})

      callsigns = Enum.map(results, & &1.callsign)
      refute "STATION-E" in callsigns
    end

    test "orders results by distance (closest first)", %{
      center_lat: lat,
      center_lon: lon,
      station_a: station_a,
      station_b: station_b
    } do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{})

      assert length(results) == 2
      # Station A is ~0.5 miles, Station B is ~10 miles
      assert List.first(results).callsign == station_a.sender
      assert List.last(results).callsign == station_b.sender
      assert List.first(results).distance_miles < List.last(results).distance_miles
    end

    test "respects limit option", %{center_lat: lat, center_lon: lon} do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{limit: 1})

      assert length(results) == 1
    end

    test "uses default hours of 6", %{center_lat: lat, center_lon: lon} do
      # Station D is 8 hours old, should be excluded with default
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{})

      callsigns = Enum.map(results, & &1.callsign)
      refute "STATION-D" in callsigns
    end

    test "returns distance in miles", %{center_lat: lat, center_lon: lon, station_a: station_a} do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{})

      station_a_result = Enum.find(results, &(&1.callsign == station_a.sender))
      assert station_a_result.distance_miles > 0
      assert station_a_result.distance_miles < 1.0
    end

    test "includes weather data in results", %{center_lat: lat, center_lon: lon} do
      results = PreparedQueries.get_nearby_weather_stations(lat, lon, 25.0, %{})

      assert length(results) > 0
      first = List.first(results)
      assert is_float(first.temperature)
      assert is_float(first.humidity)
      assert is_float(first.wind_speed)
    end

    test "returns empty list when no stations in radius", _context do
      # Middle of Pacific Ocean
      results = PreparedQueries.get_nearby_weather_stations(0.0, -180.0, 10.0, %{})

      assert results == []
    end
  end

  defp insert_weather_station(callsign, lat, lon, opts) do
    hours_ago = Keyword.get(opts, :hours_ago, 1)

    attrs = %{
      sender: callsign,
      base_callsign: callsign,
      destination: "APRS",
      path: "WIDE1-1",
      received_at: DateTime.add(DateTime.utc_now(), -hours_ago * 3600, :second),
      has_position: true,
      has_weather: true,
      lat: Decimal.from_float(lat),
      lon: Decimal.from_float(lon),
      location: %Geo.Point{coordinates: {lon, lat}, srid: 4326},
      temperature: 72.5,
      humidity: 65.0,
      pressure: 1013.2,
      wind_speed: 8.5,
      wind_direction: 270,
      wind_gust: 12.0,
      symbol_table_id: "/",
      symbol_code: "_",
      comment: "Test weather station"
    }

    %Aprsme.Packet{}
    |> Aprsme.Packet.changeset(attrs)
    |> Repo.insert()
  end

  defp insert_packet(callsign, lat, lon, opts) do
    hours_ago = Keyword.get(opts, :hours_ago, 1)
    has_weather = Keyword.get(opts, :has_weather, false)

    attrs = %{
      sender: callsign,
      base_callsign: callsign,
      destination: "APRS",
      path: "WIDE1-1",
      received_at: DateTime.add(DateTime.utc_now(), -hours_ago * 3600, :second),
      has_position: true,
      has_weather: has_weather,
      lat: Decimal.from_float(lat),
      lon: Decimal.from_float(lon),
      location: %Geo.Point{coordinates: {lon, lat}, srid: 4326},
      symbol_table_id: "/",
      symbol_code: "/"
    }

    %Aprsme.Packet{}
    |> Aprsme.Packet.changeset(attrs)
    |> Repo.insert()
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/aprsme/packets/prepared_queries_test.exs`

Expected: FAIL with "function PreparedQueries.get_nearby_weather_stations/4 is undefined"

**Step 3: Write minimal implementation**

Add to `lib/aprsme/packets/prepared_queries.ex` at line 211:

```elixir
  @doc """
  Get nearby weather stations within a radius (in miles) from a lat/lon point.
  Returns stations with recent weather data, ordered by distance.

  ## Options
    * `:hours` - Time window in hours (default: 6)
    * `:limit` - Maximum results (default: 50)

  ## Returns
  List of maps with station info, weather data, and distance in miles.
  """
  @spec get_nearby_weather_stations(float(), float(), float(), map()) :: [map()]
  def get_nearby_weather_stations(lat, lon, radius_miles, opts \\ %{}) do
    hours = Map.get(opts, :hours, 6)
    limit = Map.get(opts, :limit, 50)

    # Convert miles to meters for PostGIS (1 mile = 1609.34 meters)
    radius_meters = radius_miles * 1609.34

    cutoff_time = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)

    # Build point for distance calculation
    point = %Geo.Point{coordinates: {lon, lat}, srid: 4326}

    # Subquery to get most recent weather packet per base_callsign
    recent_weather =
      from(p in Packet,
        where: p.has_weather == true,
        where: p.has_position == true,
        where: p.received_at >= ^cutoff_time,
        where: not is_nil(p.location),
        where:
          fragment(
            "ST_DWithin(?::geography, ?::geography, ?)",
            p.location,
            ^point,
            ^radius_meters
          ),
        distinct: p.base_callsign,
        order_by: [asc: p.base_callsign, desc: p.received_at]
      )

    # Order by distance and limit results
    from(p in subquery(recent_weather),
      order_by: fragment("? <-> ?", p.location, ^point),
      limit: ^limit,
      select: %{
        callsign: p.sender,
        base_callsign: p.base_callsign,
        lat: fragment("ST_Y(?)", p.location),
        lon: fragment("ST_X(?)", p.location),
        distance_miles: fragment("ST_Distance(?::geography, ?::geography) / 1609.34", p.location, ^point),
        temperature: p.temperature,
        humidity: p.humidity,
        pressure: p.pressure,
        wind_speed: p.wind_speed,
        wind_direction: p.wind_direction,
        wind_gust: p.wind_gust,
        rain_1h: p.rain_1h,
        rain_24h: p.rain_24h,
        rain_since_midnight: p.rain_since_midnight,
        symbol_table_id: p.symbol_table_id,
        symbol_code: p.symbol_code,
        comment: p.comment,
        received_at: p.received_at
      }
    )
    |> Repo.all()
  end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/aprsme/packets/prepared_queries_test.exs`

Expected: All tests PASS

**Step 5: Run mix format**

Run: `mix format`

**Step 6: Commit**

```bash
git add lib/aprsme/packets/prepared_queries.ex test/aprsme/packets/prepared_queries_test.exs
git commit -m "feat: add get_nearby_weather_stations query function

Adds PreparedQueries.get_nearby_weather_stations/4 to find weather
stations within a radius using PostGIS ST_DWithin. Supports configurable
time window and result limit. Returns stations ordered by distance with
weather data included."
```

---

## Task 2: Add JSON View Module

**Files:**
- Create: `lib/aprsme_web/controllers/api/v1/weather_json.ex`
- Test: `test/aprsme_web/controllers/api/v1/weather_json_test.exs`

**Step 1: Write the failing test**

Create `test/aprsme_web/controllers/api/v1/weather_json_test.exs`:

```elixir
defmodule AprsmeWeb.Api.V1.WeatherJSONTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.Api.V1.WeatherJSON

  describe "nearby/1" do
    test "renders list of weather stations with all fields" do
      stations = [
        %{
          callsign: "N0CALL-13",
          base_callsign: "N0CALL",
          lat: 37.7849,
          lon: -122.4094,
          distance_miles: 0.87,
          temperature: 72.5,
          humidity: 65.0,
          pressure: 1013.2,
          wind_speed: 8.5,
          wind_direction: 270,
          wind_gust: 12.0,
          rain_1h: 0.0,
          rain_24h: 0.5,
          rain_since_midnight: 0.3,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "Davis Vantage Pro2",
          received_at: ~U[2026-03-22 15:30:00Z]
        }
      ]

      params = %{lat: 37.7749, lon: -122.4194, radius_miles: 25, hours: 6, limit: 50}

      result = WeatherJSON.nearby(%{stations: stations, params: params})

      assert %{data: [station], meta: meta} = result
      assert station.callsign == "N0CALL-13"
      assert station.base_callsign == "N0CALL"
      assert station.position.lat == 37.7849
      assert station.position.lon == -122.4094
      assert station.distance_miles == 0.87
      assert station.weather.temperature == 72.5
      assert station.weather.humidity == 65.0
      assert station.weather.pressure == 1013.2
      assert station.weather.wind_speed == 8.5
      assert station.weather.wind_direction == 270
      assert station.weather.wind_gust == 12.0
      assert station.weather.rain_1h == 0.0
      assert station.weather.rain_24h == 0.5
      assert station.weather.rain_since_midnight == 0.3
      assert station.symbol.table_id == "/"
      assert station.symbol.code == "_"
      assert station.comment == "Davis Vantage Pro2"
      assert station.last_report == "2026-03-22T15:30:00Z"

      assert meta.count == 1
      assert meta.params.lat == 37.7749
      assert meta.params.lon == -122.4194
      assert meta.params.radius_miles == 25
      assert meta.params.hours == 6
      assert meta.params.limit == 50
    end

    test "handles null weather fields" do
      stations = [
        %{
          callsign: "TEST-1",
          base_callsign: "TEST",
          lat: 37.7849,
          lon: -122.4094,
          distance_miles: 1.5,
          temperature: 70.0,
          humidity: nil,
          pressure: nil,
          wind_speed: nil,
          wind_direction: nil,
          wind_gust: nil,
          rain_1h: nil,
          rain_24h: nil,
          rain_since_midnight: nil,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: nil,
          received_at: ~U[2026-03-22 15:30:00Z]
        }
      ]

      params = %{lat: 37.7749, lon: -122.4194, radius_miles: 25, hours: 6, limit: 50}

      result = WeatherJSON.nearby(%{stations: stations, params: params})

      assert %{data: [station]} = result
      assert station.weather.temperature == 70.0
      assert station.weather.humidity == nil
      assert station.weather.pressure == nil
      assert station.comment == nil
    end

    test "renders empty list when no stations found" do
      params = %{lat: 0.0, lon: -180.0, radius_miles: 10, hours: 6, limit: 50}

      result = WeatherJSON.nearby(%{stations: [], params: params})

      assert %{data: [], meta: meta} = result
      assert meta.count == 0
    end
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/aprsme_web/controllers/api/v1/weather_json_test.exs`

Expected: FAIL with "module AprsmeWeb.Api.V1.WeatherJSON is not available"

**Step 3: Write minimal implementation**

Create `lib/aprsme_web/controllers/api/v1/weather_json.ex`:

```elixir
defmodule AprsmeWeb.Api.V1.WeatherJSON do
  @moduledoc """
  JSON rendering for weather API endpoints.
  """

  @doc """
  Renders a list of nearby weather stations.
  """
  def nearby(%{stations: stations, params: params}) do
    %{
      data: Enum.map(stations, &station_json/1),
      meta: %{
        count: length(stations),
        params: params
      }
    }
  end

  defp station_json(station) do
    %{
      callsign: station.callsign,
      base_callsign: station.base_callsign,
      position: %{
        lat: station.lat,
        lon: station.lon
      },
      distance_miles: station.distance_miles,
      weather: %{
        temperature: station.temperature,
        humidity: station.humidity,
        pressure: station.pressure,
        wind_speed: station.wind_speed,
        wind_direction: station.wind_direction,
        wind_gust: station.wind_gust,
        rain_1h: station.rain_1h,
        rain_24h: station.rain_24h,
        rain_since_midnight: station.rain_since_midnight
      },
      symbol: %{
        table_id: station.symbol_table_id,
        code: station.symbol_code
      },
      comment: station.comment,
      last_report: format_datetime(station.received_at)
    }
  end

  defp format_datetime(%DateTime{} = dt) do
    DateTime.to_iso8601(dt)
  end

  defp format_datetime(nil), do: nil
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/aprsme_web/controllers/api/v1/weather_json_test.exs`

Expected: All tests PASS

**Step 5: Run mix format**

Run: `mix format`

**Step 6: Commit**

```bash
git add lib/aprsme_web/controllers/api/v1/weather_json.ex test/aprsme_web/controllers/api/v1/weather_json_test.exs
git commit -m "feat: add WeatherJSON view for API responses

Adds JSON rendering for nearby weather stations endpoint. Formats
station data with position, weather metrics, symbols, and metadata."
```

---

## Task 3: Add Weather Controller

**Files:**
- Create: `lib/aprsme_web/controllers/api/v1/weather_controller.ex`
- Test: `test/aprsme_web/controllers/api/v1/weather_controller_test.exs`

**Step 1: Write the failing test**

Create `test/aprsme_web/controllers/api/v1/weather_controller_test.exs`:

```elixir
defmodule AprsmeWeb.Api.V1.WeatherControllerTest do
  use AprsmeWeb.ConnCase, async: false

  alias Aprsme.Repo

  describe "GET /api/v1/weather/nearby" do
    setup do
      # Insert test weather stations
      {:ok, station_a} =
        insert_weather_station("TESTA-1", 37.7821, -122.4194, hours_ago: 1)

      {:ok, station_b} =
        insert_weather_station("TESTB-1", 37.7749, -122.2694, hours_ago: 2)

      %{station_a: station_a, station_b: station_b}
    end

    test "returns 200 with weather stations when valid params provided", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25")

      assert %{"data" => data, "meta" => meta} = json_response(conn, 200)
      assert is_list(data)
      assert length(data) >= 2
      assert meta["count"] >= 2
      assert meta["params"]["lat"] == 37.7749
      assert meta["params"]["lon"] == -122.4194
      assert meta["params"]["radius_miles"] == 25
    end

    test "returns weather station data with correct structure", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25")

      assert %{"data" => [station | _]} = json_response(conn, 200)
      assert is_binary(station["callsign"])
      assert is_binary(station["base_callsign"])
      assert is_map(station["position"])
      assert is_float(station["position"]["lat"])
      assert is_float(station["position"]["lon"])
      assert is_float(station["distance_miles"])
      assert is_map(station["weather"])
      assert is_map(station["symbol"])
      assert is_binary(station["last_report"])
    end

    test "returns 400 when lat is missing", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lon=-122.4194&radius=25")

      assert %{"error" => error} = json_response(conn, 400)
      assert error =~ "lat"
    end

    test "returns 400 when lon is missing", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&radius=25")

      assert %{"error" => error} = json_response(conn, 400)
      assert error =~ "lon"
    end

    test "returns 400 when radius is missing", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194")

      assert %{"error" => error} = json_response(conn, 400)
      assert error =~ "radius"
    end

    test "returns 422 when lat is out of range", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=91&lon=-122.4194&radius=25")

      assert %{"error" => error} = json_response(conn, 422)
      assert error =~ "latitude"
    end

    test "returns 422 when lon is out of range", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=181&radius=25")

      assert %{"error" => error} = json_response(conn, 422)
      assert error =~ "longitude"
    end

    test "returns 422 when radius is zero or negative", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=0")

      assert %{"error" => error} = json_response(conn, 422)
      assert error =~ "radius"
    end

    test "returns 422 when radius exceeds maximum", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=1001")

      assert %{"error" => error} = json_response(conn, 422)
      assert error =~ "radius"
    end

    test "returns 422 when hours is out of range", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25&hours=0")

      assert %{"error" => error} = json_response(conn, 422)
      assert error =~ "hours"
    end

    test "returns 422 when limit is out of range", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25&limit=0")

      assert %{"error" => error} = json_response(conn, 422)
      assert error =~ "limit"
    end

    test "accepts optional hours parameter", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25&hours=12")

      assert %{"meta" => meta} = json_response(conn, 200)
      assert meta["params"]["hours"] == 12
    end

    test "accepts optional limit parameter", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25&limit=10")

      assert %{"meta" => meta} = json_response(conn, 200)
      assert meta["params"]["limit"] == 10
    end

    test "uses default hours when not provided", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25")

      assert %{"meta" => meta} = json_response(conn, 200)
      assert meta["params"]["hours"] == 6
    end

    test "uses default limit when not provided", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25")

      assert %{"meta" => meta} = json_response(conn, 200)
      assert meta["params"]["limit"] == 50
    end

    test "returns empty array when no stations in radius", %{conn: conn} do
      # Middle of Pacific Ocean
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=0.0&lon=-180.0&radius=10")

      assert %{"data" => data, "meta" => meta} = json_response(conn, 200)
      assert data == []
      assert meta["count"] == 0
    end

    test "coerces string params to numbers", %{conn: conn} do
      conn = get(conn, ~p"/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25")

      assert json_response(conn, 200)
    end
  end

  defp insert_weather_station(callsign, lat, lon, opts) do
    hours_ago = Keyword.get(opts, :hours_ago, 1)

    attrs = %{
      sender: callsign,
      base_callsign: String.split(callsign, "-") |> List.first(),
      destination: "APRS",
      path: "WIDE1-1",
      received_at: DateTime.add(DateTime.utc_now(), -hours_ago * 3600, :second),
      has_position: true,
      has_weather: true,
      lat: Decimal.from_float(lat),
      lon: Decimal.from_float(lon),
      location: %Geo.Point{coordinates: {lon, lat}, srid: 4326},
      temperature: 72.5,
      humidity: 65.0,
      pressure: 1013.2,
      wind_speed: 8.5,
      wind_direction: 270,
      wind_gust: 12.0,
      rain_1h: 0.0,
      rain_24h: 0.5,
      rain_since_midnight: 0.3,
      symbol_table_id: "/",
      symbol_code: "_",
      comment: "Test weather station"
    }

    %Aprsme.Packet{}
    |> Aprsme.Packet.changeset(attrs)
    |> Repo.insert()
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/aprsme_web/controllers/api/v1/weather_controller_test.exs`

Expected: FAIL with route not found or controller not available

**Step 3: Write minimal implementation**

Create `lib/aprsme_web/controllers/api/v1/weather_controller.ex`:

```elixir
defmodule AprsmeWeb.Api.V1.WeatherController do
  @moduledoc """
  API controller for weather-related endpoints.
  """
  use AprsmeWeb, :controller

  alias Aprsme.Packets.PreparedQueries
  alias AprsmeWeb.Api.V1.WeatherJSON

  action_fallback AprsmeWeb.Api.V1.FallbackController

  @doc """
  Get weather stations within a radius of a lat/lon point.

  ## Parameters
    * `lat` - Latitude (-90 to 90)
    * `lon` - Longitude (-180 to 180)
    * `radius` - Search radius in miles (> 0, max 1000)
    * `hours` - Time window in hours (optional, default: 6, max: 168)
    * `limit` - Maximum results (optional, default: 50, max: 100)

  ## Returns
    * 200 - Success with list of weather stations
    * 400 - Missing required parameters
    * 422 - Invalid parameter values
  """
  def nearby(conn, params) do
    with {:ok, validated_params} <- validate_params(params) do
      stations =
        PreparedQueries.get_nearby_weather_stations(
          validated_params.lat,
          validated_params.lon,
          validated_params.radius,
          %{
            hours: validated_params.hours,
            limit: validated_params.limit
          }
        )

      conn
      |> put_status(:ok)
      |> put_view(json: WeatherJSON)
      |> render(:nearby, %{
        stations: stations,
        params: validated_params
      })
    end
  end

  defp validate_params(params) do
    with {:ok, lat} <- parse_required_float(params, "lat"),
         {:ok, lon} <- parse_required_float(params, "lon"),
         {:ok, radius} <- parse_required_float(params, "radius"),
         {:ok, hours} <- parse_optional_int(params, "hours", 6),
         {:ok, limit} <- parse_optional_int(params, "limit", 50),
         :ok <- validate_lat(lat),
         :ok <- validate_lon(lon),
         :ok <- validate_radius(radius),
         :ok <- validate_hours(hours),
         :ok <- validate_limit(limit) do
      {:ok,
       %{
         lat: lat,
         lon: lon,
         radius: radius,
         radius_miles: radius,
         hours: hours,
         limit: limit
       }}
    end
  end

  defp parse_required_float(params, key) do
    case Map.get(params, key) do
      nil ->
        {:error, "Missing required parameter: #{key}"}

      value when is_binary(value) ->
        case Float.parse(value) do
          {float, _} -> {:ok, float}
          :error -> {:error, "Invalid #{key}: must be a number"}
        end

      value when is_number(value) ->
        {:ok, value * 1.0}
    end
  end

  defp parse_optional_int(params, key, default) do
    case Map.get(params, key) do
      nil ->
        {:ok, default}

      value when is_binary(value) ->
        case Integer.parse(value) do
          {int, _} -> {:ok, int}
          :error -> {:error, "Invalid #{key}: must be an integer"}
        end

      value when is_integer(value) ->
        {:ok, value}

      value when is_float(value) ->
        {:ok, trunc(value)}
    end
  end

  defp validate_lat(lat) when lat >= -90 and lat <= 90, do: :ok

  defp validate_lat(_),
    do: {:error, "Invalid latitude: must be a number between -90 and 90"}

  defp validate_lon(lon) when lon >= -180 and lon <= 180, do: :ok

  defp validate_lon(_),
    do: {:error, "Invalid longitude: must be a number between -180 and 180"}

  defp validate_radius(radius) when radius > 0 and radius <= 1000, do: :ok

  defp validate_radius(_),
    do: {:error, "Radius must be between 0 and 1000 miles"}

  defp validate_hours(hours) when hours >= 1 and hours <= 168, do: :ok

  defp validate_hours(_),
    do: {:error, "Hours must be between 1 and 168 (7 days)"}

  defp validate_limit(limit) when limit >= 1 and limit <= 100, do: :ok

  defp validate_limit(_),
    do: {:error, "Limit must be between 1 and 100"}
end
```

**Step 4: Run test to verify it still fails (no route)**

Run: `mix test test/aprsme_web/controllers/api/v1/weather_controller_test.exs`

Expected: FAIL with route not found

**Step 5: Add route to router**

Modify `lib/aprsme_web/router.ex` at line 118 (in the `/api/v1` scope):

```elixir
  # API v1 routes
  scope "/api/v1", AprsmeWeb.Api.V1, as: :api_v1 do
    pipe_through :api

    get "/callsign/:callsign", CallsignController, :show
    get "/weather/nearby", WeatherController, :nearby
  end
```

**Step 6: Run test to verify it passes**

Run: `mix test test/aprsme_web/controllers/api/v1/weather_controller_test.exs`

Expected: All tests PASS

**Step 7: Run mix format**

Run: `mix format`

**Step 8: Commit**

```bash
git add lib/aprsme_web/controllers/api/v1/weather_controller.ex test/aprsme_web/controllers/api/v1/weather_controller_test.exs lib/aprsme_web/router.ex
git commit -m "feat: add weather nearby API endpoint

Adds GET /api/v1/weather/nearby endpoint to find weather stations
within a radius. Validates parameters (lat, lon, radius, hours, limit)
and returns formatted JSON response with station data and weather
metrics."
```

---

## Task 4: Run Full Test Suite

**Step 1: Run all tests**

Run: `mix test`

Expected: All tests PASS

**Step 2: If any tests fail, fix them**

If tests fail, investigate and fix issues. Commit fixes separately.

**Step 3: Final commit if any fixes were needed**

```bash
git add <files>
git commit -m "fix: resolve test failures"
```

---

## Task 5: Manual Verification

**Step 1: Start the development server**

Run: `mix phx.server`

**Step 2: Test the endpoint with curl**

```bash
# Valid request
curl "http://localhost:4000/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25"

# With optional parameters
curl "http://localhost:4000/api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25&hours=12&limit=10"

# Invalid parameters
curl "http://localhost:4000/api/v1/weather/nearby?lat=91&lon=-122.4194&radius=25"
```

**Step 3: Verify responses match expected format**

Check that:
- Valid requests return 200 with data and meta
- Invalid parameters return 400/422 with error messages
- Empty results return 200 with empty array

---

## Summary

This plan implements the weather nearby API endpoint following TDD principles:

1. **Database query function** - PostGIS spatial query with ST_DWithin
2. **JSON view** - Response formatting with weather data
3. **Controller** - Parameter validation and request handling
4. **Router** - Endpoint registration
5. **Testing** - Comprehensive unit and integration tests

All code follows existing patterns in the codebase (PreparedQueries, API controllers, JSON views).
