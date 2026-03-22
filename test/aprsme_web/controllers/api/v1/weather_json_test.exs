defmodule AprsmeWeb.Api.V1.WeatherJSONTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.Api.V1.WeatherJSON

  describe "nearby/1" do
    test "renders list of weather stations with all fields" do
      stations = [
        %{
          callsign: "W1XYZ-13",
          base_callsign: "W1XYZ",
          latitude: 42.3601,
          longitude: -71.0589,
          distance_miles: 1.23,
          temperature: 72.5,
          humidity: 65,
          pressure: 1013.25,
          wind_speed: 5.0,
          wind_direction: 180,
          wind_gust: 8.0,
          rain_1h: 0.1,
          rain_24h: 0.5,
          rain_since_midnight: 0.3,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "Test station",
          last_report: ~U[2024-01-15 12:30:00Z]
        }
      ]

      params = %{lat: 42.36, lon: -71.06, radius: 10}

      result = WeatherJSON.nearby(%{stations: stations, params: params})

      assert %{data: [station], meta: meta} = result
      assert station.callsign == "W1XYZ-13"
      assert station.base_callsign == "W1XYZ"
      assert station.position == %{lat: 42.3601, lon: -71.0589}
      assert station.distance_miles == 1.23
      assert station.weather.temperature == 72.5
      assert station.weather.humidity == 65
      assert station.weather.pressure == 1013.25
      assert station.weather.wind_speed == 5.0
      assert station.weather.wind_direction == 180
      assert station.weather.wind_gust == 8.0
      assert station.weather.rain_1h == 0.1
      assert station.weather.rain_24h == 0.5
      assert station.weather.rain_since_midnight == 0.3
      assert station.symbol == %{table_id: "/", code: "_"}
      assert station.comment == "Test station"
      assert station.last_report == "2024-01-15T12:30:00Z"
      assert meta.count == 1
      assert meta.params == params
    end

    test "handles null weather fields gracefully" do
      stations = [
        %{
          callsign: "W2ABC",
          base_callsign: "W2ABC",
          latitude: 40.7128,
          longitude: -74.006,
          distance_miles: 5.0,
          temperature: nil,
          humidity: nil,
          pressure: nil,
          wind_speed: nil,
          wind_direction: nil,
          wind_gust: nil,
          rain_1h: nil,
          rain_24h: nil,
          rain_since_midnight: nil,
          symbol_table_id: "\\",
          symbol_code: "n",
          comment: nil,
          last_report: ~U[2024-01-15 13:00:00Z]
        }
      ]

      params = %{lat: 40.71, lon: -74.01, radius: 10}

      result = WeatherJSON.nearby(%{stations: stations, params: params})

      assert %{data: [station], meta: _meta} = result
      assert station.weather.temperature == nil
      assert station.weather.humidity == nil
      assert station.weather.pressure == nil
      assert station.weather.wind_speed == nil
      assert station.weather.wind_direction == nil
      assert station.weather.wind_gust == nil
      assert station.weather.rain_1h == nil
      assert station.weather.rain_24h == nil
      assert station.weather.rain_since_midnight == nil
      assert station.comment == nil
    end

    test "renders empty list when no stations found" do
      params = %{lat: 42.36, lon: -71.06, radius: 10}

      result = WeatherJSON.nearby(%{stations: [], params: params})

      assert %{data: [], meta: meta} = result
      assert meta.count == 0
      assert meta.params == params
    end
  end
end
