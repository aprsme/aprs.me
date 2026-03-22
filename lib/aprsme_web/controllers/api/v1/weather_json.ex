defmodule AprsmeWeb.Api.V1.WeatherJSON do
  @moduledoc """
  JSON view for weather API responses.
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
        lat: station.latitude,
        lon: station.longitude
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
      last_report: format_datetime(station.last_report)
    }
  end

  defp format_datetime(%DateTime{} = dt) do
    DateTime.to_iso8601(dt)
  end

  defp format_datetime(nil), do: nil
end
