defmodule Parser.Weather do
  @moduledoc """
  APRS weather report parsing.
  """

  @doc """
  Parse an APRS weather report string. Returns a struct or error.
  """
  @spec parse(String.t()) :: map() | nil
  def parse("_" <> <<timestamp::binary-size(8), rest::binary>>) do
    weather_data = parse_weather_data(rest)
    Map.merge(%{timestamp: timestamp, data_type: :weather}, weather_data)
  end

  def parse(data) do
    weather_data = parse_weather_data(data)
    Map.merge(%{data_type: :weather}, weather_data)
  end

  @doc """
  Parses a weather data string into a map of weather values.
  """
  @spec parse_weather_data(String.t()) :: map()
  def parse_weather_data(weather_data) do
    timestamp = Parser.WeatherHelpers.extract_timestamp(weather_data)
    weather_data = Parser.WeatherHelpers.remove_timestamp(weather_data)

    weather_values = %{
      wind_direction: Parser.WeatherHelpers.parse_wind_direction(weather_data),
      wind_speed: Parser.WeatherHelpers.parse_wind_speed(weather_data),
      wind_gust: Parser.WeatherHelpers.parse_wind_gust(weather_data),
      temperature: Parser.WeatherHelpers.parse_temperature(weather_data),
      rain_1h: Parser.WeatherHelpers.parse_rainfall_1h(weather_data),
      rain_24h: Parser.WeatherHelpers.parse_rainfall_24h(weather_data),
      rain_since_midnight: Parser.WeatherHelpers.parse_rainfall_since_midnight(weather_data),
      humidity: Parser.WeatherHelpers.parse_humidity(weather_data),
      pressure: Parser.WeatherHelpers.parse_pressure(weather_data),
      luminosity: Parser.WeatherHelpers.parse_luminosity(weather_data),
      snow: Parser.WeatherHelpers.parse_snow(weather_data)
    }

    result = %{timestamp: timestamp, data_type: :weather, raw_weather_data: weather_data}

    Enum.reduce(weather_values, result, fn {key, value}, acc ->
      put_weather_value(acc, key, value)
    end)
  end

  defp put_weather_value(acc, _key, nil), do: acc
  defp put_weather_value(acc, key, value), do: Map.put(acc, key, value)
end
