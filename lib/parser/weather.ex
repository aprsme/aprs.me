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

  defp parse_weather_data(weather_data) do
    timestamp = Parser.Helpers.extract_timestamp(weather_data)
    weather_data = Parser.Helpers.remove_timestamp(weather_data)

    weather_values = %{
      wind_direction: Parser.Helpers.parse_wind_direction(weather_data),
      wind_speed: Parser.Helpers.parse_wind_speed(weather_data),
      wind_gust: Parser.Helpers.parse_wind_gust(weather_data),
      temperature: Parser.Helpers.parse_temperature(weather_data),
      rain_1h: Parser.Helpers.parse_rainfall_1h(weather_data),
      rain_24h: Parser.Helpers.parse_rainfall_24h(weather_data),
      rain_since_midnight: Parser.Helpers.parse_rainfall_since_midnight(weather_data),
      humidity: Parser.Helpers.parse_humidity(weather_data),
      pressure: Parser.Helpers.parse_pressure(weather_data),
      luminosity: Parser.Helpers.parse_luminosity(weather_data),
      snow: Parser.Helpers.parse_snow(weather_data)
    }

    result = %{timestamp: timestamp, data_type: :weather, raw_weather_data: weather_data}

    Enum.reduce(weather_values, result, fn {key, value}, acc ->
      if is_nil(value), do: acc, else: Map.put(acc, key, value)
    end)
  end
end
