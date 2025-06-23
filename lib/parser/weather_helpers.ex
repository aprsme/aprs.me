defmodule Parser.WeatherHelpers do
  @moduledoc """
  Weather field extraction helpers for APRS.
  """

  @spec extract_timestamp(String.t()) :: String.t() | nil
  def extract_timestamp(weather_data) do
    case Regex.run(~r/^(\d{6}[hz\/])/, weather_data) do
      [_, timestamp] -> timestamp
      nil -> nil
    end
  end

  @spec remove_timestamp(String.t()) :: String.t()
  def remove_timestamp(weather_data) do
    case Regex.run(~r/^\d{6}[hz\/]/, weather_data) do
      [timestamp] -> String.replace(weather_data, timestamp, "")
      nil -> weather_data
    end
  end

  @spec parse_wind_direction(String.t()) :: integer() | nil
  def parse_wind_direction(weather_data) do
    case Regex.run(~r/(\d{3})\//, weather_data) do
      [_, direction] -> String.to_integer(direction)
      nil -> nil
    end
  end

  @spec parse_wind_speed(String.t()) :: integer() | nil
  def parse_wind_speed(weather_data) do
    case Regex.run(~r/\/(\d{3})/, weather_data) do
      [_, speed] -> String.to_integer(speed)
      nil -> nil
    end
  end

  @spec parse_wind_gust(String.t()) :: integer() | nil
  def parse_wind_gust(weather_data) do
    case Regex.run(~r/g(\d{3})/, weather_data) do
      [_, gust] -> String.to_integer(gust)
      nil -> nil
    end
  end

  @spec parse_temperature(String.t()) :: integer() | nil
  def parse_temperature(weather_data) do
    case Regex.run(~r/t(-?\d{3})/, weather_data) do
      [_, temp] -> String.to_integer(temp)
      nil -> nil
    end
  end

  @spec parse_rainfall_1h(String.t()) :: integer() | nil
  def parse_rainfall_1h(weather_data) do
    case Regex.run(~r/r(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  @spec parse_rainfall_24h(String.t()) :: integer() | nil
  def parse_rainfall_24h(weather_data) do
    case Regex.run(~r/p(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  @spec parse_rainfall_since_midnight(String.t()) :: integer() | nil
  def parse_rainfall_since_midnight(weather_data) do
    case Regex.run(~r/P(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  @spec parse_humidity(String.t()) :: integer() | nil
  def parse_humidity(weather_data) do
    case Regex.run(~r/h(\d{2})/, weather_data) do
      [_, humidity] ->
        val = String.to_integer(humidity)
        normalize_humidity(val)

      nil ->
        nil
    end
  end

  defp normalize_humidity(0), do: 100
  defp normalize_humidity(val), do: val

  @spec parse_pressure(String.t()) :: float() | nil
  def parse_pressure(weather_data) do
    case Regex.run(~r/b(\d{5})/, weather_data) do
      [_, pressure] -> String.to_integer(pressure) / 10.0
      nil -> nil
    end
  end

  @spec parse_luminosity(String.t()) :: integer() | nil
  def parse_luminosity(weather_data) do
    case Regex.run(~r/[lL](\d{3})/, weather_data) do
      [_, luminosity] -> String.to_integer(luminosity)
      nil -> nil
    end
  end

  @spec parse_snow(String.t()) :: integer() | nil
  def parse_snow(weather_data) do
    case Regex.run(~r/s(\d{3})/, weather_data) do
      [_, snow] -> String.to_integer(snow)
      nil -> nil
    end
  end
end
