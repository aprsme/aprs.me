defmodule AprsmeWeb.WeatherUnits do
  @moduledoc """
  Handles weather unit conversions and formatting based on locale.
  Supports imperial (US) and metric (international) units.
  """

  alias Aprsme.Convert

  @doc """
  Determines the unit system based on locale.
  Returns :imperial for US, :metric for most other countries.
  """
  @spec unit_system(String.t() | nil | any()) :: :imperial | :metric
  def unit_system(locale) when is_binary(locale) do
    case locale do
      # Default to imperial for English
      "en" -> :imperial
      # Spanish-speaking countries mostly use metric
      "es" -> :metric
      # Germany uses metric
      "de" -> :metric
      # France uses metric
      "fr" -> :metric
      # Default to metric for unknown locales
      _ -> :metric
    end
  end

  def unit_system(nil), do: :imperial
  def unit_system(_), do: :imperial

  @doc """
  Converts temperature from Fahrenheit to Celsius if needed.
  Returns the temperature in the appropriate unit for the locale.
  """
  @spec format_temperature(number() | any(), String.t()) :: {number() | any(), String.t()}
  def format_temperature(temp, locale) when is_number(temp) do
    case unit_system(locale) do
      :imperial -> {temp, "°F"}
      :metric -> {Convert.f_to_c(temp), "°C"}
    end
  end

  def format_temperature(temp, _locale), do: {temp, "°F"}

  @doc """
  Converts wind speed from mph to km/h if needed.
  Returns the wind speed in the appropriate unit for the locale.
  """
  @spec format_wind_speed(number() | any(), String.t()) :: {number() | any(), String.t()}
  def format_wind_speed(speed, locale) when is_number(speed) do
    case unit_system(locale) do
      :imperial -> {speed, "mph"}
      :metric -> {Convert.mph_to_kph(speed), "km/h"}
    end
  end

  def format_wind_speed(speed, _locale), do: {speed, "mph"}

  @doc """
  Converts rain from inches to mm if needed.
  Returns the rain amount in the appropriate unit for the locale.
  """
  @spec format_rain(number() | any(), String.t()) :: {number() | any(), String.t()}
  def format_rain(rain, locale) when is_number(rain) do
    case unit_system(locale) do
      :imperial -> {rain, "in"}
      :metric -> {Convert.inches_to_mm(rain), "mm"}
    end
  end

  def format_rain(rain, _locale), do: {rain, "in"}

  @doc """
  Formats pressure (keeps hPa for all locales as it's standard).
  """
  @spec format_pressure(number() | any(), any()) :: {number() | any(), String.t()}
  def format_pressure(pressure, _locale) when is_number(pressure) do
    {pressure, "hPa"}
  end

  def format_pressure(pressure, _locale), do: {pressure, "hPa"}

  @doc """
  Gets the appropriate unit labels for the locale.
  """
  @spec unit_labels(String.t()) :: %{
          temperature: String.t(),
          wind_speed: String.t(),
          rain: String.t(),
          pressure: String.t()
        }
  def unit_labels(locale) do
    case unit_system(locale) do
      :imperial ->
        %{
          temperature: "°F",
          wind_speed: "mph",
          rain: "in",
          pressure: "hPa"
        }

      :metric ->
        %{
          temperature: "°C",
          wind_speed: "km/h",
          rain: "mm",
          pressure: "hPa"
        }
    end
  end
end
