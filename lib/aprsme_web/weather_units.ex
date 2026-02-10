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
  def unit_system("en"), do: :imperial
  def unit_system("es"), do: :metric
  def unit_system("de"), do: :metric
  def unit_system("fr"), do: :metric
  def unit_system(locale) when is_binary(locale), do: :metric
  def unit_system(nil), do: :imperial
  def unit_system(_), do: :imperial

  @doc """
  Converts temperature from Fahrenheit to Celsius if needed.
  Returns the temperature in the appropriate unit for the locale.
  """
  @spec format_temperature(number() | any(), String.t()) :: {number() | any(), String.t()}
  def format_temperature(temp, locale) when is_number(temp),
    do: format_by_unit(temp, locale, &Convert.f_to_c/1, "°F", "°C")

  def format_temperature(temp, _locale), do: {temp, "°F"}

  @doc """
  Converts wind speed from mph to km/h if needed.
  Returns the wind speed in the appropriate unit for the locale.
  """
  @spec format_wind_speed(number() | any(), String.t()) :: {number() | any(), String.t()}
  def format_wind_speed(speed, locale) when is_number(speed),
    do: format_by_unit(speed, locale, &Convert.mph_to_kph/1, "mph", "km/h")

  def format_wind_speed(speed, _locale), do: {speed, "mph"}

  @doc """
  Converts rain from inches to mm if needed.
  Returns the rain amount in the appropriate unit for the locale.
  """
  @spec format_rain(number() | any(), String.t()) :: {number() | any(), String.t()}
  def format_rain(rain, locale) when is_number(rain),
    do: format_by_unit(rain, locale, &Convert.inches_to_mm/1, "in", "mm")

  def format_rain(rain, _locale), do: {rain, "in"}

  @doc """
  Formats pressure (keeps hPa for all locales as it's standard).
  """
  @spec format_pressure(number() | any(), any()) :: {number() | any(), String.t()}
  def format_pressure(pressure, _locale) when is_number(pressure), do: {pressure, "hPa"}
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
  def unit_labels(locale), do: do_unit_labels(unit_system(locale))

  defp do_unit_labels(:imperial) do
    %{
      temperature: "°F",
      wind_speed: "mph",
      rain: "in",
      pressure: "hPa"
    }
  end

  defp do_unit_labels(:metric) do
    %{
      temperature: "°C",
      wind_speed: "km/h",
      rain: "mm",
      pressure: "hPa"
    }
  end

  defp format_by_unit(value, locale, converter, imperial_unit, metric_unit) do
    case unit_system(locale) do
      :imperial -> {value, imperial_unit}
      :metric -> {converter.(value), metric_unit}
    end
  end
end
