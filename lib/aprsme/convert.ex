defmodule Aprsme.Convert do
  @moduledoc false

  @spec wind(number(), :ultimeter, :mph) :: float()
  def wind(speed, :ultimeter, :mph), do: speed * 0.0621371192

  @spec temp(number(), :ultimeter, :f) :: float()
  def temp(value, :ultimeter, :f), do: value * 0.1

  @spec speed(number(), :knots, :mph) :: float()
  def speed(value, :knots, :mph), do: Float.round(value * 1.15077945, 2)

  # Weather unit conversions
  @doc """
  Converts Fahrenheit to Celsius
  """
  def f_to_c(fahrenheit) when is_number(fahrenheit) do
    (fahrenheit - 32) * 5 / 9
  end

  @doc """
  Converts Celsius to Fahrenheit
  """
  def c_to_f(celsius) when is_number(celsius) do
    celsius * 9 / 5 + 32
  end

  @doc """
  Converts mph to km/h
  """
  def mph_to_kph(mph) when is_number(mph) do
    mph * 1.60934
  end

  @doc """
  Converts km/h to mph
  """
  def kph_to_mph(kph) when is_number(kph) do
    kph / 1.60934
  end

  @doc """
  Converts inches to mm
  """
  def inches_to_mm(inches) when is_number(inches) do
    inches * 25.4
  end

  @doc """
  Converts mm to inches
  """
  def mm_to_inches(mm) when is_number(mm) do
    mm / 25.4
  end
end
