defmodule Aprs.Convert do
  @moduledoc false

  @spec wind(number(), :ultimeter, :mph) :: float()
  def wind(speed, :ultimeter, :mph), do: speed * 0.0621371192

  @spec temp(number(), :ultimeter, :f) :: float()
  def temp(value, :ultimeter, :f), do: value * 0.1

  @spec speed(number(), :knots, :mph) :: float()
  def speed(value, :knots, :mph), do: Float.round(value * 1.15077945, 2)
end
