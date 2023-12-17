defmodule Aprs.Convert do
  @moduledoc false
  def wind(speed, :ultimeter, :mph), do: speed * 0.0621371192

  def temp(value, :ultimeter, :f), do: value * 0.1

  def speed(value, :knots, :mph), do: Float.round(value * 1.15077945, 2)
end
