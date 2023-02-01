defmodule Aprs.Convert do
  def wind(speed, :ultimeter, :mph), do: speed * 0.0621371192

  def temp(value, :ultimeter, :f), do: value * 0.1
end
