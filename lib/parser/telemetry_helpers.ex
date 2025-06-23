defmodule Parser.TelemetryHelpers do
  @moduledoc """
  Telemetry helpers for APRS.
  """

  @spec parse_telemetry_sequence(String.t()) :: integer() | nil
  def parse_telemetry_sequence(seq) do
    case Integer.parse(seq) do
      {num, _} -> num
      :error -> nil
    end
  end

  def parse_analog_values(values) do
    Enum.map(values, &parse_analog_value/1)
  end

  defp parse_analog_value("") do
    nil
  end

  defp parse_analog_value(value) do
    case Float.parse(value) do
      {float_val, _} -> float_val
      :error -> nil
    end
  end

  def parse_digital_values(values) do
    Enum.flat_map(values, &parse_digital_value/1)
  end

  defp parse_digital_value("1"), do: [true]

  defp parse_digital_value(value) when is_binary(value) do
    value
    |> String.graphemes()
    |> Enum.map(fn
      "1" -> true
      "0" -> false
      _ -> nil
    end)
  end

  defp parse_digital_value(_), do: [nil]

  def parse_coefficient(coeff) do
    case Float.parse(coeff) do
      {float_val, _} ->
        float_val

      :error ->
        case Integer.parse(coeff) do
          {int_val, _} -> int_val
          :error -> coeff
        end
    end
  end
end
