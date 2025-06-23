defmodule Parser.NMEAHelpers do
  @moduledoc """
  NMEA coordinate and sentence parsing helpers for APRS.
  """

  @spec parse_nmea_coordinate(String.t(), String.t()) :: {:ok, float()} | {:error, String.t()}
  def parse_nmea_coordinate(value, direction) when is_binary(value) and is_binary(direction) do
    case Float.parse(value) do
      {coord, _} ->
        coord = coord / 100.0
        coord = apply_nmea_direction(coord, direction)
        handle_coordinate_result(coord)

      :error ->
        {:error, "Invalid coordinate value"}
    end
  end

  @spec parse_nmea_coordinate(any(), any()) :: {:error, String.t()}
  def parse_nmea_coordinate(_, _), do: {:error, "Invalid coordinate format"}

  defp handle_coordinate_result(coord) when is_tuple(coord), do: coord
  defp handle_coordinate_result(coord), do: {:ok, coord}

  defp apply_nmea_direction(coord, direction) do
    case direction do
      "N" -> coord
      "S" -> -coord
      "E" -> coord
      "W" -> -coord
      _ -> {:error, "Invalid coordinate direction"}
    end
  end

  @spec parse_nmea_sentence(any()) :: {:error, String.t()}
  def parse_nmea_sentence(_sentence) do
    {:error, "NMEA parsing not implemented"}
  end
end
