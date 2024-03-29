defmodule Parser.Types.Position do
  @moduledoc """
  Positition Decoder
  """
  require Logger

  defstruct lat_degrees: 0,
            lat_minutes: 0,
            lat_fractional: 0,
            lat_direction: :unknown,
            lon_direction: :unknown,
            lon_degrees: 0,
            lon_minutes: 0,
            lon_fractional: 0

  def from_aprs(aprs_latitude, aprs_longitude) do
    aprs_latitude = aprs_latitude |> String.replace(" ", "0") |> String.pad_leading(9, "0")
    aprs_longitude = aprs_longitude |> String.replace(" ", "0") |> String.pad_leading(9, "0")

    <<latitude::binary-size(8), lat_direction::binary>> = aprs_latitude
    <<longitude::binary-size(8), lon_direction::binary>> = aprs_longitude

    <<lat_deg::binary-size(3), lat_min::binary-size(2), lat_fractional::binary-size(3)>> =
      convert_garbage_to_zero(latitude)

    <<lon_deg::binary-size(3), lon_min::binary-size(2), lon_fractional::binary-size(3)>> =
      convert_garbage_to_zero(longitude)

    try do
      lat =
        Geocalc.DMS.to_degrees(%Geocalc.DMS{
          hours: String.to_integer(lat_deg),
          minutes: String.to_integer(lat_min),
          seconds: convert_fractional(lat_fractional),
          direction: lat_direction
        })

      long =
        Geocalc.DMS.to_degrees(%Geocalc.DMS{
          hours: String.to_integer(lon_deg),
          minutes: String.to_integer(lon_min),
          seconds: convert_fractional(lon_fractional),
          direction: lon_direction
        })

      %{latitude: lat, longitude: long}
    rescue
      _ -> %{latitude: 0, longitude: 0}
    end
  end

  def from_decimal(latitude, longitude) do
    %{latitude: latitude, longitude: longitude}
  end

  defp convert_garbage_to_zero(value) do
    _ = String.to_float(value)
    value
  rescue
    ArgumentError -> "00000.00"
  end

  # def to_string(%__MODULE__{} = position) do
  #   "#{position.lat_degrees}°" <>
  #     "#{position.lat_minutes}'" <>
  #     "#{position.lat_fractional}\"" <>
  #     "#{convert_direction(position.lat_direction)} " <>
  #     "#{position.lon_degrees}°" <>
  #     "#{position.lon_minutes}'" <>
  #     "#{position.lon_fractional}\"" <> "#{convert_direction(position.lon_direction)}"
  # end

  # defp convert_direction("N"), do: :north
  # defp convert_direction("S"), do: :south
  # defp convert_direction("E"), do: :east
  # defp convert_direction("W"), do: :west
  # defp convert_direction(:north), do: "N"
  # defp convert_direction(:south), do: "S"
  # defp convert_direction(:east), do: "E"
  # defp convert_direction(:west), do: "W"
  # defp convert_direction(:unknown), do: ""
  # defp convert_direction(_nomatch), do: ""

  # defp convert_fractional(fractional),
  #   do:
  #     fractional
  #     |> String.trim()
  #     |> String.pad_leading(4, "0")
  #     |> String.to_float()
  #     |> Kernel.*(60)
  #     |> Float.round(2)

  defp convert_fractional(fractional),
    do: fractional |> String.trim() |> String.pad_leading(4, "0") |> String.to_float() |> Kernel.*(60)
end
