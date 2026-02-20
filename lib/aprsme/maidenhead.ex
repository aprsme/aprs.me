defmodule Aprsme.Maidenhead do
  @moduledoc """
  Encodes longitude/latitude to Maidenhead grid square locators (6 characters).
  """

  @spec encode(number() | nil, number() | nil) :: String.t() | nil
  def encode(lon, lat) when is_number(lon) and is_number(lat) do
    lon = lon + 180.0
    lat = lat + 90.0

    field_lon = trunc(lon / 20)
    field_lat = trunc(lat / 10)

    lon = lon - field_lon * 20
    lat = lat - field_lat * 10

    square_lon = trunc(lon / 2)
    square_lat = trunc(lat)

    lon = lon - square_lon * 2
    lat = lat - square_lat

    sub_lon = trunc(lon / (2 / 24))
    sub_lat = trunc(lat / (1 / 24))

    sub_lon = min(sub_lon, 23)
    sub_lat = min(sub_lat, 23)

    <<?A + field_lon, ?A + field_lat, ?0 + square_lon, ?0 + square_lat, ?a + sub_lon, ?a + sub_lat>>
  end

  def encode(_lon, _lat), do: nil
end
