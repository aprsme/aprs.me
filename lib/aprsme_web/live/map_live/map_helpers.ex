defmodule AprsmeWeb.MapLive.MapHelpers do
  @moduledoc false

  alias Aprs.Types.MicE
  alias AprsmeWeb.Live.Shared.BoundsUtils

  @spec get_coordinates(map() | struct()) :: {number() | nil, number() | nil, map() | nil}
  def get_coordinates(%{data_extended: %MicE{} = mic_e}) do
    {lat, lon} = get_coordinates_from_mic_e(mic_e)
    {lat, lon, mic_e}
  end

  def get_coordinates(%{data_extended: %{latitude: lat, longitude: lon}} = packet) do
    {lat, lon, packet.data_extended}
  end

  def get_coordinates(packet) do
    lat = Map.get(packet, :lat) || Map.get(packet, "lat")
    lon = Map.get(packet, :lon) || Map.get(packet, "lon")
    data_extended = Map.get(packet, :data_extended) || Map.get(packet, "data_extended")
    {lat, lon, data_extended}
  end

  @spec get_coordinates_from_mic_e(MicE.t()) :: {number() | nil, number() | nil}
  def get_coordinates_from_mic_e(mic_e) do
    lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0 + mic_e.lat_fractional / 6000.0
    lat = if mic_e.lat_direction == :south, do: -lat, else: lat
    lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0 + mic_e.lon_fractional / 6000.0
    lon = if mic_e.lon_direction == :west, do: -lon, else: lon
    if lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180, do: {lat, lon}, else: {nil, nil}
  end

  @spec has_position_data?(map()) :: boolean()
  def has_position_data?(packet) do
    lat = Map.get(packet, :lat) || Map.get(packet, "lat")
    lon = Map.get(packet, :lon) || Map.get(packet, "lon")

    if has_direct_coordinates?(lat, lon) do
      true
    else
      has_position_in_data_extended?(packet)
    end
  end

  defp has_direct_coordinates?(lat, lon) when not is_nil(lat) and not is_nil(lon), do: true
  defp has_direct_coordinates?(_, _), do: false

  defp has_position_in_data_extended?(packet) do
    data_extended = Map.get(packet, :data_extended) || Map.get(packet, "data_extended")
    has_position_in_data_extended_case?(data_extended)
  end

  defp has_position_in_data_extended_case?(%MicE{}), do: true

  defp has_position_in_data_extended_case?(%{latitude: lat, longitude: lon}) when not is_nil(lat) and not is_nil(lon),
    do: true

  defp has_position_in_data_extended_case?(_), do: false

  @spec within_bounds?(map() | tuple(), map()) :: boolean()
  def within_bounds?(packet_or_coords, bounds) do
    # Delegate to shared bounds utility
    BoundsUtils.within_bounds?(packet_or_coords, bounds)
  end

  # Delegate normalize_bounds to shared utility
  defdelegate normalize_bounds(bounds), to: BoundsUtils
end
