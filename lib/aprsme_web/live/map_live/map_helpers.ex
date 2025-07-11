defmodule AprsmeWeb.MapLive.MapHelpers do
  @moduledoc false

  alias Aprs.Types.MicE

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
    if is_nil(bounds) do
      false
    else
      {lat, lon} = extract_lat_lon(packet_or_coords)

      if is_nil(lat) or is_nil(lon) do
        false
      else
        check_bounds(lat, lon, bounds)
      end
    end
  end

  defp check_bounds(lat, lon, bounds) do
    lat = to_float(lat)
    lon = to_float(lon)
    south = to_float(bounds.south)
    north = to_float(bounds.north)
    west = to_float(bounds.west)
    east = to_float(bounds.east)

    lat_in_bounds = lat >= south && lat <= north
    lng_in_bounds = check_longitude_bounds(lon, west, east)

    lat_in_bounds && lng_in_bounds
  end

  defp check_longitude_bounds(lon, west, east) do
    if west <= east do
      lon >= west && lon <= east
    else
      lon >= west || lon <= east
    end
  end

  defp extract_lat_lon(%{lat: lat, lon: lon}), do: extract_lat_lon_atom(%{lat: lat, lon: lon})

  defp extract_lat_lon(%{"lat" => lat, "lon" => lon}), do: extract_lat_lon_string(%{"lat" => lat, "lon" => lon})

  defp extract_lat_lon(%{latitude: lat, longitude: lon}), do: extract_lat_lon_atom_alt(%{latitude: lat, longitude: lon})

  defp extract_lat_lon({lat, lon}) when is_number(lat) and is_number(lon), do: {lat, lon}
  defp extract_lat_lon(_), do: {nil, nil}

  defp extract_lat_lon_atom(packet), do: {packet.lat, packet.lon}
  defp extract_lat_lon_string(packet), do: {packet["lat"], packet["lon"]}
  defp extract_lat_lon_atom_alt(packet), do: {packet.latitude, packet.longitude}

  defp to_float(value) do
    Aprsme.EncodingUtils.to_float(value) || 0.0
  end

  @doc """
  Normalizes map bounds from string keys to atom keys and converts values to floats.

  ## Examples

      iex> normalize_bounds(%{"north" => "40.5", "south" => "40.0", "east" => "-73.5", "west" => "-74.0"})
      %{north: 40.5, south: 40.0, east: -73.5, west: -74.0}
      
      iex> normalize_bounds(%{"north" => 40.5, "south" => 40.0, "east" => -73.5, "west" => -74.0})
      %{north: 40.5, south: 40.0, east: -73.5, west: -74.0}
  """
  @spec normalize_bounds(map()) :: map()
  def normalize_bounds(%{"north" => n, "south" => s, "east" => e, "west" => w}) do
    %{
      north: to_float(n),
      south: to_float(s),
      east: to_float(e),
      west: to_float(w)
    }
  end

  def normalize_bounds(%{north: _, south: _, east: _, west: _} = bounds) do
    # Already normalized with atom keys
    bounds
  end

  def normalize_bounds(_), do: nil
end
