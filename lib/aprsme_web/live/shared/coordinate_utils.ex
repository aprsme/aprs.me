defmodule AprsmeWeb.Live.Shared.CoordinateUtils do
  @moduledoc """
  Shared coordinate parsing, validation, and calculation utilities.
  Used across multiple LiveView modules for consistent coordinate handling.
  """

  alias Aprs.Types.MicE
  alias AprsmeWeb.Live.Shared.ParamUtils

  @doc """
  Extract coordinates from various packet formats.
  Returns {lat, lon, data_extended} tuple.
  """
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

  @doc """
  Extract coordinates from MicE data format.
  """
  @spec get_coordinates_from_mic_e(MicE.t()) :: {number() | nil, number() | nil}
  def get_coordinates_from_mic_e(mic_e) do
    lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0 + mic_e.lat_fractional / 6000.0
    lat = if mic_e.lat_direction == :south, do: -lat, else: lat
    lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0 + mic_e.lon_fractional / 6000.0
    lon = if mic_e.lon_direction == :west, do: -lon, else: lon
    if lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180, do: {lat, lon}, else: {nil, nil}
  end

  @doc """
  Check if packet has position data in any format.
  """
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

  @doc """
  Validate that coordinates are within reasonable ranges.
  """
  @spec valid_coordinates?(number(), number()) :: boolean()
  def valid_coordinates?(lat, lng) when is_number(lat) and is_number(lng) do
    lat >= -90 and lat <= 90 and lng >= -180 and lng <= 180
  end

  def valid_coordinates?(_, _), do: false

  @doc """
  Calculate distance between two lat/lon points in meters using Haversine formula.
  """
  @spec calculate_distance_meters(number(), number(), number(), number()) :: float()
  def calculate_distance_meters(lat1, lon1, lat2, lon2) do
    # Convert latitude and longitude from degrees to radians
    lat1_rad = lat1 * :math.pi() / 180
    lon1_rad = lon1 * :math.pi() / 180
    lat2_rad = lat2 * :math.pi() / 180
    lon2_rad = lon2 * :math.pi() / 180

    # Haversine formula
    dlat = lat2_rad - lat1_rad
    dlon = lon2_rad - lon1_rad

    a =
      :math.sin(dlat / 2) * :math.sin(dlat / 2) +
        :math.cos(lat1_rad) * :math.cos(lat2_rad) *
          :math.sin(dlon / 2) * :math.sin(dlon / 2)

    c = 2 * :math.atan2(:math.sqrt(a), :math.sqrt(1 - a))

    # Earth's radius in meters
    earth_radius_meters = 6_371_000

    # Distance in meters
    earth_radius_meters * c
  end

  # Private helper functions

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

  @doc """
  Parse center coordinates from client update with validation.
  """
  @spec parse_center_coordinates(map() | any(), Phoenix.LiveView.Socket.t()) :: {float(), float()}
  def parse_center_coordinates(center, socket) when is_map(center) do
    default_lat = socket.assigns.map_center.lat
    default_lng = socket.assigns.map_center.lng

    lat = ParamUtils.safe_parse_coordinate(Map.get(center, "lat"), default_lat, -90.0, 90.0)
    lng = ParamUtils.safe_parse_coordinate(Map.get(center, "lng"), default_lng, -180.0, 180.0)

    {lat, lng}
  end

  def parse_center_coordinates(_, socket) do
    # Invalid center format, return current center
    {socket.assigns.map_center.lat, socket.assigns.map_center.lng}
  end
end
