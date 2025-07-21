defmodule Aprsme.GeoUtils do
  @moduledoc """
  Geographic utility functions for calculating distances and related operations.
  """

  @earth_radius_meters 6_371_000

  @doc """
  Calculate the Haversine distance between two points in meters.

  ## Examples

      iex> Aprsme.GeoUtils.haversine_distance(33.16961, -96.4921, 33.16962, -96.4921)
      1.11
  """
  def haversine_distance(lat1, lon1, lat2, lon2)
      when is_number(lat1) and is_number(lon1) and is_number(lat2) and is_number(lon2) do
    # Convert to radians
    lat1_rad = lat1 * :math.pi() / 180
    lat2_rad = lat2 * :math.pi() / 180
    dlat_rad = (lat2 - lat1) * :math.pi() / 180
    dlon_rad = (lon2 - lon1) * :math.pi() / 180

    # Haversine formula
    a =
      :math.sin(dlat_rad / 2) * :math.sin(dlat_rad / 2) +
        :math.cos(lat1_rad) * :math.cos(lat2_rad) *
          :math.sin(dlon_rad / 2) * :math.sin(dlon_rad / 2)

    c = 2 * :math.atan2(:math.sqrt(a), :math.sqrt(1 - a))

    # Distance in meters
    @earth_radius_meters * c
  end

  def haversine_distance(_, _, _, _), do: nil

  @doc """
  Check if the distance between two points exceeds a minimum threshold.
  This helps filter out GPS drift and insignificant movements.

  Default threshold is 50 meters to account for typical GPS accuracy variations.
  """
  def significant_movement?(lat1, lon1, lat2, lon2, threshold_meters \\ 50) do
    case haversine_distance(lat1, lon1, lat2, lon2) do
      nil -> false
      distance -> distance > threshold_meters
    end
  end
end
