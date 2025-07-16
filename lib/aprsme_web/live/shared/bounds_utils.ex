defmodule AprsmeWeb.Live.Shared.BoundsUtils do
  @moduledoc """
  Shared bounds validation, comparison, and filtering utilities.
  Used across multiple LiveView modules for consistent bounds handling.
  """

  alias AprsmeWeb.Live.Shared.CoordinateUtils

  @doc """
  Validate bounds to prevent invalid coordinates.
  """
  @spec valid_bounds?(map()) :: boolean()
  def valid_bounds?(%{north: north, south: south, east: east, west: west})
      when is_number(north) and is_number(south) and is_number(east) and is_number(west) and north <= 90 and
             south >= -90 and north > south and east >= -180 and west <= 180, do: true

  def valid_bounds?(_invalid_bounds), do: false

  @doc """
  Compare two bounds maps for equality (with rounding for floating point comparison).
  """
  @spec compare_bounds(map() | nil, map() | nil) :: boolean()
  def compare_bounds(nil, nil), do: true
  def compare_bounds(nil, _), do: false
  def compare_bounds(_, nil), do: false
  def compare_bounds(b1, b2), do: compare_bounds_maps(b1, b2)

  @doc """
  Check if a packet is within the specified bounds.
  """
  @spec within_bounds?(map() | struct() | tuple(), map()) :: boolean()
  def within_bounds?({lat, lon}, bounds) when is_number(lat) and is_number(lon) do
    check_coordinate_bounds(lat, lon, bounds)
  end

  def within_bounds?(packet, bounds) do
    {lat, lon, _data_extended} = CoordinateUtils.get_coordinates(packet)
    check_coordinate_bounds(lat, lon, bounds)
  end

  @doc """
  Filter packets by bounds - works with both maps and lists.
  """
  @spec filter_packets_by_bounds(map() | list(), map()) :: map() | list()
  def filter_packets_by_bounds(packets_map, bounds) when is_map(packets_map) do
    packets_map
    |> Enum.filter(fn {_k, packet} -> within_bounds?(packet, bounds) end)
    |> Map.new()
  end

  def filter_packets_by_bounds(packets_list, bounds) when is_list(packets_list) do
    Enum.filter(packets_list, &within_bounds?(&1, bounds))
  end

  @doc """
  Get packet keys that are outside the specified bounds.
  """
  @spec reject_packets_by_bounds(map(), map()) :: list()
  def reject_packets_by_bounds(packets_map, bounds) when is_map(packets_map) do
    packets_map
    |> Enum.reject(fn {_k, packet} -> within_bounds?(packet, bounds) end)
    |> Enum.map(fn {k, _} -> k end)
  end

  @doc """
  Normalize map bounds from string keys to atom keys and convert values to floats.
  """
  @spec normalize_bounds(map()) :: map() | nil
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

  @doc """
  Calculate approximate bounds based on center point and zoom level.
  """
  @spec calculate_bounds_from_center_and_zoom(map(), integer()) :: map()
  def calculate_bounds_from_center_and_zoom(center, zoom) do
    # Approximate degrees per pixel at different zoom levels
    degrees_per_pixel = calculate_degrees_per_pixel(zoom)

    # Assume viewport is roughly 800x600 pixels
    # Half of 600px height
    lat_offset = degrees_per_pixel * 300
    # Half of 800px width
    lng_offset = degrees_per_pixel * 400

    %{
      north: center.lat + lat_offset,
      south: center.lat - lat_offset,
      east: center.lng + lng_offset,
      west: center.lng - lng_offset
    }
  end

  # Private helper functions

  defp compare_bounds_maps(b1, b2) do
    Enum.all?([:north, :south, :east, :west], fn key ->
      round_to_4_places(Map.get(b1, key)) == round_to_4_places(Map.get(b2, key))
    end)
  end

  defp check_coordinate_bounds(nil, _, _), do: false
  defp check_coordinate_bounds(_, nil, _), do: false

  defp check_coordinate_bounds(lat, lon, bounds) do
    # Check latitude bounds (straightforward)
    lat_in_bounds = lat >= bounds.south && lat <= bounds.north

    # Check longitude bounds (handle potential wrapping)
    lng_in_bounds = check_longitude_bounds(lon, bounds.west, bounds.east)

    lat_in_bounds && lng_in_bounds
  end

  defp check_longitude_bounds(lon, west, east) when west <= east do
    # Normal case: bounds don't cross antimeridian
    lon >= west && lon <= east
  end

  defp check_longitude_bounds(lon, west, east) do
    # Bounds cross antimeridian (e.g., west=170, east=-170)
    lon >= west || lon <= east
  end

  defp calculate_degrees_per_pixel(zoom) when zoom >= 15, do: 0.000005
  defp calculate_degrees_per_pixel(zoom) when zoom >= 12, do: 0.00005
  defp calculate_degrees_per_pixel(zoom) when zoom >= 10, do: 0.0002
  defp calculate_degrees_per_pixel(zoom) when zoom >= 8, do: 0.001
  defp calculate_degrees_per_pixel(zoom) when zoom >= 6, do: 0.005
  defp calculate_degrees_per_pixel(zoom) when zoom >= 4, do: 0.02
  defp calculate_degrees_per_pixel(_), do: 0.1

  defp round_to_4_places(n) when is_float(n), do: Float.round(n, 4)
  defp round_to_4_places(n) when is_integer(n), do: n * 1.0
  defp round_to_4_places(x), do: x

  defp to_float(value) do
    Aprsme.EncodingUtils.to_float(value) || 0.0
  end
end
