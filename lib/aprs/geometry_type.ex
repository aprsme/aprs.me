defmodule Aprs.GeometryType do
  @moduledoc """
  Custom Ecto type for PostGIS geometry fields.
  This wraps the Geo.PostGIS.Geometry type to handle PostGIS geometry data.
  """

  use Ecto.Type

  def type, do: :geometry

  def cast(%Geo.Point{} = point), do: {:ok, point}
  def cast(%Geo.Polygon{} = polygon), do: {:ok, polygon}
  def cast(%Geo.LineString{} = linestring), do: {:ok, linestring}
  def cast(%Geo.MultiPoint{} = multipoint), do: {:ok, multipoint}
  def cast(%Geo.MultiPolygon{} = multipolygon), do: {:ok, multipolygon}
  def cast(%Geo.MultiLineString{} = multilinestring), do: {:ok, multilinestring}
  def cast(%Geo.GeometryCollection{} = collection), do: {:ok, collection}

  # Handle coordinate tuples and create Point geometry
  def cast({lon, lat}) when is_number(lon) and is_number(lat) do
    if lon >= -180 and lon <= 180 and lat >= -90 and lat <= 90 do
      {:ok, %Geo.Point{coordinates: {lon, lat}, srid: 4326}}
    else
      :error
    end
  end

  # Handle maps with lat/lon
  def cast(%{lat: lat, lon: lon}) when is_number(lat) and is_number(lon) do
    cast({lon, lat})
  end

  def cast(%{"lat" => lat, "lon" => lon}) when is_number(lat) and is_number(lon) do
    cast({lon, lat})
  end

  def cast(nil), do: {:ok, nil}
  def cast(_), do: :error

  def load(data) when is_binary(data) do
    # Handle WKB binary data directly
    case Geo.WKB.decode(data) do
      {:ok, geometry} -> {:ok, geometry}
      _ -> :error
    end
  end

  def load(%Geo.Point{} = point), do: {:ok, point}
  def load(%Geo.Polygon{} = polygon), do: {:ok, polygon}
  def load(%Geo.LineString{} = linestring), do: {:ok, linestring}
  def load(%Geo.MultiPoint{} = multipoint), do: {:ok, multipoint}
  def load(%Geo.MultiPolygon{} = multipolygon), do: {:ok, multipolygon}
  def load(%Geo.MultiLineString{} = multilinestring), do: {:ok, multilinestring}
  def load(%Geo.GeometryCollection{} = collection), do: {:ok, collection}
  def load(nil), do: {:ok, nil}
  def load(_), do: :error

  def dump(geometry) when not is_nil(geometry) do
    # Encode to WKB binary format
    case Geo.WKB.encode(geometry) do
      {:ok, data} -> {:ok, data}
      _ -> :error
    end
  end

  def dump(nil), do: {:ok, nil}
  def dump(_), do: :error

  @doc """
  Helper function to create a Point geometry from lat/lon coordinates.
  """
  def create_point(lat, lon) when is_number(lat) and is_number(lon) do
    if lat >= -90 and lat <= 90 and lon >= -180 and lon <= 180 do
      %Geo.Point{coordinates: {lon, lat}, srid: 4326}
    else
      nil
    end
  end

  def create_point(_, _), do: nil

  @doc """
  Extract lat/lon coordinates from a Point geometry.
  """
  def extract_coordinates(%Geo.Point{coordinates: {lon, lat}}), do: {lat, lon}
  def extract_coordinates(_), do: {nil, nil}
end
