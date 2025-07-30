defmodule AprsmeWeb.MapLive.UrlParams do
  @moduledoc """
  Handles URL parameter parsing, validation, and sanitization for the map LiveView.
  """

  alias AprsmeWeb.Live.Shared.CoordinateUtils
  alias AprsmeWeb.Live.Shared.ParamUtils

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5

  @doc """
  Parse map state from URL parameters.
  Returns {map_center, zoom} tuple with validated coordinates.
  """
  @spec parse_map_params(map()) :: {map(), integer()}
  def parse_map_params(params) do
    lat = parse_latitude(Map.get(params, "lat"))
    lng = parse_longitude(Map.get(params, "lng"))
    zoom = parse_zoom(Map.get(params, "z"))

    map_center = %{lat: lat, lng: lng}
    {map_center, zoom}
  end

  @doc """
  Parse and validate latitude parameter.
  """
  @spec parse_latitude(binary() | nil) :: float()
  def parse_latitude(nil), do: @default_center.lat

  def parse_latitude(lat_str) do
    ParamUtils.parse_float_in_range(lat_str, @default_center.lat, -90.0, 90.0)
  end

  @doc """
  Parse and validate longitude parameter.
  """
  @spec parse_longitude(binary() | nil) :: float()
  def parse_longitude(nil), do: @default_center.lng

  def parse_longitude(lng_str) do
    ParamUtils.parse_float_in_range(lng_str, @default_center.lng, -180.0, 180.0)
  end

  @doc """
  Parse and validate zoom parameter.
  """
  @spec parse_zoom(binary() | nil) :: integer()
  def parse_zoom(nil), do: @default_zoom

  def parse_zoom(zoom_str) do
    ParamUtils.parse_int_in_range(zoom_str, @default_zoom, 1, 20)
  end

  # Delegate to shared utilities
  defdelegate parse_float_in_range(str, default, min, max), to: ParamUtils
  defdelegate parse_int_in_range(str, default, min, max), to: ParamUtils
  defdelegate sanitize_numeric_string(str), to: ParamUtils
  defdelegate limit_string_length(str, max_length), to: ParamUtils
  defdelegate finite?(float), to: ParamUtils
  defdelegate valid_coordinates?(lat, lng), to: CoordinateUtils

  @doc """
  Check if URL parameters explicitly specify map location.
  """
  @spec has_explicit_url_params?(map()) :: boolean()
  def has_explicit_url_params?(params) do
    !!(params["lat"] || params["lng"] || params["z"])
  end

  @doc """
  Get default map center coordinates.
  """
  @spec default_center() :: map()
  def default_center, do: @default_center

  @doc """
  Get default zoom level.
  """
  @spec default_zoom() :: integer()
  def default_zoom, do: @default_zoom
end
