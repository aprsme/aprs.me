defmodule AprsmeWeb.Api.V1.WeatherController do
  @moduledoc """
  Controller for weather-related API endpoints.
  """
  use AprsmeWeb, :controller

  alias Aprsme.Packets.PreparedQueries

  action_fallback AprsmeWeb.Api.V1.FallbackController

  @doc """
  Returns nearby weather stations within a specified radius.

  ## Parameters

    * `lat` - Latitude (-90 to 90) - required
    * `lon` - Longitude (-180 to 180) - required
    * `radius` - Search radius in miles (> 0, max 1000) - required
    * `hours` - Hours of data to retrieve (1-168, default: 6) - optional
    * `limit` - Maximum number of results (1-100, default: 50) - optional

  ## Examples

      GET /api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=10
      GET /api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=10&hours=24&limit=25

  """
  def nearby(conn, params) do
    with {:ok, validated_params} <- validate_params(params) do
      stations =
        PreparedQueries.get_nearby_weather_stations(
          validated_params.lat,
          validated_params.lon,
          validated_params.radius,
          hours: validated_params.hours,
          limit: validated_params.limit
        )

      # Transform params for JSON response
      response_params = %{
        latitude: validated_params.lat,
        longitude: validated_params.lon,
        radius_miles: validated_params.radius,
        hours: validated_params.hours,
        limit: validated_params.limit
      }

      render(conn, :nearby,
        stations: stations,
        params: response_params
      )
    end
  end

  # Private Functions

  @spec validate_params(map()) ::
          {:ok,
           %{
             lat: float(),
             lon: float(),
             radius: float(),
             hours: integer(),
             limit: integer()
           }}
          | {:error, :bad_request, String.t()}
          | {:error, :unprocessable_entity, String.t()}
  defp validate_params(params) do
    with {:ok, lat} <- validate_required_float(params, "lat"),
         {:ok, lon} <- validate_required_float(params, "lon"),
         {:ok, radius} <- validate_required_float(params, "radius"),
         {:ok, hours} <- validate_optional_integer(params, "hours", 6),
         {:ok, limit} <- validate_optional_integer(params, "limit", 50),
         :ok <- validate_latitude(lat),
         :ok <- validate_longitude(lon),
         :ok <- validate_radius(radius),
         :ok <- validate_hours(hours),
         :ok <- validate_limit(limit) do
      {:ok,
       %{
         lat: lat,
         lon: lon,
         radius: radius,
         hours: hours,
         limit: limit
       }}
    end
  end

  @spec validate_required_float(map(), String.t()) ::
          {:ok, float()} | {:error, :bad_request, String.t()}
  defp validate_required_float(params, key) do
    case Map.get(params, key) do
      nil ->
        {:error, :bad_request, "Missing required parameter: #{key}"}

      value when is_binary(value) ->
        case Float.parse(value) do
          {float_value, ""} -> {:ok, float_value}
          _ -> {:error, :bad_request, "Invalid numeric value for parameter: #{key}"}
        end

      value when is_number(value) ->
        {:ok, value / 1}
    end
  end

  @spec validate_optional_integer(map(), String.t(), integer()) ::
          {:ok, integer()} | {:error, :bad_request, String.t()}
  defp validate_optional_integer(params, key, default) do
    case Map.get(params, key) do
      nil ->
        {:ok, default}

      value when is_binary(value) ->
        case Integer.parse(value) do
          {int_value, ""} -> {:ok, int_value}
          _ -> {:error, :bad_request, "Invalid integer value for parameter: #{key}"}
        end

      value when is_integer(value) ->
        {:ok, value}

      value when is_float(value) ->
        {:ok, trunc(value)}
    end
  end

  @spec validate_latitude(float()) :: :ok | {:error, :unprocessable_entity, String.t()}
  defp validate_latitude(lat) when lat >= -90 and lat <= 90, do: :ok

  defp validate_latitude(_lat), do: {:error, :unprocessable_entity, "Invalid latitude: must be between -90 and 90"}

  @spec validate_longitude(float()) :: :ok | {:error, :unprocessable_entity, String.t()}
  defp validate_longitude(lon) when lon >= -180 and lon <= 180, do: :ok

  defp validate_longitude(_lon), do: {:error, :unprocessable_entity, "Invalid longitude: must be between -180 and 180"}

  @spec validate_radius(float()) :: :ok | {:error, :unprocessable_entity, String.t()}
  defp validate_radius(radius) when radius > 0 and radius <= 1000, do: :ok

  defp validate_radius(radius) when radius <= 0,
    do: {:error, :unprocessable_entity, "Invalid radius: must be greater than 0"}

  defp validate_radius(_radius), do: {:error, :unprocessable_entity, "Invalid radius: must not exceed 1000 miles"}

  @spec validate_hours(integer()) :: :ok | {:error, :unprocessable_entity, String.t()}
  defp validate_hours(hours) when hours >= 1 and hours <= 168, do: :ok

  defp validate_hours(_hours), do: {:error, :unprocessable_entity, "Invalid hours: must be between 1 and 168"}

  @spec validate_limit(integer()) :: :ok | {:error, :unprocessable_entity, String.t()}
  defp validate_limit(limit) when limit >= 1 and limit <= 100, do: :ok

  defp validate_limit(_limit), do: {:error, :unprocessable_entity, "Invalid limit: must be between 1 and 100"}
end
