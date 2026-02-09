defmodule Aprsme.Packets.Clustering do
  @moduledoc """
  Handles clustering of APRS packets for heat map visualization at low zoom levels.
  """

  alias Aprsme.Packet

  @doc """
  Clusters packets based on zoom level.

  Returns {:raw_packets, packets} for zoom > 8, or {:heat_map, clusters} for zoom <= 8.

  ## Parameters
    - packets: List of Packet structs
    - zoom: Current map zoom level (1-20)
    - opts: Additional options (unused currently)

  ## Returns
    - {:raw_packets, packets} when zoom > 8
    - {:heat_map, clusters} when zoom <= 8, where clusters is a list of:
      %{lat: float, lng: float, intensity: integer}
  """
  @spec cluster_packets([Packet.t()], integer(), map()) ::
          {:raw_packets, [Packet.t()]} | {:heat_map, [map()]}
  def cluster_packets(packets, zoom, _opts \\ %{})

  def cluster_packets(packets, zoom, _opts) when zoom > 8 do
    {:raw_packets, packets}
  end

  def cluster_packets(packets, zoom, _opts) do
    # Calculate clustering radius based on zoom level
    # Lower zoom = larger radius (more clustering)
    radius = calculate_cluster_radius(zoom)

    # Filter out packets without valid coordinates
    valid_packets = filter_valid_coordinates(packets)

    # Perform clustering
    clusters = perform_clustering(valid_packets, radius)

    {:heat_map, clusters}
  end

  @doc """
  Calculates the clustering radius in degrees based on zoom level.
  Lower zoom levels get larger radii for more aggressive clustering.
  """
  # Clustering radius lookup table by zoom level
  # More aggressive scaling for better separation at higher zooms
  # Zoom 1: ~5 degrees, Zoom 5: ~0.3 degrees, Zoom 8: ~0.04 degrees
  @cluster_radii %{
    1 => 5.0,
    2 => 2.5,
    3 => 1.25,
    4 => 0.625,
    5 => 0.3125,
    6 => 0.15625,
    7 => 0.078125,
    8 => 0.0390625
  }
  @default_radius 0.0390625

  @spec calculate_cluster_radius(integer()) :: float()
  def calculate_cluster_radius(zoom) do
    Map.get(@cluster_radii, zoom, @default_radius)
  end

  # Filter packets with valid lat/lon coordinates
  defp filter_valid_coordinates(packets) do
    Enum.filter(packets, fn packet ->
      lat = decimal_to_float(Map.get(packet, :lat) || Map.get(packet, "lat"))
      lon = decimal_to_float(Map.get(packet, :lon) || Map.get(packet, "lon"))

      not is_nil(lat) and not is_nil(lon) and
        lat >= -90 and lat <= 90 and
        lon >= -180 and lon <= 180
    end)
  end

  # Convert Decimal to float, handling nil
  defp decimal_to_float(nil), do: nil
  defp decimal_to_float(%Decimal{} = d), do: Decimal.to_float(d)
  defp decimal_to_float(n) when is_number(n), do: n / 1.0

  defp decimal_to_float(s) when is_binary(s) do
    case Float.parse(s) do
      {f, _} -> f
      :error -> nil
    end
  end

  defp decimal_to_float(_), do: nil

  # Perform the actual clustering using a simple grid-based approach
  defp perform_clustering(packets, radius) do
    packets
    |> Enum.reduce(%{}, fn packet, clusters ->
      lat = decimal_to_float(Map.get(packet, :lat) || Map.get(packet, "lat"))
      lon = decimal_to_float(Map.get(packet, :lon) || Map.get(packet, "lon"))

      # Find cluster key by rounding to grid
      cluster_lat = Float.round(lat / radius) * radius
      cluster_lon = Float.round(lon / radius) * radius
      cluster_key = {cluster_lat, cluster_lon}

      # Update cluster
      Map.update(clusters, cluster_key, %{lat: lat, lng: lon, intensity: 1, lat_sum: lat, lon_sum: lon}, fn cluster ->
        %{
          lat_sum: cluster.lat_sum + lat,
          lon_sum: cluster.lon_sum + lon,
          intensity: cluster.intensity + 1,
          # Average position will be calculated after
          lat: 0,
          lng: 0
        }
      end)
    end)
    |> Enum.map(fn {{_cluster_lat, _cluster_lon}, cluster} ->
      # Calculate average position for cluster center
      %{
        lat: cluster.lat_sum / cluster.intensity,
        lng: cluster.lon_sum / cluster.intensity,
        intensity: cluster.intensity
      }
    end)
  end
end
