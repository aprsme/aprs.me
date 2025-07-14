defmodule AprsmeWeb.MapLive.BoundsManager do
  @moduledoc """
  Handles map bounds calculations, validation, and filtering operations.
  """

  alias AprsmeWeb.Live.Shared.BoundsUtils

  # Delegate to shared utilities
  defdelegate calculate_bounds_from_center_and_zoom(center, zoom), to: BoundsUtils
  defdelegate valid_bounds?(map_bounds), to: BoundsUtils
  defdelegate compare_bounds(b1, b2), to: BoundsUtils
  defdelegate within_bounds?(packet, bounds), to: BoundsUtils
  defdelegate filter_packets_by_bounds(packets, bounds), to: BoundsUtils
  defdelegate reject_packets_by_bounds(packets_map, bounds), to: BoundsUtils

  @doc """
  Filter packets by both time threshold and bounds.
  """
  @spec filter_packets_by_time_and_bounds(map(), map(), DateTime.t()) :: map()
  def filter_packets_by_time_and_bounds(packets, bounds, time_threshold) do
    # Use shared packet utils for this functionality
    AprsmeWeb.Live.Shared.PacketUtils.filter_packets_by_time_and_bounds(packets, bounds, time_threshold)
  end
end
