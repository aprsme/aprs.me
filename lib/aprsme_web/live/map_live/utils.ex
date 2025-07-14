defmodule AprsmeWeb.MapLive.Utils do
  @moduledoc """
  Common utility functions for the map LiveView.
  """

  alias AprsmeWeb.Live.Shared.CoordinateUtils
  alias AprsmeWeb.Live.Shared.PacketUtils
  alias AprsmeWeb.Live.Shared.ParamUtils

  # Delegate to shared utilities
  defdelegate safe_parse_coordinate(value, default, min, max), to: ParamUtils
  defdelegate clamp_coordinate(value, min, max), to: ParamUtils
  defdelegate clamp_zoom(zoom), to: ParamUtils
  defdelegate finite_number?(num), to: ParamUtils
  defdelegate calculate_distance_meters(lat1, lon1, lat2, lon2), to: CoordinateUtils
  defdelegate parse_trail_duration(duration), to: PacketUtils
  defdelegate parse_historical_hours(hours), to: PacketUtils
  defdelegate get_callsign_key(packet), to: PacketUtils
  defdelegate valid_callsign?(callsign), to: ParamUtils
  defdelegate sanitize_path_string(path), to: ParamUtils
  defdelegate prune_oldest_packets(packets_map, max_count), to: PacketUtils

  @doc """
  Round number to 4 decimal places for bounds comparison.
  """
  @spec round_to_4_places(number() | any()) :: number()
  def round_to_4_places(n) when is_float(n), do: Float.round(n, 4)
  def round_to_4_places(n) when is_integer(n), do: n * 1.0
  def round_to_4_places(x), do: x
end
