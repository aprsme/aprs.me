defmodule AprsmeWeb.MapLive.PacketUtils do
  @moduledoc """
  Map-specific packet utilities. Provides weather-check queries against the
  database and a convenience wrapper for weather field display.

  For general packet field access, symbol info, timestamps, callsign
  generation, and weather-packet detection, use
  `AprsmeWeb.Live.Shared.PacketUtils` directly.

  For building marker/popup data, use `AprsmeWeb.MapLive.DataBuilder` directly.
  """

  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.MapLive.DataBuilder

  @doc """
  Checks if a station has sent weather packets by looking at recent packets.
  """
  @spec has_weather_packets?(String.t()) :: boolean()
  def has_weather_packets?(callsign) when is_binary(callsign) do
    query = build_weather_check_query(callsign)
    Aprsme.Repo.exists?(query)
  rescue
    _ -> false
  end

  def has_weather_packets?(_), do: false

  @doc """
  Recursively converts tuples in data structures to strings for JSON serialization.
  """
  @spec convert_tuples_to_strings(any()) :: any()
  def convert_tuples_to_strings(map) when is_map(map) do
    if Map.has_key?(map, :__struct__) do
      map
    else
      Map.new(map, fn {k, v} ->
        {k, convert_tuples_to_strings(v)}
      end)
    end
  end

  def convert_tuples_to_strings(list) when is_list(list) do
    Enum.map(list, &convert_tuples_to_strings/1)
  end

  def convert_tuples_to_strings(tuple) when is_tuple(tuple) do
    to_string(inspect(tuple))
  end

  def convert_tuples_to_strings(other), do: other

  @doc """
  Extracts weather field data with "N/A" fallback for display.
  """
  @spec get_weather_field(map(), atom()) :: String.t()
  def get_weather_field(packet, key) do
    case SharedPacketUtils.get_weather_field(packet, key) do
      nil -> "N/A"
      value -> value
    end
  end

  # Keep build_packet_data delegations for backward compatibility with tests
  defdelegate build_packet_data(packet, is_most_recent_for_callsign, locale), to: DataBuilder
  defdelegate build_packet_data(packet, is_most_recent_for_callsign), to: DataBuilder
  defdelegate build_packet_data(packet), to: DataBuilder

  defp build_weather_check_query(callsign) do
    import Ecto.Query

    from p in Aprsme.Packet,
      where: p.sender == ^callsign,
      where:
        fragment(
          "? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ?->>'luminosity' IS NOT NULL OR ?->>'rain_midnight' IS NOT NULL",
          p.temperature,
          p.humidity,
          p.pressure,
          p.wind_direction,
          p.wind_speed,
          p.wind_gust,
          p.rain_1h,
          p.rain_24h,
          p.snow,
          p.data,
          p.data
        ),
      select: fragment("1"),
      limit: 1
  end
end
