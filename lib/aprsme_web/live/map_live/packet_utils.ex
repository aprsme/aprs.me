defmodule AprsmeWeb.MapLive.PacketUtils do
  @moduledoc """
  Shared utilities for extracting and processing packet data in map views.
  """

  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.Live.Shared.ParamUtils
  alias AprsmeWeb.MapLive.DataBuilder

  @doc """
  Safely extracts a value from a packet or data_extended map with fallback support.
  Checks both atom and string keys, and provides a default value.
  """
  @spec get_packet_field(map(), atom() | String.t(), any()) :: any()
  def get_packet_field(packet, field, default \\ nil) do
    SharedPacketUtils.get_packet_field(packet, field, default)
  end

  @doc """
  Extracts symbol information from a packet with fallbacks.
  """
  @spec get_symbol_info(map()) :: {String.t(), String.t()}
  def get_symbol_info(packet) do
    SharedPacketUtils.get_symbol_info(packet)
  end

  @doc """
  Extracts timestamp from a packet in ISO8601 format.
  """
  @spec get_timestamp(map()) :: String.t()
  def get_timestamp(packet) do
    SharedPacketUtils.get_timestamp(packet)
  end

  @doc """
  Converts various numeric types to float for consistent display.
  """
  @spec to_float(any()) :: float()
  def to_float(value) do
    ParamUtils.to_float(value)
  end

  @doc """
  Generates a callsign string with SSID if present.
  """
  @spec generate_callsign(map()) :: String.t()
  def generate_callsign(packet) do
    SharedPacketUtils.generate_callsign(packet)
  end

  @doc """
  Determines if a packet is a weather packet.
  """
  @spec weather_packet?(map()) :: boolean()
  def weather_packet?(packet) do
    SharedPacketUtils.weather_packet?(packet)
  end

  @doc """
  Checks if a station has sent weather packets by looking at recent packets.
  """
  @spec has_weather_packets?(String.t()) :: boolean()
  # Get recent packets for this callsign and check if any are weather packets
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
  Extracts weather field data with fallback support.
  """
  @spec get_weather_field(map(), atom()) :: String.t()
  def get_weather_field(packet, key) do
    case SharedPacketUtils.get_weather_field(packet, key) do
      nil -> "N/A"
      value -> value
    end
  end

  # These functions have been moved to AprsmeWeb.MapLive.DataBuilder
  defdelegate build_packet_data(packet, is_most_recent_for_callsign, locale), to: DataBuilder
  defdelegate build_packet_data(packet, is_most_recent_for_callsign), to: DataBuilder
  defdelegate build_packet_data(packet), to: DataBuilder

  # All packet data building functions have been moved to AprsmeWeb.MapLive.DataBuilder

  defp build_weather_check_query(callsign) do
    import Ecto.Query

    from p in Aprsme.Packet,
      where: p.sender == ^callsign,
      where:
        fragment(
          "? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL OR ? IS NOT NULL",
          p.temperature,
          p.humidity,
          p.pressure,
          p.wind_direction,
          p.wind_speed,
          p.wind_gust,
          p.rain_1h,
          p.rain_24h,
          p.rain_midnight,
          p.luminosity,
          p.snow_24h
        ),
      select: fragment("1"),
      limit: 1
  end
end
