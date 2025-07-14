defmodule AprsmeWeb.Live.Shared.PacketUtils do
  @moduledoc """
  Shared packet processing and memory management utilities.
  Used across multiple LiveView modules for consistent packet handling.
  """

  @doc """
  Get unique callsign key from packet.
  """
  @spec get_callsign_key(map()) :: binary()
  def get_callsign_key(packet) do
    if Map.has_key?(packet, "id"),
      do: to_string(packet["id"]),
      else: [:positive] |> System.unique_integer() |> to_string()
  end

  @doc """
  Prune oldest packets from a map to enforce memory limits.
  """
  @spec prune_oldest_packets(map(), integer()) :: map()
  def prune_oldest_packets(packets_map, max_count) when map_size(packets_map) <= max_count do
    packets_map
  end

  def prune_oldest_packets(packets_map, max_count) do
    # Convert to list, sort by timestamp, take newest max_count packets
    packets_map
    |> Enum.to_list()
    |> Enum.sort_by(
      fn {_key, packet} ->
        case packet do
          %{received_at: %DateTime{} = dt} -> DateTime.to_unix(dt)
          %{"received_at" => timestamp} when is_integer(timestamp) -> timestamp
          _ -> 0
        end
      end,
      :desc
    )
    |> Enum.take(max_count)
    |> Map.new()
  end

  @doc """
  Parse trail duration with validation and bounds checking.
  """
  @spec parse_trail_duration(binary() | any()) :: integer()
  def parse_trail_duration(duration) when is_binary(duration) do
    case Integer.parse(duration) do
      {hours, ""} when hours in [1, 6, 12, 24, 48, 168] ->
        hours

      _ ->
        # Default to 1 hour if invalid
        1
    end
  end

  def parse_trail_duration(_), do: 1

  @doc """
  Parse historical hours with validation.
  """
  @spec parse_historical_hours(binary() | any()) :: integer()
  def parse_historical_hours(hours) when is_binary(hours) do
    case Integer.parse(hours) do
      {h, ""} when h in [1, 3, 6, 12, 24] ->
        h

      _ ->
        # Default to 1 hour if invalid
        1
    end
  end

  def parse_historical_hours(_), do: 1

  @doc """
  Filter packets by both time threshold and bounds.
  """
  @spec filter_packets_by_time_and_bounds(map(), map(), DateTime.t()) :: map()
  def filter_packets_by_time_and_bounds(packets, bounds, time_threshold) do
    packets
    |> Enum.filter(fn {_callsign, packet} ->
      AprsmeWeb.Live.Shared.BoundsUtils.within_bounds?(packet, bounds) &&
        packet_within_time_threshold?(packet, time_threshold)
    end)
    |> Map.new()
  end

  @doc """
  Check if packet contains weather data.
  """
  @spec has_weather_data?(map()) :: boolean()
  def has_weather_data?(packet) do
    weather_fields = [
      :temperature,
      :humidity,
      :pressure,
      :wind_speed,
      :wind_direction,
      :rain_1h,
      :rain_24h,
      :rain_since_midnight,
      :snow,
      :luminosity
    ]

    Enum.any?(weather_fields, fn field ->
      value = get_packet_field(packet, field, nil)
      not is_nil(value)
    end)
  end

  @doc """
  Check if packet is a weather packet based on its data.
  """
  @spec weather_packet?(map()) :: boolean()
  def weather_packet?(packet) do
    has_weather_data?(packet)
  end

  @doc """
  Get weather field value from packet with fallback.
  """
  @spec get_weather_field(map(), atom()) :: any()
  def get_weather_field(packet, field) do
    data_extended = Map.get(packet, :data_extended, Map.get(packet, "data_extended", %{})) || %{}

    Map.get(packet, field) ||
      Map.get(packet, to_string(field)) ||
      Map.get(data_extended, field) ||
      Map.get(data_extended, to_string(field))
  end

  @doc """
  Get packet field value with fallback and default.
  Checks both atom and string keys, and data_extended field.
  """
  @spec get_packet_field(map(), atom() | String.t(), any()) :: any()
  def get_packet_field(packet, field, default \\ nil) do
    data_extended = Map.get(packet, :data_extended, Map.get(packet, "data_extended", %{})) || %{}

    Map.get(packet, field) ||
      Map.get(packet, to_string(field)) ||
      Map.get(data_extended, field) ||
      Map.get(data_extended, to_string(field)) ||
      default
  end

  # Private helper functions

  @doc """
  Check if a packet is within the time threshold (not too old).
  """
  @spec packet_within_time_threshold?(struct(), any()) :: boolean()
  def packet_within_time_threshold?(packet, threshold) do
    case packet do
      %{received_at: received_at} when not is_nil(received_at) ->
        threshold_dt = convert_threshold_to_datetime(threshold)
        DateTime.compare(received_at, threshold_dt) in [:gt, :eq]

      _ ->
        # If no timestamp, treat as current
        true
    end
  end

  defp convert_threshold_to_datetime(threshold) when is_integer(threshold) do
    # Assume seconds since epoch
    DateTime.from_unix!(threshold)
  end

  defp convert_threshold_to_datetime(threshold) when is_binary(threshold) do
    case DateTime.from_iso8601(threshold) do
      {:ok, dt, _} -> dt
      _ -> DateTime.utc_now()
    end
  end

  defp convert_threshold_to_datetime(%DateTime{} = threshold), do: threshold
  defp convert_threshold_to_datetime(_), do: DateTime.utc_now()

  @doc """
  Extracts symbol information from a packet with fallbacks.
  """
  @spec get_symbol_info(map()) :: {String.t(), String.t()}
  def get_symbol_info(packet) do
    AprsmeWeb.AprsSymbol.extract_from_packet(packet)
  end

  @doc """
  Extracts timestamp from a packet in ISO8601 format.
  """
  @spec get_timestamp(map()) :: String.t()
  def get_timestamp(packet) do
    cond do
      Map.has_key?(packet, :received_at) && packet.received_at ->
        DateTime.to_iso8601(packet.received_at)

      Map.has_key?(packet, "received_at") && packet["received_at"] ->
        DateTime.to_iso8601(packet["received_at"])

      true ->
        ""
    end
  end

  @doc """
  Generates a callsign string with SSID if present.
  """
  @spec generate_callsign(map()) :: String.t()
  def generate_callsign(packet) do
    base_callsign = get_packet_field(packet, :base_callsign, "")
    ssid = get_packet_field(packet, :ssid, nil)

    if ssid != nil and ssid != "" do
      "#{base_callsign}-#{ssid}"
    else
      base_callsign
    end
  end
end
