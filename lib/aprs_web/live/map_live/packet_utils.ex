defmodule AprsWeb.MapLive.PacketUtils do
  @moduledoc """
  Shared utilities for extracting and processing packet data in map views.
  """

  @doc """
  Safely extracts a value from a packet or data_extended map with fallback support.
  Checks both atom and string keys, and provides a default value.
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

  @doc """
  Extracts symbol information from a packet with fallbacks.
  """
  @spec get_symbol_info(map()) :: {String.t(), String.t()}
  def get_symbol_info(packet) do
    symbol_table_id = get_packet_field(packet, :symbol_table_id, "/")
    symbol_code = get_packet_field(packet, :symbol_code, ">")

    {symbol_table_id, symbol_code}
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
  Converts various numeric types to float for consistent display.
  """
  @spec to_float(any()) :: float()
  def to_float(value) do
    case value do
      %Decimal{} = d ->
        Decimal.to_float(d)

      n when is_float(n) ->
        n

      n when is_integer(n) ->
        n * 1.0

      n when is_binary(n) ->
        case Float.parse(n) do
          {f, _} -> f
          :error -> 0.0
        end

      _ ->
        0.0
    end
  end

  @doc """
  Generates a callsign string with SSID if present.
  """
  @spec generate_callsign(map()) :: String.t()
  def generate_callsign(packet) do
    base_callsign = get_packet_field(packet, :base_callsign, "")
    ssid = get_packet_field(packet, :ssid, 0)

    if ssid != 0 and ssid != "" and ssid != nil do
      "#{base_callsign}-#{ssid}"
    else
      base_callsign
    end
  end

  @doc """
  Determines if a packet is a weather packet.
  """
  @spec is_weather_packet?(map()) :: boolean()
  def is_weather_packet?(packet) do
    data_type = get_packet_field(packet, :data_type, "")
    {symbol_table_id, symbol_code} = get_symbol_info(packet)

    data_type == "weather" or (symbol_table_id == "/" and symbol_code == "_")
  end

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
    data_extended = Map.get(packet, "data_extended", %{}) || %{}

    Map.get(packet, key) ||
      Map.get(packet, to_string(key)) ||
      Map.get(data_extended, key) ||
      Map.get(data_extended, to_string(key)) || "N/A"
  end
end
