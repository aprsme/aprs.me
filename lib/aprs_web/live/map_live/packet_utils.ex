defmodule AprsWeb.MapLive.PacketUtils do
  @moduledoc """
  Shared utilities for extracting and processing packet data in map views.
  """

  alias AprsWeb.MapLive.MapHelpers
  alias AprsWeb.TimeHelpers

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
    ssid = get_packet_field(packet, :ssid, nil)

    if ssid != nil and ssid != "" do
      "#{base_callsign}-#{ssid}"
    else
      base_callsign
    end
  end

  @doc """
  Determines if a packet is a weather packet.
  """
  @spec weather_packet?(map()) :: boolean()
  def weather_packet?(packet) do
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

  def build_packet_data(packet, is_most_recent_for_callsign) when is_boolean(is_most_recent_for_callsign) do
    {lat, lon, data_extended} = MapHelpers.get_coordinates(packet)
    callsign = generate_callsign(packet)

    if lat && lon && callsign != "" && callsign != nil do
      packet_data = build_packet_map(packet, lat, lon, data_extended)
      Map.put(packet_data, "is_most_recent_for_callsign", is_most_recent_for_callsign)
    end
  end

  def build_packet_data(packet) do
    build_packet_data(packet, false)
  end

  defp build_packet_map(packet, lat, lon, data_extended) do
    data_extended = data_extended || %{}
    packet_info = extract_packet_info(packet, data_extended)
    popup = build_popup_content(packet, packet_info, lat, lon)

    build_packet_result(packet, packet_info, lat, lon, popup)
  end

  defp extract_packet_info(packet, data_extended) do
    %{
      callsign: get_packet_field(packet, :sender, ""),
      symbol_table_id: get_packet_field(packet, :symbol_table_id, "/"),
      symbol_code: get_packet_field(packet, :symbol_code, ">"),
      timestamp: get_timestamp(packet),
      comment: get_packet_field(packet, :comment, ""),
      safe_data_extended: convert_tuples_to_strings(data_extended),
      is_weather_packet: weather_packet?(packet)
    }
  end

  defp build_popup_content(packet, packet_info, lat, lon) do
    if packet_info.is_weather_packet do
      build_weather_popup_html(packet, packet_info.callsign)
    else
      build_standard_popup_html(packet_info, lat, lon)
    end
  end

  defp build_standard_popup_html(packet_info, lat, lon) do
    comment_html =
      if packet_info.comment == "",
        do: "",
        else: ~s(<div class="aprs-comment">#{packet_info.comment}</div>)

    timestamp_dt = TimeHelpers.to_datetime(packet_info.timestamp)

    timestamp_html =
      if timestamp_dt do
        """
        <div class="aprs-timestamp">
          <div>#{TimeHelpers.time_ago_in_words(timestamp_dt)}</div>
          <div class="text-slate-400">#{Calendar.strftime(timestamp_dt, "%Y-%m-%d %H:%M:%S UTC")}</div>
        </div>
        """
      else
        ""
      end

    """
    <div class="aprs-popup">
      <div class="aprs-callsign"><strong><a href="/#{packet_info.callsign}">#{packet_info.callsign}</a></strong></div>
      #{comment_html}
      <div class="aprs-coords">#{Float.round(to_float(lat), 4)}, #{Float.round(to_float(lon), 4)}</div>
      #{timestamp_html}
    </div>
    """
  end

  defp build_weather_popup_html(packet, sender) do
    received_at = get_received_at(packet)
    timestamp_dt = TimeHelpers.to_datetime(received_at)

    timestamp_html =
      if timestamp_dt do
        """
        <div class="aprs-timestamp" style="font-size: 11px; color: #6b7280; padding-top: 0; padding-bottom: 4px;">
          <div style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; font-size: 12px; line-height: 1.4;">#{TimeHelpers.time_ago_in_words(timestamp_dt)}</div>
          <div style="font-family: monospace;">#{Calendar.strftime(timestamp_dt, "%Y-%m-%d %H:%M:%S UTC")}</div>
        </div>
        """
      else
        ""
      end

    """
    <strong>#{sender} - Weather Report</strong>
    #{timestamp_html}
    <hr style="margin-top: 4px; margin-bottom: 4px;">
    Temperature: #{get_weather_field(packet, :temperature)}°F<br>
    Humidity: #{get_weather_field(packet, :humidity)}%<br>
    Wind: #{get_weather_field(packet, :wind_direction)}° at #{get_weather_field(packet, :wind_speed)} mph, gusts to #{get_weather_field(packet, :wind_gust)} mph<br>
    Pressure: #{get_weather_field(packet, :pressure)} hPa<br>
    Rain (1h): #{get_weather_field(packet, :rain_1h)} in.<br>
    Rain (24h): #{get_weather_field(packet, :rain_24h)} in.<br>
    Rain (since midnight): #{get_weather_field(packet, :rain_since_midnight)} in.<br>
    """
  end

  defp build_packet_result(packet, packet_info, lat, lon, popup) do
    # Generate unique ID for live packets to prevent conflicts
    packet_id =
      if Map.has_key?(packet, :id) do
        "live_#{packet.id}_#{System.unique_integer([:positive])}"
      else
        "live_#{packet_info.callsign}_#{System.unique_integer([:positive])}"
      end

    %{
      "id" => packet_id,
      "callsign" => packet_info.callsign,
      "base_callsign" => get_packet_field(packet, :base_callsign, ""),
      "ssid" => get_packet_field(packet, :ssid, nil),
      "lat" => to_float(lat),
      "lng" => to_float(lon),
      "data_type" => to_string(get_packet_field(packet, :data_type, "unknown")),
      "path" => get_packet_field(packet, :path, ""),
      "comment" => packet_info.comment,
      "data_extended" => packet_info.safe_data_extended || %{},
      "symbol_table_id" => packet_info.symbol_table_id,
      "symbol_code" => packet_info.symbol_code,
      "symbol_description" => "Symbol: #{packet_info.symbol_table_id}#{packet_info.symbol_code}",
      "timestamp" => packet_info.timestamp,
      "popup" => popup
    }
  end

  defp get_received_at(packet) do
    cond do
      Map.has_key?(packet, :received_at) -> packet.received_at
      Map.has_key?(packet, "received_at") -> packet["received_at"]
      true -> nil
    end
  end
end
