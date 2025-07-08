defmodule AprsmeWeb.MapLive.PacketUtils do
  @moduledoc """
  Shared utilities for extracting and processing packet data in map views.
  """

  alias AprsmeWeb.MapLive.MapHelpers
  alias AprsmeWeb.MapLive.PopupComponent
  alias AprsmeWeb.TimeHelpers
  alias Phoenix.HTML.Safe

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
    Aprsme.EncodingUtils.to_float(value) || 0.0
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
  Checks if a station has sent weather packets by looking at recent packets.
  """
  @spec has_weather_packets?(String.t()) :: boolean()
  # Get recent packets for this callsign and check if any are weather packets
  def has_weather_packets?(callsign) when is_binary(callsign) do
    %{callsign: callsign, limit: 10}
    |> Aprsme.Packets.get_recent_packets()
    |> Enum.any?(&weather_packet?/1)
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
    data_extended = Map.get(packet, :data_extended, Map.get(packet, "data_extended", %{})) || %{}

    Map.get(packet, key) ||
      Map.get(packet, to_string(key)) ||
      Map.get(data_extended, key) ||
      Map.get(data_extended, to_string(key)) || "N/A"
  end

  def build_packet_data(packet, is_most_recent_for_callsign, locale \\ "en")
      when is_boolean(is_most_recent_for_callsign) do
    {lat, lon, data_extended} = MapHelpers.get_coordinates(packet)
    callsign = generate_callsign(packet)

    if lat && lon && callsign != "" && callsign != nil do
      packet_data = build_packet_map(packet, lat, lon, data_extended, locale)
      Map.put(packet_data, "is_most_recent_for_callsign", is_most_recent_for_callsign)
    end
  end

  def build_packet_data(packet) do
    build_packet_data(packet, false, "en")
  end

  defp build_packet_map(packet, lat, lon, data_extended, locale) do
    data_extended = data_extended || %{}
    packet_info = extract_packet_info(packet, data_extended)
    popup = build_popup_content(packet, packet_info, lat, lon, locale)

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

  defp build_popup_content(packet, packet_info, lat, lon, locale) do
    if packet_info.is_weather_packet do
      build_weather_popup_html(packet, packet_info.callsign, locale)
    else
      build_standard_popup_html(packet_info, lat, lon)
    end
  end

  defp build_standard_popup_html(packet_info, _lat, _lon) do
    timestamp_dt = TimeHelpers.to_datetime(packet_info.timestamp)
    cache_buster = System.system_time(:millisecond)
    weather_link = has_weather_packets?(packet_info.callsign)

    %{
      callsign: packet_info.callsign,
      comment: packet_info.comment,
      timestamp_dt: timestamp_dt,
      cache_buster: cache_buster,
      weather: false,
      weather_link: weather_link
    }
    |> PopupComponent.popup()
    |> Safe.to_iodata()
    |> IO.iodata_to_binary()
  end

  defp build_weather_popup_html(packet, sender, locale) do
    received_at = get_received_at(packet)
    timestamp_dt = TimeHelpers.to_datetime(received_at)
    cache_buster = System.system_time(:millisecond)

    # Get weather data and convert units based on locale
    temperature_raw = get_weather_field(packet, :temperature)
    wind_speed_raw = get_weather_field(packet, :wind_speed)
    wind_gust_raw = get_weather_field(packet, :wind_gust)
    rain_1h_raw = get_weather_field(packet, :rain_1h)
    rain_24h_raw = get_weather_field(packet, :rain_24h)
    rain_since_midnight_raw = get_weather_field(packet, :rain_since_midnight)

    # Convert units if the values are numbers
    {temperature, temp_unit} = convert_temperature(temperature_raw, locale)
    {wind_speed, wind_unit} = convert_wind_speed(wind_speed_raw, locale)
    {wind_gust, gust_unit} = convert_wind_speed(wind_gust_raw, locale)
    {rain_1h, rain_1h_unit} = convert_rain(rain_1h_raw, locale)
    {rain_24h, rain_24h_unit} = convert_rain(rain_24h_raw, locale)
    {rain_since_midnight, rain_since_midnight_unit} = convert_rain(rain_since_midnight_raw, locale)

    %{
      callsign: sender,
      comment: nil,
      timestamp_dt: timestamp_dt,
      cache_buster: cache_buster,
      weather: true,
      weather_link: true,
      temperature: temperature,
      temp_unit: temp_unit,
      humidity: get_weather_field(packet, :humidity),
      wind_direction: get_weather_field(packet, :wind_direction),
      wind_speed: wind_speed,
      wind_unit: wind_unit,
      wind_gust: wind_gust,
      gust_unit: gust_unit,
      pressure: get_weather_field(packet, :pressure),
      rain_1h: rain_1h,
      rain_24h: rain_24h,
      rain_since_midnight: rain_since_midnight,
      rain_1h_unit: rain_1h_unit,
      rain_24h_unit: rain_24h_unit,
      rain_since_midnight_unit: rain_since_midnight_unit
    }
    |> PopupComponent.popup()
    |> Safe.to_iodata()
    |> IO.iodata_to_binary()
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

  # Weather unit conversion helpers
  defp convert_temperature(value, locale) when is_binary(value) and value != "N/A" do
    case Float.parse(value) do
      {num_value, _} ->
        AprsmeWeb.WeatherUnits.format_temperature(num_value, locale)

      :error ->
        {value, "°F"}
    end
  end

  defp convert_temperature(value, _locale), do: {value, "°F"}

  defp convert_wind_speed(value, locale) when is_binary(value) and value != "N/A" do
    case Float.parse(value) do
      {num_value, _} ->
        AprsmeWeb.WeatherUnits.format_wind_speed(num_value, locale)

      :error ->
        {value, "mph"}
    end
  end

  defp convert_wind_speed(value, _locale), do: {value, "mph"}

  defp convert_rain(value, locale) when is_binary(value) and value != "N/A" do
    case Float.parse(value) do
      {num_value, _} ->
        AprsmeWeb.WeatherUnits.format_rain(num_value, locale)

      :error ->
        {value, "in"}
    end
  end

  defp convert_rain(value, _locale), do: {value, "in"}
end
