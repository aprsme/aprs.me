defmodule AprsmeWeb.MapLive.DataBuilder do
  @moduledoc """
  Centralizes all packet data building logic for map display.

  This module consolidates data building functions that were previously 
  scattered across multiple modules (index.ex, display_manager.ex, 
  packet_utils.ex, and historical_loader.ex).

  The refactoring eliminates code duplication and provides a single 
  source of truth for:
  - Building packet data for real-time display
  - Building minimal packet data for historical display
  - Creating popup content for both weather and standard packets
  - Converting packet data structures for frontend consumption

  All functions handle both real-time and historical packet data with
  proper coordinate validation, symbol rendering, and weather data processing.
  """

  alias AprsmeWeb.Live.Shared.CoordinateUtils
  alias AprsmeWeb.Live.Shared.PacketUtils, as: SharedPacketUtils
  alias AprsmeWeb.Live.Shared.ParamUtils
  alias AprsmeWeb.MapLive.MapHelpers
  alias AprsmeWeb.MapLive.PopupComponent
  alias AprsmeWeb.TimeHelpers
  alias Phoenix.HTML.Safe
  alias Phoenix.LiveView.Socket

  @doc """
  Build packet data list from a map of packets.
  Replaces the duplicated function in both index.ex and display_manager.ex.
  """
  @spec build_packet_data_list_from_map(map(), boolean(), Socket.t()) :: list()
  def build_packet_data_list_from_map(packets_map, is_most_recent, socket) do
    locale = get_locale(socket)

    packets_map
    |> Enum.map(fn {_callsign, packet} ->
      build_packet_data(packet, is_most_recent, locale)
    end)
    |> Enum.filter(& &1)
  end

  @doc """
  Build packet data for display on the map.
  Main function moved from packet_utils.ex.
  """
  @spec build_packet_data(map(), boolean(), String.t()) :: map() | nil
  def build_packet_data(packet, is_most_recent_for_callsign, locale) when is_boolean(is_most_recent_for_callsign) do
    {lat, lon, data_extended} = MapHelpers.get_coordinates(packet)
    callsign = generate_callsign(packet)

    # Validate coordinates and callsign before building packet data
    if valid_coordinates?(lat, lon) && callsign != "" && callsign != nil do
      packet_data = build_packet_map(packet, to_float(lat), to_float(lon), data_extended, locale)
      Map.put(packet_data, "is_most_recent_for_callsign", is_most_recent_for_callsign)
    else
      # Log invalid data for debugging
      if not valid_coordinates?(lat, lon) do
        require Logger

        Logger.debug("Invalid coordinates in packet #{get_packet_id(packet)}: lat=#{inspect(lat)}, lon=#{inspect(lon)}")
      end

      nil
    end
  end

  @doc """
  Build packet data with default locale.
  """
  @spec build_packet_data(map(), boolean()) :: map() | nil
  def build_packet_data(packet, is_most_recent_for_callsign) do
    build_packet_data(packet, is_most_recent_for_callsign, "en")
  end

  @doc """
  Build packet data with default parameters.
  """
  @spec build_packet_data(map()) :: map() | nil
  def build_packet_data(packet) do
    build_packet_data(packet, false, "en")
  end

  @doc """
  Build minimal packet data for historical display.
  Moved from historical_loader.ex.
  """
  @spec build_minimal_packet_data(map(), boolean(), boolean()) :: map() | nil
  def build_minimal_packet_data(packet, is_most_recent, has_weather) do
    # Build minimal packet data without calling expensive build_packet_data
    {lat, lon, _} = get_coordinates(packet)

    # Validate coordinates are numeric and within valid ranges
    if valid_coordinates?(lat, lon) do
      # Use get_packet_field to get symbol information properly (includes data_extended fallback)
      symbol_table_id = get_packet_field(packet, :symbol_table_id, "/")
      symbol_code = get_packet_field(packet, :symbol_code, ">")

      # Generate symbol HTML using the SymbolRenderer
      symbol_html =
        AprsmeWeb.SymbolRenderer.render_marker_symbol(
          symbol_table_id,
          symbol_code,
          get_packet_field(packet, :sender, ""),
          32
        )

      %{
        "id" => if(is_most_recent, do: "current_#{get_packet_id(packet)}", else: "hist_#{get_packet_id(packet)}"),
        "lat" => to_float(lat),
        "lng" => to_float(lon),
        "callsign" => get_packet_field(packet, :sender, ""),
        "symbol_table_id" => symbol_table_id,
        "symbol_code" => symbol_code,
        "symbol_html" => symbol_html,
        "comment" => get_packet_field(packet, :comment, ""),
        "timestamp" => get_packet_timestamp_unix(packet),
        "historical" => !is_most_recent,
        "is_most_recent_for_callsign" => is_most_recent,
        "path" => get_packet_field(packet, :path, ""),
        "popup" => build_simple_popup(packet, has_weather)
      }
    else
      # Log invalid coordinates for debugging
      require Logger

      Logger.debug("Invalid coordinates in packet #{get_packet_id(packet)}: lat=#{inspect(lat)}, lon=#{inspect(lon)}")
      nil
    end
  end

  # Validate coordinates are numeric and within valid ranges
  @spec valid_coordinates?(any(), any()) :: boolean()
  defp valid_coordinates?(lat, lon) when is_number(lat) and is_number(lon) do
    lat >= -90 and lat <= 90 and lon >= -180 and lon <= 180 and
      :math.is_finite(lat) and :math.is_finite(lon)
  end

  defp valid_coordinates?(_, _), do: false

  @doc """
  Build packet data list for historical display.
  Moved from historical_loader.ex.
  """
  @spec build_packet_data_list(list()) :: list()
  def build_packet_data_list(historical_packets) do
    # Include weather data in initial grouping to avoid separate query
    grouped_packets =
      Enum.group_by(historical_packets, fn packet ->
        get_packet_field(packet, :sender, "unknown")
      end)

    # Build weather callsign set from packets themselves (no DB query needed)
    weather_callsigns = build_weather_callsign_set(historical_packets)

    # For each callsign group, find the most recent packet and mark it appropriately
    grouped_packets
    |> Enum.flat_map(fn {callsign, packets} ->
      # Sort by received_at to find most recent
      sorted_packets = Enum.sort_by(packets, &get_packet_received_at(&1), {:desc, DateTime})

      case sorted_packets do
        [] ->
          []

        packets_list ->
          # Find the best packet to display as "current" - prioritize position over weather
          selected_packet = select_best_packet_for_display(packets_list)
          historical = Enum.reject(packets_list, &(get_packet_id(&1) == get_packet_id(selected_packet)))

          # Always include the selected packet
          has_weather = MapSet.member?(weather_callsigns, String.upcase(callsign))
          most_recent_data = build_minimal_packet_data(selected_packet, true, has_weather)

          # Get coordinates of selected packet for distance filtering
          {most_recent_lat, most_recent_lon, _} = get_coordinates(selected_packet)

          # Filter historical packets that are too close to most recent position
          filtered_historical =
            if most_recent_lat && most_recent_lon do
              Enum.filter(historical, fn packet ->
                {lat, lon, _} = get_coordinates(packet)

                if lat && lon do
                  distance_meters =
                    CoordinateUtils.calculate_distance_meters(
                      most_recent_lat,
                      most_recent_lon,
                      lat,
                      lon
                    )

                  # Only show if 10+ meters away
                  distance_meters >= 10.0
                else
                  # Skip packets without coordinates
                  false
                end
              end)
            else
              # If most recent has no coordinates, include all historical
              historical
            end

          # Build data for remaining historical packets
          historical_data =
            filtered_historical
            |> Enum.map(fn packet -> build_minimal_packet_data(packet, false, has_weather) end)
            |> Enum.filter(& &1)

          # Combine most recent and filtered historical
          Enum.filter([most_recent_data | historical_data], & &1)
      end
    end)
    |> Enum.filter(& &1)
  end

  @doc """
  Build simple popup content for historical display.
  Moved from historical_loader.ex.
  """
  @spec build_simple_popup(map(), boolean()) :: String.t()
  def build_simple_popup(packet, has_weather) do
    # Build popup HTML directly without database queries
    callsign = get_packet_field(packet, :sender, "Unknown")
    timestamp_dt = get_packet_received_at(packet)
    cache_buster = System.system_time(:millisecond)

    # Check if this packet itself is a weather packet
    is_weather = weather_packet?(packet)

    if is_weather do
      # Build weather popup
      %{
        callsign: callsign,
        comment: nil,
        timestamp_dt: timestamp_dt,
        cache_buster: cache_buster,
        weather: true,
        weather_link: true,
        temperature: get_weather_field(packet, :temperature),
        temp_unit: "°F",
        humidity: get_weather_field(packet, :humidity),
        wind_direction: get_weather_field(packet, :wind_direction),
        wind_speed: get_weather_field(packet, :wind_speed),
        wind_unit: "mph",
        wind_gust: get_weather_field(packet, :wind_gust),
        gust_unit: "mph",
        pressure: get_weather_field(packet, :pressure),
        rain_1h: get_weather_field(packet, :rain_1h),
        rain_24h: get_weather_field(packet, :rain_24h),
        rain_since_midnight: get_weather_field(packet, :rain_since_midnight),
        rain_1h_unit: "in",
        rain_24h_unit: "in",
        rain_since_midnight_unit: "in"
      }
      |> PopupComponent.popup()
      |> Safe.to_iodata()
      |> IO.iodata_to_binary()
    else
      # Build standard popup
      %{
        callsign: callsign,
        comment: get_packet_field(packet, :comment, ""),
        timestamp_dt: timestamp_dt,
        cache_buster: cache_buster,
        weather: false,
        # Use pre-fetched weather info
        weather_link: has_weather
      }
      |> PopupComponent.popup()
      |> Safe.to_iodata()
      |> IO.iodata_to_binary()
    end
  end

  @doc """
  Select the best packet to display for a callsign.
  Moved from historical_loader.ex.
  """
  @spec select_best_packet_for_display(list()) :: map()
  def select_best_packet_for_display(packets) do
    # Separate position and weather packets using the same logic as weather_packet?
    {position_packets, weather_packets} =
      Enum.split_with(packets, fn packet ->
        # A packet is a position packet if it's NOT a weather packet
        not weather_packet?(packet)
      end)

    # Prefer the most recent position packet, fall back to most recent weather packet
    case position_packets do
      [] ->
        # No position packets, use most recent weather packet
        hd(weather_packets)

      [single_position] ->
        # Only one position packet, use it
        single_position

      position_list ->
        # Multiple position packets, use most recent one
        Enum.max_by(position_list, &get_packet_received_at(&1), DateTime)
    end
  end

  @doc """
  Build weather callsign set from packets.
  Moved from historical_loader.ex.
  """
  @spec build_weather_callsign_set(list()) :: MapSet.t()
  def build_weather_callsign_set(packets) do
    packets
    |> Enum.filter(&weather_packet?/1)
    |> MapSet.new(fn packet -> String.upcase(get_packet_field(packet, :sender, "")) end)
  end

  # Private functions moved from packet_utils.ex

  @spec build_packet_map(map(), float(), float(), map(), String.t()) :: map()
  defp build_packet_map(packet, lat, lon, data_extended, locale) do
    data_extended = data_extended || %{}
    packet_info = extract_packet_info(packet, data_extended)
    popup = build_popup_content(packet, packet_info, lat, lon, locale)

    build_packet_result(packet, packet_info, lat, lon, popup)
  end

  @spec extract_packet_info(map(), map()) :: map()
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

  @spec build_popup_content(map(), map(), float(), float(), String.t()) :: String.t()
  defp build_popup_content(packet, packet_info, lat, lon, locale) do
    if packet_info.is_weather_packet do
      build_weather_popup_html(packet, packet_info.callsign, locale)
    else
      build_standard_popup_html(packet_info, lat, lon)
    end
  end

  @spec build_standard_popup_html(map(), float(), float()) :: String.t()
  defp build_standard_popup_html(packet_info, _lat, _lon) do
    timestamp_dt = TimeHelpers.to_datetime(packet_info.timestamp)
    cache_buster = System.system_time(:millisecond)
    weather_link = has_weather_packets?(packet_info.callsign)

    popup_data = %{
      callsign: packet_info.callsign,
      comment: packet_info.comment,
      timestamp_dt: timestamp_dt,
      cache_buster: cache_buster,
      weather: false,
      weather_link: weather_link
    }

    # Optimize string conversion - avoid intermediate iodata step
    popup_data
    |> PopupComponent.popup()
    |> Safe.to_iodata()
    |> IO.iodata_to_binary()
  end

  @spec build_weather_popup_html(map(), String.t(), String.t()) :: String.t()
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

  @spec build_packet_result(map(), map(), float(), float(), String.t()) :: map()
  defp build_packet_result(packet, packet_info, lat, lon, popup) do
    # Generate unique ID for live packets to prevent conflicts
    packet_id =
      if Map.has_key?(packet, :id) do
        "live_#{packet.id}_#{System.unique_integer([:positive])}"
      else
        "live_#{packet_info.callsign}_#{System.unique_integer([:positive])}"
      end

    # Generate symbol HTML using the server-side renderer
    symbol_html =
      AprsmeWeb.SymbolRenderer.render_marker_symbol(
        packet_info.symbol_table_id,
        packet_info.symbol_code,
        packet_info.callsign,
        32
      )

    path_value = get_packet_field(packet, :path, "")

    %{
      "id" => packet_id,
      "callsign" => packet_info.callsign,
      "base_callsign" => get_packet_field(packet, :base_callsign, ""),
      "ssid" => get_packet_field(packet, :ssid, nil),
      "lat" => to_float(lat),
      "lng" => to_float(lon),
      "data_type" => to_string(get_packet_field(packet, :data_type, "unknown")),
      "path" => path_value,
      "comment" => packet_info.comment,
      "data_extended" => packet_info.safe_data_extended || %{},
      "symbol_table_id" => packet_info.symbol_table_id,
      "symbol_code" => packet_info.symbol_code,
      "symbol_description" => "Symbol: #{packet_info.symbol_table_id}#{packet_info.symbol_code}",
      "timestamp" => packet_info.timestamp,
      "popup" => popup,
      "symbol_html" => symbol_html
    }
  end

  # Utility functions for packet field access

  @spec get_packet_field(map(), atom() | String.t(), any()) :: any()
  defp get_packet_field(packet, field, default) do
    SharedPacketUtils.get_packet_field(packet, field, default)
  end

  @spec get_timestamp(map()) :: String.t()
  defp get_timestamp(packet) do
    SharedPacketUtils.get_timestamp(packet)
  end

  @spec to_float(any()) :: float()
  defp to_float(value) do
    ParamUtils.to_float(value)
  end

  @spec generate_callsign(map()) :: String.t()
  defp generate_callsign(packet) do
    SharedPacketUtils.generate_callsign(packet)
  end

  @spec weather_packet?(map()) :: boolean()
  defp weather_packet?(packet) do
    SharedPacketUtils.weather_packet?(packet)
  end

  @spec has_weather_packets?(String.t()) :: boolean()
  defp has_weather_packets?(callsign) when is_binary(callsign) do
    Aprsme.Packets.has_weather_packets?(callsign)
  rescue
    _ -> false
  end

  defp has_weather_packets?(_), do: false

  @spec convert_tuples_to_strings(any()) :: any()
  defp convert_tuples_to_strings(map) when is_map(map) do
    # Avoid processing structs entirely
    case map do
      %{__struct__: _} -> map
      _ -> Map.new(map, &convert_tuple_entry/1)
    end
  end

  defp convert_tuples_to_strings(list) when is_list(list) do
    # Use Stream for memory efficiency on large lists
    list
    |> Stream.map(&convert_tuples_to_strings/1)
    |> Enum.to_list()
  end

  defp convert_tuples_to_strings(tuple) when is_tuple(tuple) do
    # Optimize tuple string conversion - avoid inspect for common cases
    case tuple_size(tuple) do
      2 ->
        {a, b} = tuple
        "#{a}, #{b}"

      3 ->
        {a, b, c} = tuple
        "#{a}, #{b}, #{c}"

      _ ->
        inspect(tuple)
    end
  end

  defp convert_tuples_to_strings(other), do: other

  defp convert_tuple_entry({k, v}), do: {k, convert_tuples_to_strings(v)}

  @spec get_weather_field(map(), atom()) :: String.t()
  defp get_weather_field(packet, key) do
    case SharedPacketUtils.get_weather_field(packet, key) do
      nil -> "N/A"
      value -> value
    end
  end

  @spec get_received_at(map()) :: DateTime.t() | nil
  defp get_received_at(packet) do
    cond do
      Map.has_key?(packet, :received_at) -> packet.received_at
      Map.has_key?(packet, "received_at") -> packet["received_at"]
      true -> nil
    end
  end

  # Helper to get locale from socket
  @spec get_locale(Socket.t()) :: String.t()
  defp get_locale(socket) do
    Map.get(socket.assigns, :locale, "en")
  end

  # Helper to get coordinates from packet
  @spec get_coordinates(map()) :: {float() | nil, float() | nil, map()}
  defp get_coordinates(packet) do
    CoordinateUtils.get_coordinates(packet)
  end

  # Helper to get packet ID
  @spec get_packet_id(map()) :: any()
  defp get_packet_id(packet) do
    Map.get(packet, :id) || Map.get(packet, "id")
  end

  # Helper to get packet received_at datetime
  @spec get_packet_received_at(map()) :: DateTime.t()
  defp get_packet_received_at(packet) do
    get_received_at(packet) || DateTime.utc_now()
  end

  # Helper to get packet timestamp as unix milliseconds
  @spec get_packet_timestamp_unix(map()) :: integer()
  defp get_packet_timestamp_unix(packet) do
    received_at = get_packet_received_at(packet)
    DateTime.to_unix(received_at, :millisecond)
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
