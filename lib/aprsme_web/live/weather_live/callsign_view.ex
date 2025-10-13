defmodule AprsmeWeb.WeatherLive.CallsignView do
  @moduledoc false
  use AprsmeWeb, :live_view
  use Gettext, backend: AprsmeWeb.Gettext

  import Phoenix.LiveView, only: [push_event: 3, connected?: 1]

  alias Aprsme.Callsign
  alias AprsmeWeb.MapLive.PacketUtils
  alias AprsmeWeb.TimeUtils
  alias AprsmeWeb.WeatherUnits

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = Callsign.normalize(callsign)

    # Subscribe to weather-specific topic for targeted updates
    if connected?(socket) do
      weather_topic = "weather:#{normalized_callsign}"
      Phoenix.PubSub.subscribe(Aprsme.PubSub, weather_topic)
    end

    weather_packet = get_latest_weather_packet(normalized_callsign)
    {start_time, end_time} = default_time_range()
    weather_history = get_weather_history(normalized_callsign, start_time, end_time)

    # Get locale from socket assigns (set by LocaleHook)
    locale = Map.get(socket.assigns, :locale, "en")
    unit_labels = WeatherUnits.unit_labels(locale)

    weather_history_json = convert_weather_history_to_json(weather_history, locale)

    # Add chart labels for translation with locale-aware units
    chart_labels = %{
      temp_title: gettext("Temperature & Dew Point (%{unit})", unit: unit_labels.temperature),
      temp_label: gettext("Temperature (%{unit})", unit: unit_labels.temperature),
      dew_label: gettext("Dew Point (%{unit})", unit: unit_labels.temperature),
      humidity_title: gettext("Humidity (%)"),
      humidity_label: gettext("Humidity (%)"),
      pressure_title: gettext("Pressure (mb)"),
      pressure_label: gettext("Pressure (mb)"),
      wind_title: gettext("Wind (%{unit})", unit: unit_labels.wind_speed),
      wind_label: gettext("Wind Speed (%{unit})", unit: unit_labels.wind_speed),
      gust_label: gettext("Wind Gust (%{unit})", unit: unit_labels.wind_speed),
      time: gettext("Time"),
      degf: unit_labels.temperature,
      percent: gettext("%"),
      mb: gettext("mb")
    }

    socket =
      socket
      |> assign(:callsign, normalized_callsign)
      |> assign(:weather_packet, weather_packet)
      |> assign(:page_title, "Weather for #{normalized_callsign}")
      |> assign(:weather_history, weather_history)
      |> assign(:weather_history_json, weather_history_json)
      |> assign(:chart_labels, chart_labels)

    {:ok, socket}
  end

  @impl true
  def handle_info({:weather_packet, packet}, socket) do
    # Check if this packet is actually newer than what we have
    current_packet = socket.assigns.weather_packet

    if should_update_weather?(current_packet, packet) do
      update_weather_data(socket)
    else
      {:noreply, socket}
    end
  end

  # Keep the old handler for backward compatibility
  def handle_info({:postgres_packet, packet}, socket) do
    # Only process if it's a packet with weather data for this callsign
    has_weather_data =
      not is_nil(packet.temperature) or not is_nil(packet.humidity) or
        not is_nil(packet.pressure) or not is_nil(packet.wind_speed) or
        not is_nil(packet.wind_direction) or not is_nil(packet.rain_1h)

    if packet.sender == socket.assigns.callsign && has_weather_data do
      handle_info({:weather_packet, packet}, socket)
    else
      {:noreply, socket}
    end
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp should_update_weather?(nil, _new_packet), do: true

  defp should_update_weather?(current_packet, new_packet) do
    # Only update if the new packet is actually newer
    # Handle both atom and string keys
    new_received_at = Map.get(new_packet, :received_at) || Map.get(new_packet, "received_at")
    current_received_at = Map.get(current_packet, :received_at) || Map.get(current_packet, "received_at")

    case {new_received_at, current_received_at} do
      {nil, _} ->
        false

      {_, nil} ->
        true

      {new_time, current_time} when is_binary(new_time) and is_binary(current_time) ->
        new_time > current_time

      {new_time, current_time} when is_binary(new_time) ->
        # Parse string timestamp and compare
        case DateTime.from_iso8601(new_time <> "Z") do
          {:ok, new_dt, _} -> DateTime.after?(new_dt, current_time)
          _ -> false
        end

      {new_time, current_time} when is_binary(current_time) ->
        # Parse string timestamp and compare
        case DateTime.from_iso8601(current_time <> "Z") do
          {:ok, current_dt, _} -> DateTime.after?(new_time, current_dt)
          _ -> true
        end

      {new_time, current_time} ->
        DateTime.after?(new_time, current_time)
    end
  end

  defp update_weather_data(socket) do
    # Fetch updated data only once
    weather_packet = get_latest_weather_packet(socket.assigns.callsign)
    {start_time, end_time} = default_time_range()
    weather_history = get_weather_history(socket.assigns.callsign, start_time, end_time)

    # Get locale from socket assigns
    locale = Map.get(socket.assigns, :locale, "en")

    weather_history_json = convert_weather_history_to_json(weather_history, locale)

    socket =
      socket
      |> assign(:weather_packet, weather_packet)
      |> assign(:weather_history, weather_history)
      |> assign(:weather_history_json, weather_history_json)

    # Push event to update all charts with new data
    socket = push_event(socket, "update_weather_charts", %{weather_history: weather_history_json})

    {:noreply, socket}
  end

  defp get_latest_weather_packet(callsign) do
    # Use optimized cached query that checks recent data first
    Aprsme.Packets.get_latest_weather_packet(callsign)
  end

  defp get_weather_history(callsign, start_time, end_time) do
    # Use cached queries to avoid repeated database hits
    Aprsme.Packets.get_weather_packets(callsign, start_time, end_time, %{limit: 500})
  end

  defp default_time_range do
    TimeUtils.time_range(:last_two_days)
  end

  defp calc_dew_point(temp, humidity) when is_number(temp) and is_number(humidity) do
    temp - (100 - humidity) / 5
  end

  defp calc_dew_point(_, _), do: nil

  defp convert_weather_history_to_json(weather_history, locale) do
    weather_history
    |> Enum.map(fn pkt ->
      dew_point =
        if is_number(pkt.temperature) and is_number(pkt.humidity) do
          calc_dew_point(pkt.temperature, pkt.humidity)
        end

      # Convert units based on locale
      {temp_value, _} = WeatherUnits.format_temperature(pkt.temperature, locale)
      {dew_value, _} = if dew_point, do: WeatherUnits.format_temperature(dew_point, locale), else: {nil, nil}
      {wind_speed_value, _} = WeatherUnits.format_wind_speed(pkt.wind_speed, locale)
      {rain_1h_value, _} = WeatherUnits.format_rain(pkt.rain_1h, locale)
      {rain_24h_value, _} = WeatherUnits.format_rain(pkt.rain_24h, locale)
      {rain_since_midnight_value, _} = WeatherUnits.format_rain(pkt.rain_since_midnight, locale)

      %{
        timestamp: pkt.received_at,
        temperature: temp_value,
        dew_point: dew_value,
        humidity: pkt.humidity,
        pressure: pkt.pressure,
        wind_direction: pkt.wind_direction,
        wind_speed: wind_speed_value,
        rain_1h: rain_1h_value,
        rain_24h: rain_24h_value,
        rain_since_midnight: rain_since_midnight_value,
        luminosity: pkt.luminosity
      }
    end)
    |> Jason.encode!()
  end

  @doc """
  Gets weather field value, returning "0" instead of "N/A" for missing numeric data.
  """
  def get_weather_field_zero(packet, key) do
    value = PacketUtils.get_weather_field(packet, key)

    # Return "0" for missing numeric fields, keep other values as-is
    case value do
      "N/A" -> "0"
      _ -> value
    end
  end

  # Weather formatters mapping
  @weather_formatters %{
    temperature: {&WeatherUnits.format_temperature/2, ""},
    wind_speed: {&WeatherUnits.format_wind_speed/2, " "},
    wind_gust: {&WeatherUnits.format_wind_speed/2, " "},
    rain_1h: {&WeatherUnits.format_rain/2, " "},
    rain_24h: {&WeatherUnits.format_rain/2, " "},
    rain_since_midnight: {&WeatherUnits.format_rain/2, " "}
  }

  @doc """
  Checks if a weather field has a valid value (not nil, "N/A", or empty).
  """
  def has_weather_field?(packet, field) do
    value = PacketUtils.get_weather_field(packet, field)

    case value do
      "N/A" -> false
      "" -> false
      nil -> false
      _ -> true
    end
  end

  @doc """
  Formats weather values with appropriate units based on locale.
  """
  def format_weather_value(packet, key, locale) do
    value = PacketUtils.get_weather_field(packet, key)

    case value do
      "N/A" ->
        nil

      nil ->
        nil

      value when is_binary(value) ->
        case @weather_formatters[key] do
          {formatter, separator} ->
            {converted_value, unit} = formatter.(value, locale)
            "#{converted_value}#{separator}#{unit}"

          nil ->
            "#{value}"
        end

        case @weather_formatters[key] do
          {formatter, separator} ->
            case Float.parse(value) do
              {num_value, _} ->
                {converted_value, unit} = formatter.(num_value, locale)
                "#{converted_value}#{separator}#{unit}"

              :error ->
                nil
            end

          nil ->
            "#{value}"
        end

      value when is_number(value) ->
        case @weather_formatters[key] do
          {formatter, separator} ->
            {converted_value, unit} = formatter.(value, locale)
            "#{converted_value}#{separator}#{unit}"

          nil ->
            "#{value}"
        end
    end
  end
end
