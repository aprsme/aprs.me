defmodule AprsmeWeb.WeatherLive.CallsignView do
  @moduledoc false
  use AprsmeWeb, :live_view
  use Gettext, backend: AprsmeWeb.Gettext

  import Phoenix.LiveView, only: [push_event: 3, connected?: 1]

  alias Aprsme.Packets
  alias AprsmeWeb.MapLive.PacketUtils
  alias AprsmeWeb.WeatherUnits

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = String.upcase(String.trim(callsign))

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

    weather_history_json =
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
  def handle_info({:weather_packet, _packet}, socket) do
    # Update weather data when new weather packet arrives for this callsign
    weather_packet = get_latest_weather_packet(socket.assigns.callsign)
    {start_time, end_time} = default_time_range()
    weather_history = get_weather_history(socket.assigns.callsign, start_time, end_time)

    # Get locale from socket assigns
    locale = Map.get(socket.assigns, :locale, "en")

    weather_history_json =
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

    socket =
      socket
      |> assign(:weather_packet, weather_packet)
      |> assign(:weather_history, weather_history)
      |> assign(:weather_history_json, weather_history_json)

    # Push event to update all charts with new data
    socket = push_event(socket, "update_weather_charts", %{weather_history: weather_history_json})

    {:noreply, socket}
  end

  # Keep the old handler for backward compatibility
  def handle_info({:postgres_packet, packet}, socket) do
    # Only update if the packet is for our callsign and contains weather data
    if packet_matches_callsign?(packet, socket.assigns.callsign) and has_weather_data?(packet) do
      # Refresh weather data when new weather packet arrives
      weather_packet = get_latest_weather_packet(socket.assigns.callsign)
      {start_time, end_time} = default_time_range()
      weather_history = get_weather_history(socket.assigns.callsign, start_time, end_time)

      # Get locale from socket assigns
      locale = Map.get(socket.assigns, :locale, "en")

      weather_history_json =
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

      socket =
        socket
        |> assign(:weather_packet, weather_packet)
        |> assign(:weather_history, weather_history)
        |> assign(:weather_history_json, weather_history_json)

      # Push event to update all charts with new data
      socket = push_event(socket, "update_weather_charts", %{weather_history: weather_history_json})

      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp packet_matches_callsign?(packet, callsign) do
    packet_sender = Map.get(packet, "sender") || Map.get(packet, :sender, "")
    String.upcase(packet_sender) == String.upcase(callsign)
  end

  defp has_weather_data?(packet) do
    # Check if packet contains any weather-related fields
    Enum.any?(Aprsme.EncodingUtils.weather_fields(), fn field ->
      value = Map.get(packet, field) || Map.get(packet, to_string(field))
      not is_nil(value)
    end)
  end

  defp get_latest_weather_packet(callsign) do
    # Get weather packets from the last 7 days to find the most recent one
    end_time = DateTime.utc_now()
    start_time = DateTime.add(end_time, -7 * 24 * 3600, :second)

    callsign
    |> Packets.get_weather_packets(start_time, end_time, %{limit: 1})
    |> List.first()
  end

  defp get_weather_history(callsign, start_time, end_time) do
    Packets.get_weather_packets(callsign, start_time, end_time, %{limit: 500})
  end

  defp default_time_range do
    now = DateTime.utc_now()
    {DateTime.add(now, -24 * 3600, :second), now}
  end

  defp calc_dew_point(temp, humidity) when is_number(temp) and is_number(humidity) do
    temp - (100 - humidity) / 5
  end

  defp calc_dew_point(_, _), do: nil

  @doc """
  Gets weather field value, returning "0" instead of "N/A" for missing numeric data.
  """
  def get_weather_field_zero(packet, key) do
    value = PacketUtils.get_weather_field(packet, key)

    # Return "0" for missing numeric fields, keep other values as-is
    case value do
      "N/A" -> "0"
      nil -> "0"
      _ -> value
    end
  end

  @doc """
  Formats weather values with appropriate units based on locale.
  """
  def format_weather_value(packet, key, locale) do
    value = PacketUtils.get_weather_field(packet, key)

    case {key, value} do
      {:temperature, value} when is_number(value) ->
        {converted_value, unit} = WeatherUnits.format_temperature(value, locale)
        "#{converted_value}#{unit}"

      {:wind_speed, value} when is_number(value) ->
        {converted_value, unit} = WeatherUnits.format_wind_speed(value, locale)
        "#{converted_value} #{unit}"

      {:wind_gust, value} when is_number(value) ->
        {converted_value, unit} = WeatherUnits.format_wind_speed(value, locale)
        "#{converted_value} #{unit}"

      {:rain_1h, value} when is_number(value) ->
        {converted_value, unit} = WeatherUnits.format_rain(value, locale)
        "#{converted_value} #{unit}"

      {:rain_24h, value} when is_number(value) ->
        {converted_value, unit} = WeatherUnits.format_rain(value, locale)
        "#{converted_value} #{unit}"

      {:rain_since_midnight, value} when is_number(value) ->
        {converted_value, unit} = WeatherUnits.format_rain(value, locale)
        "#{converted_value} #{unit}"

      _ ->
        case value do
          "N/A" -> "N/A"
          nil -> "N/A"
          _ -> "#{value}"
        end
    end
  end
end
