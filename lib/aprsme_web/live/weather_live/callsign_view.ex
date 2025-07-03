defmodule AprsmeWeb.WeatherLive.CallsignView do
  @moduledoc false
  use AprsmeWeb, :live_view

  import Jason

  alias Aprsme.Packets
  alias AprsmeWeb.MapLive.PacketUtils

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = String.upcase(String.trim(callsign))
    weather_packet = get_latest_weather_packet(normalized_callsign)
    {start_time, end_time} = default_time_range()
    weather_history = get_weather_history(normalized_callsign, start_time, end_time)

    weather_history_json =
      weather_history
      |> Enum.map(fn pkt ->
        dew_point =
          if is_number(pkt.temperature) and is_number(pkt.humidity) do
            calc_dew_point(pkt.temperature, pkt.humidity)
          end

        %{
          timestamp: pkt.received_at,
          temperature: pkt.temperature,
          dew_point: dew_point,
          humidity: pkt.humidity,
          pressure: pkt.pressure,
          wind_direction: pkt.wind_direction,
          wind_speed: pkt.wind_speed,
          rain_1h: pkt.rain_1h,
          rain_24h: pkt.rain_24h,
          rain_since_midnight: pkt.rain_since_midnight,
          luminosity: pkt.luminosity
        }
      end)
      |> Jason.encode!()

    socket =
      socket
      |> assign(:callsign, normalized_callsign)
      |> assign(:weather_packet, weather_packet)
      |> assign(:page_title, "Weather for #{normalized_callsign}")
      |> assign(:weather_history, weather_history)
      |> assign(:weather_history_json, weather_history_json)

    {:ok, socket}
  end

  defp get_latest_weather_packet(callsign) do
    # Get the most recent packet for this callsign that is a weather report
    %{callsign: callsign, limit: 10}
    |> Packets.get_recent_packets()
    |> Enum.find(&PacketUtils.weather_packet?/1)
  end

  defp get_weather_history(callsign, start_time, end_time) do
    opts = %{callsign: callsign, start_time: start_time, end_time: end_time, limit: 1000}

    opts
    |> Packets.get_packets_for_replay()
    |> Enum.filter(&PacketUtils.weather_packet?/1)
    |> Enum.sort_by(& &1.received_at)
  end

  defp default_time_range do
    now = DateTime.utc_now()
    {DateTime.add(now, -48 * 3600, :second), now}
  end

  defp build_temp_chart_svg(history) do
    data =
      history
      |> Enum.map(fn pkt ->
        [pkt.received_at, pkt.temperature, pkt.dew_point]
      end)
      |> Enum.filter(fn [_, t, d] -> not is_nil(t) and not is_nil(d) end)

    if data == [] do
      "<svg width=\"600\" height=\"300\"><text x=\"50\" y=\"150\">No data</text></svg>"
    else
      dataset = Dataset.new(data, ["Time", "Temperature", "Dew Point"])

      dataset
      |> Plot.new(LinePlot, 600, 300, smoothed: false)
      |> Plot.titles("Temperature & Dew Point (°F)", nil)
      |> Plot.axis_labels("Time", "°F")
      |> Plot.to_svg()
    end
  end

  defp build_humidity_chart_svg(history) do
    data =
      history
      |> Enum.map(fn pkt ->
        [pkt.received_at, pkt.humidity]
      end)
      |> Enum.filter(fn [_, h] -> not is_nil(h) end)

    if data == [] do
      "<svg width=\"600\" height=\"300\"><text x=\"50\" y=\"150\">No data</text></svg>"
    else
      dataset = Dataset.new(data, ["Time", "Humidity"])

      dataset
      |> Plot.new(LinePlot, 600, 300, smoothed: false)
      |> Plot.titles("Humidity (%)", nil)
      |> Plot.axis_labels("Time", "%")
      |> Plot.to_svg()
    end
  end

  defp calc_dew_point(temp, humidity) when is_number(temp) and is_number(humidity) do
    temp - (100 - humidity) / 5
  end

  defp calc_dew_point(_, _), do: nil
end
