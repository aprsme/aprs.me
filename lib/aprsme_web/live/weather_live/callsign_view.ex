defmodule AprsmeWeb.WeatherLive.CallsignView do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Packets
  alias AprsmeWeb.MapLive.PacketUtils

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    normalized_callsign = String.upcase(String.trim(callsign))
    weather_packet = get_latest_weather_packet(normalized_callsign)

    socket =
      socket
      |> assign(:callsign, normalized_callsign)
      |> assign(:weather_packet, weather_packet)
      |> assign(:page_title, "Weather for #{normalized_callsign}")

    {:ok, socket}
  end

  defp get_latest_weather_packet(callsign) do
    # Get the most recent packet for this callsign that is a weather report
    %{callsign: callsign, limit: 10}
    |> Packets.get_recent_packets()
    |> Enum.find(&PacketUtils.weather_packet?/1)
  end
end
