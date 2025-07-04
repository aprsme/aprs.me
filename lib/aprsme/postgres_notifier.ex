defmodule Aprsme.PostgresNotifier do
  @moduledoc """
  Listens to PostgreSQL NOTIFY events on the "aprs_events" and "aprs_packets" channels and broadcasts
  them via Phoenix.PubSub for reactive, event-driven updates.
  """
  use GenServer

  @event_channel "aprs_events"
  @event_topic "postgres:aprsme_events"
  @packet_channel "aprs_packets"
  @packet_topic "postgres:aprsme_packets"

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  @impl true
  def init(_) do
    {:ok, conn} = Postgrex.Notifications.start_link(Aprsme.Repo.config())
    {:ok, _ref1} = Postgrex.Notifications.listen(conn, @event_channel)
    {:ok, _ref2} = Postgrex.Notifications.listen(conn, @packet_channel)
    {:ok, %{conn: conn}}
  end

  @impl true
  def handle_info({:notification, _conn, _pid, @event_channel, payload}, state) do
    Phoenix.PubSub.broadcast(Aprsme.PubSub, @event_topic, {:postgres_notify, payload})
    {:noreply, state}
  end

  def handle_info({:notification, _conn, _pid, @packet_channel, payload}, state) do
    case Jason.decode(payload) do
      {:ok, packet} ->
        # Broadcast to the general packet topic
        Phoenix.PubSub.broadcast(Aprsme.PubSub, @packet_topic, {:postgres_packet, packet})

        # Also broadcast to callsign-specific weather topic if it's a weather packet
        broadcast_weather_packet_if_relevant(packet)

      _ ->
        :noop
    end

    {:noreply, state}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  defp broadcast_weather_packet_if_relevant(packet) do
    # Check if this is a weather packet using centralized weather fields
    has_weather_data =
      Enum.any?(Aprsme.EncodingUtils.weather_fields(), fn field ->
        value = Map.get(packet, to_string(field))
        not is_nil(value)
      end)

    if has_weather_data do
      # Extract callsign from packet
      callsign = Map.get(packet, "sender") || Map.get(packet, :sender)

      if is_binary(callsign) and callsign != "" do
        # Normalize callsign and broadcast to weather-specific topic
        normalized_callsign = String.upcase(String.trim(callsign))
        weather_topic = "weather:#{normalized_callsign}"
        Phoenix.PubSub.broadcast(Aprsme.PubSub, weather_topic, {:weather_packet, packet})
      end
    end
  end
end
