defmodule Aprs.PostgresNotifier do
  @moduledoc """
  Listens to PostgreSQL NOTIFY events on the "aprs_events" and "aprs_packets" channels and broadcasts
  them via Phoenix.PubSub for reactive, event-driven updates.
  """
  use GenServer

  @event_channel "aprs_events"
  @event_topic "postgres:aprs_events"
  @packet_channel "aprs_packets"
  @packet_topic "postgres:aprs_packets"

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  @impl true
  def init(_) do
    {:ok, conn} = Postgrex.Notifications.start_link(Aprs.Repo.config())
    {:ok, _ref1} = Postgrex.Notifications.listen(conn, @event_channel)
    {:ok, _ref2} = Postgrex.Notifications.listen(conn, @packet_channel)
    {:ok, %{conn: conn}}
  end

  @impl true
  def handle_info({:notification, _conn, _pid, @event_channel, payload}, state) do
    Phoenix.PubSub.broadcast(Aprs.PubSub, @event_topic, {:postgres_notify, payload})
    {:noreply, state}
  end

  def handle_info({:notification, _conn, _pid, @packet_channel, payload}, state) do
    case Jason.decode(payload) do
      {:ok, packet} ->
        Phoenix.PubSub.broadcast(Aprs.PubSub, @packet_topic, {:postgres_packet, packet})

      _ ->
        :noop
    end

    {:noreply, state}
  end

  def handle_info(_msg, state), do: {:noreply, state}
end
