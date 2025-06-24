defmodule AprsmeWeb.BadPacketsLive.Index do
  @moduledoc false
  use AprsmeWeb, :live_view

  import Ecto.Query

  alias Aprsme.BadPacket
  alias Aprsme.Repo

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Subscribe to Postgres notifications for bad packets
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_events")
      # Load initial bad packets
      bad_packets = fetch_bad_packets()
      # Extra safeguard to ensure we never show more than 100
      limited_packets = Enum.take(bad_packets, 100)

      {:ok,
       assign(socket,
         bad_packets: limited_packets,
         loading: false,
         last_updated: DateTime.utc_now()
       )}
    else
      {:ok, assign(socket, bad_packets: [], loading: false, last_updated: nil)}
    end
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    assign(socket, :page_title, "Bad Packets")
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    send(self(), :do_refresh)
    {:noreply, assign(socket, loading: true)}
  end

  @impl true
  def handle_info({:postgres_notify, _payload}, socket) do
    # Optionally filter payload for bad packet events
    send(self(), :do_refresh)
    {:noreply, socket}
  end

  @impl true
  def handle_info(:do_refresh, socket) do
    bad_packets = fetch_bad_packets()
    # Extra safeguard to ensure we never show more than 100
    limited_packets = Enum.take(bad_packets, 100)

    {:noreply,
     assign(socket,
       bad_packets: limited_packets,
       loading: false,
       last_updated: DateTime.utc_now()
     )}
  end

  defp fetch_bad_packets(limit \\ 100) do
    # Hard cap at 100 to prevent showing too many records
    actual_limit = min(limit, 100)

    BadPacket
    |> order_by([b], desc: b.attempted_at)
    |> limit(^actual_limit)
    |> Repo.all()
  end
end
