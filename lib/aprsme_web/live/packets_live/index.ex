defmodule AprsmeWeb.PacketsLive.Index do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.EncodingUtils
  alias AprsmeWeb.Endpoint

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")
    end

    {:ok, assign(socket, :packets, [])}
  end

  @impl true
  def handle_info(%{event: "packet", payload: payload}, socket) do
    # Sanitize the packet to prevent JSON encoding errors
    sanitized_payload = EncodingUtils.sanitize_packet(payload)
    packets = Enum.take([sanitized_payload | socket.assigns.packets], 100)
    socket = assign(socket, :packets, packets)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:postgres_packet, payload}, socket) do
    sanitized_payload = Aprsme.EncodingUtils.sanitize_packet(payload)
    packets = Enum.take([sanitized_payload | socket.assigns.packets], 100)
    socket = assign(socket, :packets, packets)
    {:noreply, socket}
  end
end
