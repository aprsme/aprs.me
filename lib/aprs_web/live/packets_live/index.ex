defmodule AprsWeb.PacketsLive.Index do
  @moduledoc false
  use AprsWeb, :live_view

  alias Aprs.EncodingUtils
  alias AprsWeb.Endpoint

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      Phoenix.PubSub.subscribe(Aprs.PubSub, "postgres:aprs_packets")
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
    sanitized_payload = Aprs.EncodingUtils.sanitize_packet(payload)
    packets = Enum.take([sanitized_payload | socket.assigns.packets], 100)
    socket = assign(socket, :packets, packets)
    {:noreply, socket}
  end
end
