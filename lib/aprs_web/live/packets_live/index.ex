defmodule AprsWeb.PacketsLive.Index do
  @moduledoc false
  use AprsWeb, :live_view

  alias AprsWeb.Endpoint

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
    end

    {:ok, assign(socket, :packets, [])}
  end

  @impl true
  def handle_info(%{event: "packet", payload: payload}, socket) do
    socket = assign(socket, :packets, [payload | socket.assigns.packets])
    {:noreply, socket}
  end
end
