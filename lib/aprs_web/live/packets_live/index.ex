defmodule AprsWeb.PacketsLive.Index do
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

  # AprsWeb.PacketsLive.Index.handle_info(%Phoenix.Socket.Broadcast{topic: "aprs_messages", event: "packet", payload: %{base_callsign: "AE5PL", data_extended: %{aprs_messaging?: false, comment: "RNG0001 70cm Voice 441.1625MHz", data_type: :position, latitude: 33.26733333333333, longitude: -96.53266666666667, symbol_code: "&", symbol_table_id: "D"}, data_type: :position, destination: "APJI43", information_field: "!3316.04ND09631.96W&RNG0001 70cm Voice 441.1625MHz", path: "TCPIP*,qAC,AE5PL-IG", sender: "AE5PL-B", ssid: "B"}}, #Phoenix.LiveView.Socket<id: "phx-Fz9zBNjSOD3z4wAG", endpoint: AprsWeb.Endpoint, view: AprsWeb.PacketsLive.Index, parent_pid: nil, root_pid: #PID<0.785.0>, router: AprsWeb.Router, assigns: %{__changed__: %{}, flash: %{}, live_action: :index, packets: []}, transport_pid: #PID<0.776.0>, ...>)

  # @impl true
  # def handle_params(params, _url, socket) do
  #   {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  # end

  # defp apply_action(socket, :edit, %{"id" => id}) do
  #   socket
  #   |> assign(:page_title, "Edit Book")
  #   |> assign(:book, Books.get_book!(id))
  # end

  # defp apply_action(socket, :new, _params) do
  #   socket
  #   |> assign(:page_title, "New Book")
  #   |> assign(:book, %Book{})
  # end

  # defp apply_action(socket, :index, _params) do
  #   socket
  #   |> assign(:page_title, "Listing Books")
  #   |> assign(:book, nil)
  # end

  # @impl true
  # def handle_event("delete", %{"id" => id}, socket) do
  #   book = Books.get_book!(id)
  #   {:ok, _} = Books.delete_book(book)

  #   {:noreply, assign(socket, :books, list_books())}
  # end

  # defp list_books do
  #   Books.list_books()
  # end
end
