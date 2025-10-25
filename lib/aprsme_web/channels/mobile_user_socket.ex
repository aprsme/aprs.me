defmodule AprsmeWeb.MobileUserSocket do
  @moduledoc """
  Socket for mobile applications (iOS/Android) to receive real-time APRS packets.
  """
  use Phoenix.Socket

  # Channels
  channel "mobile:packets", AprsmeWeb.MobileChannel

  # Socket params are passed from the client and can be used to verify and authenticate a user.
  # After verification, you can put default assigns into the socket that will be set for all channels.
  @impl true
  def connect(_params, socket, _connect_info) do
    # For now, allow anonymous connections
    # In the future, you can add authentication here:
    # case verify_token(params["token"]) do
    #   {:ok, user_id} -> {:ok, assign(socket, :user_id, user_id)}
    #   {:error, _} -> :error
    # end

    {:ok, socket}
  end

  # Socket id's are topics that allow you to identify all sockets for a given user:
  #
  #     def id(socket), do: "mobile_user_socket:#{socket.assigns.user_id}"
  #
  # Would allow you to broadcast a "disconnect" event and terminate
  # all active sockets and channels for a given user:
  #
  #     Elixir.AprsmeWeb.Endpoint.broadcast("mobile_user_socket:#{user.id}", "disconnect", %{})
  #
  # Returning `nil` makes this socket anonymous.
  @impl true
  def id(_socket), do: nil
end
