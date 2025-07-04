defmodule AprsmeWeb.UserForgotPasswordLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts

  def render(assigns) do
    ~H"""
    <div class="mx-auto max-w-sm">
      <.header class="text-center" />

      <h2 class="text-center text-2xl font-bold mt-6 mb-2">Forgot your password?</h2>
      <p class="text-center text-gray-600 mb-6">We'll send a password reset link to your inbox</p>

      <.simple_form :let={f} id="reset_password_form" for={%{}} as={:user} phx-submit="send_email">
        <.input field={{f, :email}} type="email" placeholder="Email" required />
        <:actions>
          <.button phx-disable-with="Sending..." class="w-full">
            Send password reset instructions
          </.button>
        </:actions>
      </.simple_form>
      <p class="text-center mt-4">
        <.link navigate={~p"/users/register"}>Register</.link>
        | <.link navigate={~p"/users/log_in"}>Log in</.link>
      </p>
    </div>
    """
  end

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  def handle_event("send_email", %{"user" => %{"email" => email}}, socket) do
    if user = Accounts.get_user_by_email(email) do
      Accounts.deliver_user_reset_password_instructions(
        user,
        &url(~p"/users/reset_password/#{&1}")
      )
    end

    info =
      "If your email is in our system, you will receive instructions to reset your password shortly."

    {:noreply,
     socket
     |> put_flash(:info, info)
     |> redirect(to: ~p"/")}
  end
end
