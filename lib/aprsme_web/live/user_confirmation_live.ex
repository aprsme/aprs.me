defmodule AprsmeWeb.UserConfirmationLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts

  def render(%{live_action: :edit} = assigns) do
    ~H"""
    <div class="hero min-h-screen bg-base-200">
      <div class="hero-content text-center">
        <div class="max-w-md">
          <div class="card bg-base-100 shadow-xl">
            <div class="card-body">
              <h1 class="card-title text-3xl mb-4 justify-center">Confirm your account</h1>
              <p class="text-base-content/70 mb-6">Click the button below to confirm your account</p>

              <.simple_form :let={_f} for={%{}} as={:user} id="confirmation_form" phx-submit="confirm_account">
                <input type="hidden" name="user[token]" value={@token} />

                <div class="form-control mt-6">
                  <button type="submit" class="btn btn-primary w-full" phx-disable-with="Confirming...">
                    Confirm my account
                  </button>
                </div>
              </.simple_form>

              <div class="divider">OR</div>

              <div class="text-center text-sm">
                <.link navigate={~p"/users/register"} class="link link-primary">
                  Register
                </.link>
                |
                <.link navigate={~p"/users/log_in"} class="link link-primary">
                  Log in
                </.link>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def mount(params, _session, socket) do
    {:ok, assign(socket, token: params["token"]), temporary_assigns: [token: nil]}
  end

  # Do not log in the user after confirmation to avoid a
  # leaked token giving the user access to the account.
  def handle_event("confirm_account", %{"user" => %{"token" => token}}, socket) do
    case Accounts.confirm_user(token) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:info, "User confirmed successfully.")
         |> redirect(to: ~p"/")}

      :error ->
        # If there is a current user and the account was already confirmed,
        # then odds are that the confirmation link was already visited, either
        # by some automation or by the user themselves, so we redirect without
        # a warning message.
        case socket.assigns do
          %{current_user: %{confirmed_at: confirmed_at}} when not is_nil(confirmed_at) ->
            {:noreply, redirect(socket, to: ~p"/")}

          %{} ->
            {:noreply,
             socket
             |> put_flash(:error, "User confirmation link is invalid or it has expired.")
             |> redirect(to: ~p"/")}
        end
    end
  end
end
