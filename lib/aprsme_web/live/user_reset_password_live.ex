defmodule AprsmeWeb.UserResetPasswordLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts

  def render(assigns) do
    ~H"""
    <div class="hero min-h-screen bg-base-200">
      <div class="hero-content text-center">
        <div class="max-w-md">
          <div class="card bg-base-100 shadow-xl">
            <div class="card-body">
              <h1 class="card-title text-3xl mb-4 justify-center">Reset Password</h1>
              <p class="text-base-content/70 mb-6">Enter your new password below</p>

              <.simple_form
                :let={f}
                for={@changeset}
                id="reset_password_form"
                phx-submit="reset_password"
                phx-change="validate"
              >
                <div :if={@changeset.action == :insert} class="alert alert-error mb-4">
                  <span>Oops, something went wrong! Please check the errors below.</span>
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">New password</span>
                  </label>
                  <input
                    type="password"
                    name={Phoenix.HTML.Form.input_name(f, :password)}
                    value={Phoenix.HTML.Form.input_value(f, :password) || ""}
                    class="input input-bordered w-full bg-base-100 text-base-content"
                    placeholder="Enter new password"
                    required
                  />
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">Confirm new password</span>
                  </label>
                  <input
                    type="password"
                    name={Phoenix.HTML.Form.input_name(f, :password_confirmation)}
                    value={Phoenix.HTML.Form.input_value(f, :password_confirmation) || ""}
                    class="input input-bordered w-full bg-base-100 text-base-content"
                    placeholder="Confirm new password"
                    required
                  />
                </div>

                <div class="form-control mt-6">
                  <button type="submit" class="btn btn-primary w-full" phx-disable-with="Resetting...">
                    Reset Password
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
    socket = assign_user_and_token(socket, params)

    socket =
      case socket.assigns do
        %{user: user} ->
          assign(socket, :changeset, Accounts.change_user_password(user))

        _ ->
          socket
      end

    {:ok, socket, temporary_assigns: [changeset: nil]}
  end

  # Do not log in the user after reset password to avoid a
  # leaked token giving the user access to the account.
  def handle_event("reset_password", %{"user" => user_params}, socket) do
    case Accounts.reset_user_password(socket.assigns.user, user_params) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:info, "Password reset successfully.")
         |> redirect(to: ~p"/users/log_in")}

      {:error, changeset} ->
        {:noreply, assign(socket, :changeset, Map.put(changeset, :action, :insert))}
    end
  end

  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset = Accounts.change_user_password(socket.assigns.user, user_params)
    {:noreply, assign(socket, changeset: Map.put(changeset, :action, :validate))}
  end

  defp assign_user_and_token(socket, %{"token" => token}) do
    if user = Accounts.get_user_by_reset_password_token(token) do
      assign(socket, user: user, token: token)
    else
      socket
      |> put_flash(:error, "Reset password link is invalid or it has expired.")
      |> redirect(to: ~p"/")
    end
  end
end
