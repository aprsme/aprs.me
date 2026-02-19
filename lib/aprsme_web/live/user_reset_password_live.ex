defmodule AprsmeWeb.UserResetPasswordLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts

  def render(assigns) do
    ~H"""
    <div class="flex min-h-full flex-col justify-center py-12 sm:px-6 lg:px-8">
      <div class="sm:mx-auto sm:w-full sm:max-w-md">
        <h2 class="text-center text-2xl/9 font-bold tracking-tight text-gray-900 dark:text-white">
          Reset Password
        </h2>
        <p class="mt-2 text-center text-sm/6 text-gray-500 dark:text-gray-400">
          Enter your new password below
        </p>
      </div>

      <div class="mt-10 sm:mx-auto sm:w-full sm:max-w-[480px]">
        <div class="bg-white px-6 py-12 shadow-sm sm:rounded-lg sm:px-12 dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <.simple_form
            :let={f}
            for={@changeset}
            id="reset_password_form"
            phx-submit="reset_password"
            phx-change="validate"
          >
            <div :if={@changeset.action == :insert} class="rounded-md bg-red-50 p-4 dark:bg-red-500/10">
              <p class="text-sm text-red-700 dark:text-red-400">
                Oops, something went wrong! Please check the errors below.
              </p>
            </div>

            <div>
              <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">New password</label>
              <div class="mt-2">
                <input
                  type="password"
                  name={Phoenix.HTML.Form.input_name(f, :password)}
                  value={Phoenix.HTML.Form.input_value(f, :password) || ""}
                  class="block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 outline-gray-300 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:outline-white/10 dark:placeholder:text-gray-500 dark:focus:outline-indigo-500"
                  placeholder="Enter new password"
                  required
                />
              </div>
            </div>

            <div>
              <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Confirm new password</label>
              <div class="mt-2">
                <input
                  type="password"
                  name={Phoenix.HTML.Form.input_name(f, :password_confirmation)}
                  value={Phoenix.HTML.Form.input_value(f, :password_confirmation) || ""}
                  class="block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 outline-gray-300 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:outline-white/10 dark:placeholder:text-gray-500 dark:focus:outline-indigo-500"
                  placeholder="Confirm new password"
                  required
                />
              </div>
            </div>

            <div>
              <button
                type="submit"
                class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm/6 font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400 dark:focus-visible:outline-indigo-500"
                phx-disable-with="Resetting..."
              >
                Reset Password
              </button>
            </div>
          </.simple_form>

          <div class="relative mt-10">
            <div class="absolute inset-0 flex items-center">
              <div class="w-full border-t border-gray-200 dark:border-white/10"></div>
            </div>
            <div class="relative flex justify-center text-sm/6">
              <span class="bg-white px-6 text-gray-900 dark:bg-gray-800/50 dark:text-white">OR</span>
            </div>
          </div>

          <div class="mt-6 text-center text-sm/6 text-gray-500 dark:text-gray-400">
            <.link
              navigate={~p"/users/register"}
              class="font-semibold text-indigo-600 hover:text-indigo-500 dark:text-indigo-400 dark:hover:text-indigo-300"
            >
              Register
            </.link>
            <span class="mx-2">|</span>
            <.link
              navigate={~p"/users/log_in"}
              class="font-semibold text-indigo-600 hover:text-indigo-500 dark:text-indigo-400 dark:hover:text-indigo-300"
            >
              Log in
            </.link>
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
