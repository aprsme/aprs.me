defmodule AprsmeWeb.UserConfirmationInstructionsLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts

  def render(assigns) do
    ~H"""
    <div class="flex min-h-full flex-col justify-center py-12 sm:px-6 lg:px-8">
      <div class="sm:mx-auto sm:w-full sm:max-w-md">
        <h2 class="text-center text-2xl/9 font-bold tracking-tight text-gray-900 dark:text-white">
          Resend confirmation instructions
        </h2>
        <p class="mt-2 text-center text-sm/6 text-gray-500 dark:text-gray-400">
          Enter your email address to receive a new confirmation link
        </p>
      </div>

      <div class="mt-10 sm:mx-auto sm:w-full sm:max-w-[480px]">
        <div class="bg-white px-6 py-12 shadow-sm sm:rounded-lg sm:px-12 dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <.simple_form :let={_f} for={%{}} as={:user} id="resend_confirmation_form" phx-submit="send_instructions">
            <div>
              <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Email</label>
              <div class="mt-2">
                <input
                  type="email"
                  name="user[email]"
                  class="block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 outline-gray-300 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:outline-white/10 dark:placeholder:text-gray-500 dark:focus:outline-indigo-500"
                  placeholder="Enter your email"
                  required
                />
              </div>
            </div>

            <div>
              <button
                type="submit"
                class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm/6 font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400 dark:focus-visible:outline-indigo-500"
                phx-disable-with="Sending..."
              >
                Resend confirmation instructions
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

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  def handle_event("send_instructions", %{"user" => %{"email" => email}}, socket) do
    if user = Accounts.get_user_by_email(email) do
      case Accounts.deliver_user_confirmation_instructions(
             user,
             &url(~p"/users/confirm/#{&1}")
           ) do
        {:ok, _} ->
          info =
            "If your email is in our system and it has not been confirmed yet, you will receive an email with instructions shortly."

          {:noreply,
           socket
           |> put_flash(:info, info)
           |> redirect(to: ~p"/")}

        {:error, _reason} ->
          {:noreply, put_flash(socket, :error, "Failed to send email. Please try again later.")}
      end
    else
      info =
        "If your email is in our system and it has not been confirmed yet, you will receive an email with instructions shortly."

      {:noreply,
       socket
       |> put_flash(:info, info)
       |> redirect(to: ~p"/")}
    end
  end
end
