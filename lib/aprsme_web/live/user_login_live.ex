defmodule AprsmeWeb.UserLoginLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts
  alias Aprsme.Accounts.User

  def render(assigns) do
    ~H"""
    <div class="flex min-h-full flex-col justify-center py-12 sm:px-6 lg:px-8">
      <div class="sm:mx-auto sm:w-full sm:max-w-md">
        <h2 class="text-center text-2xl/9 font-bold tracking-tight text-gray-900 dark:text-white">
          {gettext("Sign in to account")}
        </h2>
        <p class="mt-2 text-center text-sm/6 text-gray-500 dark:text-gray-400">
          {gettext("Don't have an account?")}
          <.link
            navigate={~p"/users/register"}
            class="font-semibold text-indigo-600 hover:text-indigo-500 dark:text-indigo-400 dark:hover:text-indigo-300"
          >
            {gettext("Sign up")}
          </.link>
          {gettext("for an account now.")}
        </p>
      </div>

      <div class="mt-10 sm:mx-auto sm:w-full sm:max-w-[480px]">
        <div class="bg-white px-6 py-12 shadow-sm sm:rounded-lg sm:px-12 dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <.simple_form
            :let={f}
            id="login_form"
            for={@changeset}
            action={~p"/users/log_in"}
            as={:user}
            phx-update="ignore"
          >
            <div>
              <label for="user_email" class="block text-sm/6 font-medium text-gray-900 dark:text-white">
                {gettext("Email")}
              </label>
              <div class="mt-2">
                <input
                  type="email"
                  id="user_email"
                  name={Phoenix.HTML.Form.input_name(f, :email)}
                  value={Phoenix.HTML.Form.input_value(f, :email) || ""}
                  class="block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 outline-gray-300 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:outline-white/10 dark:placeholder:text-gray-500 dark:focus:outline-indigo-500"
                  placeholder={gettext("Enter your email")}
                  required
                />
              </div>
            </div>

            <div>
              <label for="user_password" class="block text-sm/6 font-medium text-gray-900 dark:text-white">
                {gettext("Password")}
              </label>
              <div class="mt-2">
                <input
                  type="password"
                  id="user_password"
                  name={Phoenix.HTML.Form.input_name(f, :password)}
                  value={Phoenix.HTML.Form.input_value(f, :password) || ""}
                  class="block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 outline-gray-300 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:outline-white/10 dark:placeholder:text-gray-500 dark:focus:outline-indigo-500"
                  placeholder={gettext("Enter your password")}
                  required
                />
              </div>
            </div>

            <div class="flex items-center justify-between">
              <div class="flex items-center gap-3">
                <input
                  type="checkbox"
                  id="user_remember_me"
                  name={Phoenix.HTML.Form.input_name(f, :remember_me)}
                  value="true"
                  class="size-4 rounded-sm border-gray-300 text-indigo-600 focus:ring-indigo-600 dark:border-white/10 dark:bg-white/5"
                />
                <label for="user_remember_me" class="block text-sm/6 text-gray-900 dark:text-white">
                  {gettext("Keep me logged in")}
                </label>
              </div>
              <div class="text-sm/6">
                <.link
                  navigate={~p"/users/reset_password"}
                  class="font-semibold text-indigo-600 hover:text-indigo-500 dark:text-indigo-400 dark:hover:text-indigo-300"
                >
                  {gettext("Forgot your password?")}
                </.link>
              </div>
            </div>

            <div>
              <button
                type="submit"
                class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm/6 font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400 dark:focus-visible:outline-indigo-500"
                phx-disable-with={gettext("Signing in...")}
              >
                {gettext("Sign in")}
              </button>
            </div>
          </.simple_form>
        </div>
      </div>
    </div>
    """
  end

  def mount(_params, _session, socket) do
    changeset = Accounts.change_user_registration(%User{})
    {:ok, assign(socket, changeset: changeset), temporary_assigns: [changeset: nil]}
  end
end
