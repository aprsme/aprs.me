defmodule AprsmeWeb.UserRegistrationLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  import AprsmeWeb.CoreComponents, only: [translate_error: 1, simple_form: 1]

  alias Aprsme.Accounts
  alias Aprsme.Accounts.User

  def render(assigns) do
    ~H"""
    <div class="flex min-h-full flex-col justify-center py-12 sm:px-6 lg:px-8">
      <div class="sm:mx-auto sm:w-full sm:max-w-md">
        <h2 class="text-center text-2xl/9 font-bold tracking-tight text-gray-900 dark:text-white">
          {gettext("Register for an account")}
        </h2>
        <p class="mt-2 text-center text-sm/6 text-gray-500 dark:text-gray-400">
          {gettext("Already registered?")}
          <.link
            navigate={~p"/users/log_in"}
            class="font-semibold text-indigo-600 hover:text-indigo-500 dark:text-indigo-400 dark:hover:text-indigo-300"
          >
            {gettext("Sign in")}
          </.link>
          {gettext("to your account now.")}
        </p>
      </div>

      <div class="mt-10 sm:mx-auto sm:w-full sm:max-w-[480px]">
        <div class="bg-white px-6 py-12 shadow-sm sm:rounded-lg sm:px-12 dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <.simple_form
            :let={f}
            id="registration_form"
            for={@changeset}
            phx-submit="save"
            phx-change="validate"
            phx-trigger-action={@trigger_submit}
            action={~p"/users/log_in?_action=registered"}
            method="post"
            as={:user}
          >
            <div :if={@changeset.action == :insert} class="rounded-md bg-red-50 p-4 dark:bg-red-500/10">
              <p class="text-sm text-red-700 dark:text-red-400">
                {gettext("Oops, something went wrong! Please check the errors below.")}
              </p>
            </div>

            <div>
              <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">{gettext("Email")}</label>
              <div class="mt-2">
                <input
                  type="email"
                  name={Phoenix.HTML.Form.input_name(f, :email)}
                  value={Phoenix.HTML.Form.input_value(f, :email) || ""}
                  class={[
                    "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                    if(@changeset.action && Keyword.has_key?(@changeset.errors, :email),
                      do: "outline-red-500 dark:outline-red-400",
                      else: "outline-gray-300 dark:outline-white/10"
                    )
                  ]}
                  placeholder={gettext("Enter your email")}
                  required
                />
              </div>
              <p
                :if={@changeset.action && Keyword.has_key?(@changeset.errors, :email)}
                class="mt-1 text-sm text-red-600 dark:text-red-400"
              >
                {translate_error(Keyword.get(@changeset.errors, :email))}
              </p>
            </div>

            <div>
              <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">{gettext("Callsign")}</label>
              <div class="mt-2">
                <input
                  type="text"
                  name={Phoenix.HTML.Form.input_name(f, :callsign)}
                  value={Phoenix.HTML.Form.input_value(f, :callsign) || ""}
                  class={[
                    "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                    if(@changeset.action && Keyword.has_key?(@changeset.errors, :callsign),
                      do: "outline-red-500 dark:outline-red-400",
                      else: "outline-gray-300 dark:outline-white/10"
                    )
                  ]}
                  placeholder={gettext("Enter your amateur radio callsign")}
                  required
                />
              </div>
              <p
                :if={@changeset.action && Keyword.has_key?(@changeset.errors, :callsign)}
                class="mt-1 text-sm text-red-600 dark:text-red-400"
              >
                {translate_error(Keyword.get(@changeset.errors, :callsign))}
              </p>
            </div>

            <div>
              <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">{gettext("Password")}</label>
              <div class="mt-2">
                <input
                  type="password"
                  name={Phoenix.HTML.Form.input_name(f, :password)}
                  value={Phoenix.HTML.Form.input_value(f, :password) || ""}
                  class={[
                    "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                    if(@changeset.action && Keyword.has_key?(@changeset.errors, :password),
                      do: "outline-red-500 dark:outline-red-400",
                      else: "outline-gray-300 dark:outline-white/10"
                    )
                  ]}
                  placeholder={gettext("Enter your password")}
                  required
                />
              </div>
              <p
                :if={@changeset.action && Keyword.has_key?(@changeset.errors, :password)}
                class="mt-1 text-sm text-red-600 dark:text-red-400"
              >
                {translate_error(Keyword.get(@changeset.errors, :password))}
              </p>
            </div>

            <div>
              <button
                type="submit"
                class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm/6 font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400 dark:focus-visible:outline-indigo-500"
                phx-disable-with={gettext("Creating account...")}
              >
                {gettext("Create an account")}
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
    socket = assign(socket, changeset: changeset, trigger_submit: false)
    {:ok, socket, temporary_assigns: [changeset: nil]}
  end

  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.register_user(user_params) do
      {:ok, user} ->
        {:ok, _} =
          Accounts.deliver_user_confirmation_instructions(
            user,
            &url(~p"/users/confirm/#{&1}")
          )

        changeset = Accounts.change_user_registration(user)
        {:noreply, assign(socket, trigger_submit: true, changeset: changeset)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset = Accounts.change_user_registration(%User{}, user_params)
    {:noreply, assign(socket, changeset: Map.put(changeset, :action, :validate))}
  end
end
