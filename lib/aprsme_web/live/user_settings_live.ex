defmodule AprsmeWeb.UserSettingsLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  import AprsmeWeb.CoreComponents, only: [translate_error: 1, simple_form: 1]

  alias Aprsme.Accounts

  def render(assigns) do
    ~H"""
    <div class="min-h-screen bg-gray-50 py-12 dark:bg-gray-900">
      <div class="mx-auto max-w-4xl px-4 sm:px-6 lg:px-8">
        <div class="text-center mb-8">
          <h1 class="text-4xl font-bold text-gray-900 dark:text-white">Account Settings</h1>
          <p class="mt-2 text-gray-500 dark:text-gray-400">Update your email address, callsign, and password</p>
        </div>

        <div class="grid grid-cols-1 lg:grid-cols-3 gap-8">
          <!-- Change Email Section -->
          <div class="bg-white px-6 py-8 shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
            <h2 class="text-xl font-semibold text-gray-900 mb-6 dark:text-white">Change Email</h2>

            <.simple_form
              :let={f}
              id="email_form"
              for={@email_changeset}
              phx-submit="update_email"
              phx-change="validate_email"
            >
              <div :if={@email_changeset.action == :insert} class="rounded-md bg-red-50 p-4 dark:bg-red-500/10">
                <p class="text-sm text-red-700 dark:text-red-400">
                  Oops, something went wrong! Please check the errors below.
                </p>
              </div>

              <div>
                <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Email</label>
                <div class="mt-2">
                  <input
                    type="email"
                    name={Phoenix.HTML.Form.input_name(f, :email)}
                    value={Phoenix.HTML.Form.input_value(f, :email) || ""}
                    class={[
                      "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                      if(@email_changeset.action && Keyword.has_key?(@email_changeset.errors, :email),
                        do: "outline-red-500 dark:outline-red-400",
                        else: "outline-gray-300 dark:outline-white/10"
                      )
                    ]}
                    placeholder="Enter your email"
                    required
                  />
                </div>
                <p
                  :if={@email_changeset.action && Keyword.has_key?(@email_changeset.errors, :email)}
                  class="mt-1 text-sm text-red-600 dark:text-red-400"
                >
                  {translate_error(Keyword.get(@email_changeset.errors, :email))}
                </p>
              </div>

              <div>
                <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Current password</label>
                <div class="mt-2">
                  <input
                    type="password"
                    name="current_password"
                    id="current_password_for_email"
                    value={@email_form_current_password || ""}
                    class={[
                      "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                      if(@email_changeset.action && Keyword.has_key?(@email_changeset.errors, :current_password),
                        do: "outline-red-500 dark:outline-red-400",
                        else: "outline-gray-300 dark:outline-white/10"
                      )
                    ]}
                    placeholder="Enter current password"
                    required
                  />
                </div>
                <p
                  :if={@email_changeset.action && Keyword.has_key?(@email_changeset.errors, :current_password)}
                  class="mt-1 text-sm text-red-600 dark:text-red-400"
                >
                  {translate_error(Keyword.get(@email_changeset.errors, :current_password))}
                </p>
              </div>

              <div>
                <button
                  type="submit"
                  class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm/6 font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400 dark:focus-visible:outline-indigo-500"
                  phx-disable-with="Changing..."
                >
                  Change Email
                </button>
              </div>
            </.simple_form>
          </div>
          
    <!-- Change Callsign Section -->
          <div class="bg-white px-6 py-8 shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
            <h2 class="text-xl font-semibold text-gray-900 mb-6 dark:text-white">Change Callsign</h2>

            <.simple_form
              :let={f}
              id="callsign_form"
              for={@callsign_changeset}
              phx-submit="update_callsign"
              phx-change="validate_callsign"
            >
              <div :if={@callsign_changeset.action == :insert} class="rounded-md bg-red-50 p-4 dark:bg-red-500/10">
                <p class="text-sm text-red-700 dark:text-red-400">
                  Oops, something went wrong! Please check the errors below.
                </p>
              </div>

              <div>
                <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Callsign</label>
                <div class="mt-2">
                  <input
                    type="text"
                    name={Phoenix.HTML.Form.input_name(f, :callsign)}
                    value={Phoenix.HTML.Form.input_value(f, :callsign) || ""}
                    class={[
                      "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                      if(@callsign_changeset.action && Keyword.has_key?(@callsign_changeset.errors, :callsign),
                        do: "outline-red-500 dark:outline-red-400",
                        else: "outline-gray-300 dark:outline-white/10"
                      )
                    ]}
                    placeholder="Enter your callsign"
                    required
                  />
                </div>
                <p
                  :if={@callsign_changeset.action && Keyword.has_key?(@callsign_changeset.errors, :callsign)}
                  class="mt-1 text-sm text-red-600 dark:text-red-400"
                >
                  {translate_error(Keyword.get(@callsign_changeset.errors, :callsign))}
                </p>
              </div>

              <div>
                <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Current password</label>
                <div class="mt-2">
                  <input
                    type="password"
                    name="current_password"
                    id="current_password_for_callsign"
                    value={@callsign_form_current_password || ""}
                    class={[
                      "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                      if(@callsign_changeset.action && Keyword.has_key?(@callsign_changeset.errors, :current_password),
                        do: "outline-red-500 dark:outline-red-400",
                        else: "outline-gray-300 dark:outline-white/10"
                      )
                    ]}
                    placeholder="Enter current password"
                    required
                  />
                </div>
                <p
                  :if={@callsign_changeset.action && Keyword.has_key?(@callsign_changeset.errors, :current_password)}
                  class="mt-1 text-sm text-red-600 dark:text-red-400"
                >
                  {translate_error(Keyword.get(@callsign_changeset.errors, :current_password))}
                </p>
              </div>

              <div>
                <button
                  type="submit"
                  class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm/6 font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400 dark:focus-visible:outline-indigo-500"
                  phx-disable-with="Changing..."
                >
                  Change Callsign
                </button>
              </div>
            </.simple_form>
          </div>
          
    <!-- Change Password Section -->
          <div class="bg-white px-6 py-8 shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
            <h2 class="text-xl font-semibold text-gray-900 mb-6 dark:text-white">Change Password</h2>

            <.simple_form
              :let={f}
              id="password_form"
              for={@password_changeset}
              action={~p"/users/log_in?_action=password_updated"}
              method="post"
              phx-change="validate_password"
              phx-submit="update_password"
              phx-trigger-action={@trigger_submit}
            >
              <div :if={@password_changeset.action == :insert} class="rounded-md bg-red-50 p-4 dark:bg-red-500/10">
                <p class="text-sm text-red-700 dark:text-red-400">
                  Oops, something went wrong! Please check the errors below.
                </p>
              </div>

              <input type="hidden" name={Phoenix.HTML.Form.input_name(f, :email)} value={@current_email} />

              <div>
                <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">New password</label>
                <div class="mt-2">
                  <input
                    type="password"
                    name={Phoenix.HTML.Form.input_name(f, :password)}
                    value={Phoenix.HTML.Form.input_value(f, :password) || ""}
                    class={[
                      "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                      if(@password_changeset.action && Keyword.has_key?(@password_changeset.errors, :password),
                        do: "outline-red-500 dark:outline-red-400",
                        else: "outline-gray-300 dark:outline-white/10"
                      )
                    ]}
                    placeholder="Enter new password"
                    required
                  />
                </div>
                <p
                  :if={@password_changeset.action && Keyword.has_key?(@password_changeset.errors, :password)}
                  class="mt-1 text-sm text-red-600 dark:text-red-400"
                >
                  {translate_error(Keyword.get(@password_changeset.errors, :password))}
                </p>
              </div>

              <div>
                <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Confirm new password</label>
                <div class="mt-2">
                  <input
                    type="password"
                    name={Phoenix.HTML.Form.input_name(f, :password_confirmation)}
                    value={Phoenix.HTML.Form.input_value(f, :password_confirmation) || ""}
                    class={[
                      "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                      if(@password_changeset.action && Keyword.has_key?(@password_changeset.errors, :password_confirmation),
                        do: "outline-red-500 dark:outline-red-400",
                        else: "outline-gray-300 dark:outline-white/10"
                      )
                    ]}
                    placeholder="Confirm new password"
                    required
                  />
                </div>
                <p
                  :if={@password_changeset.action && Keyword.has_key?(@password_changeset.errors, :password_confirmation)}
                  class="mt-1 text-sm text-red-600 dark:text-red-400"
                >
                  {translate_error(Keyword.get(@password_changeset.errors, :password_confirmation))}
                </p>
              </div>

              <div>
                <label class="block text-sm/6 font-medium text-gray-900 dark:text-white">Current password</label>
                <div class="mt-2">
                  <input
                    type="password"
                    name="current_password"
                    id="current_password_for_password"
                    value={@current_password || ""}
                    class={[
                      "block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:placeholder:text-gray-500 dark:focus:outline-indigo-500",
                      if(@password_changeset.action && Keyword.has_key?(@password_changeset.errors, :current_password),
                        do: "outline-red-500 dark:outline-red-400",
                        else: "outline-gray-300 dark:outline-white/10"
                      )
                    ]}
                    placeholder="Enter current password"
                    required
                  />
                </div>
                <p
                  :if={@password_changeset.action && Keyword.has_key?(@password_changeset.errors, :current_password)}
                  class="mt-1 text-sm text-red-600 dark:text-red-400"
                >
                  {translate_error(Keyword.get(@password_changeset.errors, :current_password))}
                </p>
              </div>

              <div>
                <button
                  type="submit"
                  class="flex w-full justify-center rounded-md bg-indigo-600 px-3 py-1.5 text-sm/6 font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400 dark:focus-visible:outline-indigo-500"
                  phx-disable-with="Changing..."
                >
                  Change Password
                </button>
              </div>
            </.simple_form>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def mount(%{"token" => token}, _session, socket) do
    socket =
      case Accounts.update_user_email(socket.assigns.current_user, token) do
        :ok ->
          put_flash(socket, :info, "Email changed successfully.")

        :error ->
          put_flash(socket, :error, "Email change link is invalid or it has expired.")
      end

    {:ok, push_navigate(socket, to: ~p"/users/settings")}
  end

  def mount(_params, _session, socket) do
    user = socket.assigns.current_user

    socket =
      socket
      |> assign(:current_password, nil)
      |> assign(:email_form_current_password, nil)
      |> assign(:callsign_form_current_password, nil)
      |> assign(:current_email, user.email)
      |> assign(:current_callsign, user.callsign)
      |> assign(:email_changeset, Accounts.change_user_email(user))
      |> assign(:callsign_changeset, Accounts.change_user_callsign(user))
      |> assign(:password_changeset, Accounts.change_user_password(user))
      |> assign(:trigger_submit, false)

    {:ok, socket}
  end

  def handle_event("validate_email", params, socket) do
    %{"current_password" => password, "user" => user_params} = params
    email_changeset = Accounts.change_user_email(socket.assigns.current_user, user_params)

    socket =
      assign(socket,
        email_changeset: Map.put(email_changeset, :action, :validate),
        email_form_current_password: password
      )

    {:noreply, socket}
  end

  def handle_event("update_email", params, socket) do
    %{"current_password" => password, "user" => user_params} = params
    user = socket.assigns.current_user

    case Accounts.apply_user_email(user, password, user_params) do
      {:ok, applied_user} ->
        Accounts.deliver_user_update_email_instructions(
          applied_user,
          user.email,
          &url(~p"/users/settings/confirm_email/#{&1}")
        )

        info = "A link to confirm your email change has been sent to the new address."
        {:noreply, put_flash(socket, :info, info)}

      {:error, changeset} ->
        {:noreply, assign(socket, :email_changeset, Map.put(changeset, :action, :insert))}
    end
  end

  def handle_event("validate_callsign", params, socket) do
    %{"current_password" => password, "user" => user_params} = params
    callsign_changeset = Accounts.change_user_callsign(socket.assigns.current_user, user_params)

    socket =
      assign(socket,
        callsign_changeset: Map.put(callsign_changeset, :action, :validate),
        callsign_form_current_password: password
      )

    {:noreply, socket}
  end

  def handle_event("update_callsign", params, socket) do
    %{"current_password" => password, "user" => user_params} = params
    user = socket.assigns.current_user

    case Accounts.update_user_callsign(user, password, user_params) do
      {:ok, _user} ->
        info = "Callsign updated successfully."
        {:noreply, socket |> put_flash(:info, info) |> push_navigate(to: ~p"/users/settings")}

      {:error, changeset} ->
        {:noreply, assign(socket, :callsign_changeset, Map.put(changeset, :action, :insert))}
    end
  end

  def handle_event("validate_password", params, socket) do
    %{"current_password" => password, "user" => user_params} = params
    password_changeset = Accounts.change_user_password(socket.assigns.current_user, user_params)

    {:noreply,
     socket
     |> assign(:password_changeset, Map.put(password_changeset, :action, :validate))
     |> assign(:current_password, password)}
  end

  def handle_event("update_password", params, socket) do
    %{"current_password" => password, "user" => user_params} = params
    user = socket.assigns.current_user

    case Accounts.update_user_password(user, password, user_params) do
      {:ok, user} ->
        socket =
          socket
          |> assign(:trigger_submit, true)
          |> assign(:password_changeset, Accounts.change_user_password(user, user_params))

        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign(socket, :password_changeset, changeset)}
    end
  end
end
