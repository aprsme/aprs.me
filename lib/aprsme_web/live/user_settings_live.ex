defmodule AprsmeWeb.UserSettingsLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts

  def render(assigns) do
    ~H"""
    <div class="hero min-h-screen bg-base-200">
      <div class="hero-content flex-col w-full max-w-4xl">
        <div class="text-center mb-8">
          <h1 class="text-4xl font-bold">Account Settings</h1>
          <p class="text-base-content/70">Update your email address and password</p>
        </div>

        <div class="grid grid-cols-1 md:grid-cols-2 gap-8 w-full">
          <!-- Change Email Section -->
          <div class="card bg-base-100 shadow-xl">
            <div class="card-body">
              <h2 class="card-title text-2xl mb-4">Change Email</h2>

              <.simple_form
                :let={f}
                id="email_form"
                for={@email_changeset}
                phx-submit="update_email"
                phx-change="validate_email"
              >
                <div :if={@email_changeset.action == :insert} class="alert alert-error mb-4">
                  <span>Oops, something went wrong! Please check the errors below.</span>
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">Email</span>
                  </label>
                  <input
                    type="email"
                    name={Phoenix.HTML.Form.input_name(f, :email)}
                    value={Phoenix.HTML.Form.input_value(f, :email) || ""}
                    class="input input-bordered w-full bg-base-100 text-base-content"
                    placeholder="Enter your email"
                    required
                  />
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">Current password</span>
                  </label>
                  <input
                    type="password"
                    name="current_password"
                    id="current_password_for_email"
                    value={@email_form_current_password || ""}
                    class="input input-bordered w-full bg-base-100 text-base-content"
                    placeholder="Enter current password"
                    required
                  />
                </div>

                <div class="form-control mt-6">
                  <button type="submit" class="btn btn-primary w-full" phx-disable-with="Changing...">
                    Change Email
                  </button>
                </div>
              </.simple_form>
            </div>
          </div>
          
    <!-- Change Password Section -->
          <div class="card bg-base-100 shadow-xl">
            <div class="card-body">
              <h2 class="card-title text-2xl mb-4">Change Password</h2>

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
                <div :if={@password_changeset.action == :insert} class="alert alert-error mb-4">
                  <span>Oops, something went wrong! Please check the errors below.</span>
                </div>

                <input
                  type="hidden"
                  name={Phoenix.HTML.Form.input_name(f, :email)}
                  value={@current_email}
                />

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

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">Current password</span>
                  </label>
                  <input
                    type="password"
                    name="current_password"
                    id="current_password_for_password"
                    value={@current_password || ""}
                    class="input input-bordered w-full bg-base-100 text-base-content"
                    placeholder="Enter current password"
                    required
                  />
                </div>

                <div class="form-control mt-6">
                  <button type="submit" class="btn btn-primary w-full" phx-disable-with="Changing...">
                    Change Password
                  </button>
                </div>
              </.simple_form>
            </div>
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
      |> assign(:current_email, user.email)
      |> assign(:email_changeset, Accounts.change_user_email(user))
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
