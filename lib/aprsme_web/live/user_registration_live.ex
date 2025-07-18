defmodule AprsmeWeb.UserRegistrationLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  import AprsmeWeb.CoreComponents, only: [translate_error: 1, simple_form: 1]

  alias Aprsme.Accounts
  alias Aprsme.Accounts.User

  def render(assigns) do
    ~H"""
    <div class="hero min-h-screen bg-base-200">
      <div class="hero-content text-center">
        <div class="max-w-md">
          <div class="card bg-base-100 shadow-xl">
            <div class="card-body">
              <h1 class="card-title text-3xl mb-4 justify-center">{gettext("Register for an account")}</h1>
              <p class="text-base-content/70 mb-6">
                {gettext("Already registered?")}
                <.link navigate={~p"/users/log_in"} class="link link-primary">
                  {gettext("Sign in")}
                </.link>
                {gettext("to your account now.")}
              </p>

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
                <div :if={@changeset.action == :insert} class="alert alert-error mb-4">
                  <span>{gettext("Oops, something went wrong! Please check the errors below.")}</span>
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">{gettext("Email")}</span>
                  </label>
                  <input
                    type="email"
                    name={Phoenix.HTML.Form.input_name(f, :email)}
                    value={Phoenix.HTML.Form.input_value(f, :email) || ""}
                    class={[
                      "input input-bordered w-full bg-base-100 text-base-content",
                      Keyword.has_key?(@changeset.errors, :email) && "input-error"
                    ]}
                    placeholder={gettext("Enter your email")}
                    required
                  />
                  <label :if={Keyword.has_key?(@changeset.errors, :email)} class="label">
                    <span class="label-text-alt text-error">
                      {translate_error(Keyword.get(@changeset.errors, :email))}
                    </span>
                  </label>
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">{gettext("Callsign")}</span>
                  </label>
                  <input
                    type="text"
                    name={Phoenix.HTML.Form.input_name(f, :callsign)}
                    value={Phoenix.HTML.Form.input_value(f, :callsign) || ""}
                    class={[
                      "input input-bordered w-full bg-base-100 text-base-content",
                      Keyword.has_key?(@changeset.errors, :callsign) && "input-error"
                    ]}
                    placeholder={gettext("Enter your amateur radio callsign")}
                    required
                  />
                  <label :if={Keyword.has_key?(@changeset.errors, :callsign)} class="label">
                    <span class="label-text-alt text-error">
                      {translate_error(Keyword.get(@changeset.errors, :callsign))}
                    </span>
                  </label>
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">{gettext("Password")}</span>
                  </label>
                  <input
                    type="password"
                    name={Phoenix.HTML.Form.input_name(f, :password)}
                    value={Phoenix.HTML.Form.input_value(f, :password) || ""}
                    class={[
                      "input input-bordered w-full bg-base-100 text-base-content",
                      Keyword.has_key?(@changeset.errors, :password) && "input-error"
                    ]}
                    placeholder={gettext("Enter your password")}
                    required
                  />
                  <label :if={Keyword.has_key?(@changeset.errors, :password)} class="label">
                    <span class="label-text-alt text-error">
                      {translate_error(Keyword.get(@changeset.errors, :password))}
                    </span>
                  </label>
                </div>

                <div class="form-control mt-6">
                  <button type="submit" class="btn btn-primary w-full" phx-disable-with={gettext("Creating account...")}>
                    {gettext("Create an account")}
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
