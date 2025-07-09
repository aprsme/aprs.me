defmodule AprsmeWeb.UserLoginLive do
  @moduledoc false
  use AprsmeWeb, :live_view

  alias Aprsme.Accounts
  alias Aprsme.Accounts.User

  def render(assigns) do
    ~H"""
    <div class="hero min-h-screen bg-base-200">
      <div class="hero-content text-center">
        <div class="max-w-md">
          <div class="card bg-base-100 shadow-xl">
            <div class="card-body">
              <h1 class="card-title text-3xl mb-4 justify-center">Sign in to account</h1>
              <p class="text-base-content/70 mb-6">
                Don't have an account?
                <.link navigate={~p"/users/register"} class="link link-primary">
                  Sign up
                </.link>
                for an account now.
              </p>

              <.simple_form
                :let={f}
                id="login_form"
                for={@changeset}
                action={~p"/users/log_in"}
                as={:user}
                phx-update="ignore"
              >
                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">Email</span>
                  </label>
                  <input
                    type="email"
                    name={Phoenix.HTML.Form.input_name(f, :email)}
                    value={Phoenix.HTML.Form.input_value(f, :email) || ""}
                    class="input input-bordered w-full"
                    placeholder="Enter your email"
                    required
                  />
                </div>

                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">Password</span>
                  </label>
                  <input
                    type="password"
                    name={Phoenix.HTML.Form.input_name(f, :password)}
                    value={Phoenix.HTML.Form.input_value(f, :password) || ""}
                    class="input input-bordered w-full"
                    placeholder="Enter your password"
                    required
                  />
                </div>

                <div class="form-control">
                  <label class="label cursor-pointer justify-start">
                    <input
                      type="checkbox"
                      name={Phoenix.HTML.Form.input_name(f, :remember_me)}
                      value="true"
                      class="checkbox checkbox-primary mr-2"
                    />
                    <span class="label-text">Keep me logged in</span>
                  </label>
                </div>

                <div class="text-center mb-4">
                  <.link navigate={~p"/users/reset_password"} class="link link-primary text-sm">
                    Forgot your password?
                  </.link>
                </div>

                <div class="form-control mt-6">
                  <button
                    type="submit"
                    class="btn btn-primary w-full"
                    phx-disable-with="Signing in..."
                  >
                    Sign in →
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
    {:ok, assign(socket, changeset: changeset), temporary_assigns: [changeset: nil]}
  end
end
