defmodule AprsmeWeb.UserForgotPasswordLive do
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
              <h1 class="card-title text-3xl mb-4 justify-center">Forgot your password?</h1>
              <p class="text-base-content/70 mb-6">We'll send a password reset link to your inbox</p>

              <.simple_form
                :let={_f}
                id="reset_password_form"
                for={%{}}
                as={:user}
                phx-submit="send_email"
              >
                <div class="form-control w-full">
                  <label class="label">
                    <span class="label-text">Email</span>
                  </label>
                  <input
                    type="email"
                    name="user[email]"
                    class="input input-bordered w-full bg-base-100 text-base-content"
                    placeholder="Enter your email"
                    required
                  />
                </div>

                <div class="form-control mt-6">
                  <button type="submit" class="btn btn-primary w-full" phx-disable-with="Sending...">
                    Send password reset instructions
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

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  def handle_event("send_email", %{"user" => %{"email" => email}}, socket) do
    if user = Accounts.get_user_by_email(email) do
      Accounts.deliver_user_reset_password_instructions(
        user,
        &url(~p"/users/reset_password/#{&1}")
      )
    end

    info =
      "If your email is in our system, you will receive instructions to reset your password shortly."

    {:noreply,
     socket
     |> put_flash(:info, info)
     |> redirect(to: ~p"/")}
  end
end
