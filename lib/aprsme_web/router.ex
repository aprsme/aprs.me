defmodule AprsmeWeb.Router do
  use AprsmeWeb, :router
  use ErrorTracker.Web, :router

  import AprsmeWeb.UserAuth
  import Phoenix.LiveDashboard.Router

  alias AprsmeWeb.Plugs.RateLimiter

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {AprsmeWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :fetch_current_user
    plug AprsmeWeb.Plugs.SetLocale
    plug RateLimiter, scale: 60_000, limit: 200
  end

  pipeline :public_api do
    plug :accepts, ["json"]
    plug RateLimiter, scale: 60_000, limit: 100
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug RateLimiter, scale: 60_000, limit: 100
    plug AprsmeWeb.Plugs.ApiCSRF
  end

  scope "/", AprsmeWeb do
    pipe_through :public_api
    get "/health", PageController, :health
    get "/ready", PageController, :ready
    get "/status.json", PageController, :status_json
  end

  scope "/", AprsmeWeb do
    pipe_through :browser

    live_session :regular_pages, on_mount: [{AprsmeWeb.LocaleHook, :set_locale}] do
      live "/status", StatusLive.Index, :index
      live "/packets", PacketsLive.Index, :index
      live "/packets/:callsign", PacketsLive.CallsignView, :index
      live "/badpackets", BadPacketsLive.Index, :index
      live "/weather/:callsign", WeatherLive.CallsignView, :index
      live "/about", AboutLive, :index
      live "/api", ApiDocsLive, :index
      live "/info/:callsign", InfoLive.Show, :show
      live "/", MapLive.Index, :index
      live "/:callsign", MapLive.CallsignView, :index
    end
  end

  # API v1 routes
  scope "/api/v1", AprsmeWeb.Api.V1, as: :api_v1 do
    pipe_through :api

    get "/callsign/:callsign", CallsignController, :show
  end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:aprsme, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).

    scope "/dev" do
      pipe_through :browser

      # live_dashboard "/dashboard", metrics: AprsmeWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end

  ## Authentication routes

  scope "/", AprsmeWeb do
    pipe_through [:browser, :redirect_if_user_is_authenticated]

    live_session :redirect_if_user_is_authenticated,
      on_mount: [{AprsmeWeb.UserAuth, :redirect_if_user_is_authenticated}, {AprsmeWeb.LocaleHook, :set_locale}] do
      live "/users/register", UserRegistrationLive, :new
      live "/users/log_in", UserLoginLive, :new
      live "/users/reset_password", UserForgotPasswordLive, :new
      live "/users/reset_password/:token", UserResetPasswordLive, :edit
    end

    post "/users/log_in", UserSessionController, :create
  end

  scope "/", AprsmeWeb do
    pipe_through [:browser, :require_authenticated_user]

    live_dashboard "/dashboard", metrics: AprsmeWeb.Telemetry
    error_tracker_dashboard("/errors")

    live_session :require_authenticated_user,
      on_mount: [{AprsmeWeb.UserAuth, :ensure_authenticated}, {AprsmeWeb.LocaleHook, :set_locale}] do
      live "/users/settings", UserSettingsLive, :edit
      live "/users/settings/confirm_email/:token", UserSettingsLive, :confirm_email
    end
  end

  scope "/", AprsmeWeb do
    pipe_through [:browser]

    delete "/users/log_out", UserSessionController, :delete

    live_session :current_user,
      on_mount: [{AprsmeWeb.UserAuth, :mount_current_user}, {AprsmeWeb.LocaleHook, :set_locale}] do
      live "/users/confirm/:token", UserConfirmationLive, :edit
      live "/users/confirm", UserConfirmationInstructionsLive, :new
    end
  end
end
