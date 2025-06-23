# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the `config/runtime.exs`.
config :aprs, Aprs.Mailer, adapter: Swoosh.Adapters.Local

# Configures the endpoint
config :aprs, AprsWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [
    formats: [html: AprsWeb.ErrorHTML, json: AprsWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Aprs.PubSub,
  live_view: [signing_salt: "ees098qG"]

# Configure Oban for background jobs
config :aprs, Oban,
  repo: Aprs.Repo,
  plugins: [
    {Oban.Plugins.Pruner, max_age: 60 * 60 * 24 * 7},
    {Oban.Plugins.Cron,
     crontab: [
       {"0 0 * * *", Aprs.Workers.PacketCleanupWorker},
       {"0 3 * * 1", Aprs.DeviceIdentification.Worker}
     ]}
  ],
  queues: [default: 10, maintenance: 2]

config :aprs,
  ecto_repos: [Aprs.Repo]

config :aprs,
  ecto_repos: [Aprs.Repo],
  aprs_is_server: System.get_env("APRS_SERVER", "dallas.aprs2.net"),
  aprs_is_port: 14_580,
  aprs_is_default_filter: System.get_env("APRS_FILTER"),
  aprs_is_login_id: System.get_env("APRS_CALLSIGN"),
  aprs_is_password: System.get_env("APRS_PASSCODE"),
  auto_migrate: true,
  env: config_env()

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.24.2",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/* --define:global.L=window.L),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configure tailwind (the version is required)
config :tailwind,
  version: "3.3.2",
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    # Import environment specific config. This must remain at the bottom
    # of this file so it overrides the configuration defined above.
    cd: Path.expand("../assets", __DIR__)
  ]

import_config "#{config_env()}.exs"
