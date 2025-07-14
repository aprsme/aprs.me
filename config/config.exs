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
config :aprsme, Aprsme.Mailer, adapter: Swoosh.Adapters.Local

# Configures the endpoint
config :aprsme, AprsmeWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [
    formats: [html: AprsmeWeb.ErrorHTML, json: AprsmeWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Aprsme.PubSub,
  live_view: [signing_salt: "ees098qG"]

# Configure Gettext with supported locales from AprsmeWeb.Gettext module
config :aprsme, AprsmeWeb.Gettext,
  locales: ~w(en es de fr),
  default_locale: "en"

# Configure Oban for background jobs
config :aprsme, Oban,
  repo: Aprsme.Repo,
  plugins: [
    {Oban.Plugins.Pruner, max_age: 60 * 60 * 24 * 7},
    {Oban.Plugins.Cron,
     crontab: [
       {"0 0 * * *", Aprsme.Workers.PacketCleanupWorker}
     ]}
  ],
  queues: [default: 10, maintenance: 2]

config :aprsme,
  ecto_repos: [Aprsme.Repo]

config :aprsme,
  ecto_repos: [Aprsme.Repo],
  aprs_is_server: System.get_env("APRS_SERVER", "dallas.aprs2.net"),
  aprs_is_port: 10_152,
  aprs_is_default_filter: System.get_env("APRS_FILTER"),
  aprs_is_login_id: System.get_env("APRS_CALLSIGN"),
  aprs_is_password: System.get_env("APRS_PASSCODE"),
  auto_migrate: true,
  env: config_env(),
  # Packet retention period in days (default: 365 days = 1 year)
  packet_retention_days: String.to_integer(System.get_env("PACKET_RETENTION_DAYS", "365")),
  # GenStage packet processing configuration
  packet_pipeline: [
    max_buffer_size: 1000,
    batch_size: 100,
    batch_timeout: 1000,
    max_demand: 50
  ]

config :error_tracker,
  repo: Aprsme.Repo,
  otp_app: :aprsme,
  enabled: true

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.24.2",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/* --external:chart.js/auto --define:global.L=window.L),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :gettext, :plural_forms, GettextPseudolocalize.Plural

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configure tailwind (the version is required)
config :tailwind,
  version: "4.0.9",
  default: [
    args: ~w(
      --input=assets/css/app.css
      --output=priv/static/assets/css/app.css
    ),
    cd: Path.expand("..", __DIR__)
  ]

import_config "#{config_env()}.exs"
