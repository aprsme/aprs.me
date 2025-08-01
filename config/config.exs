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

# Configure periodic cleanup job
config :aprsme, :cleanup_scheduler,
  enabled: true,
  # 6 hours in milliseconds
  interval: 6 * 60 * 60 * 1000

# Configure position tracking sensitivity
config :aprsme, :position_tracking,
  # Position change threshold in degrees (~100 meters at equator)
  change_threshold: 0.001

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
  # Optimized for PostgreSQL with work_mem=16MB and synchronous_commit=off
  packet_pipeline: [
    # Larger buffer since inserts are async
    max_buffer_size: 5000,
    # Smaller batch size to reduce memory pressure
    batch_size: 100,
    # 1 second timeout for smaller batches
    batch_timeout: 1000,
    # Adjust demand for smaller batches
    max_demand: 300,
    # Number of parallel consumers for better throughput
    num_consumers: 3
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
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/* --loader:.css=css --loader:.png=file --loader:.svg=file),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ],
  vendor: [
    args:
      ~w(js/vendor.js --bundle --target=es2017 --outdir=../priv/static/assets --minify --loader:.css=css --loader:.png=file --loader:.svg=file),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ],
  # Optimized CSS bundle
  vendor_css: [
    args: ~w(vendor/css/minimal-bundle.css --outdir=../priv/static/assets/vendor/css --minify),
    cd: Path.expand("../assets", __DIR__)
  ],
  # Core bundle - always loaded
  core_js: [
    args: ~w(vendor/js/core-bundle.js --outdir=../priv/static/assets/vendor/js --minify --target=es2017),
    cd: Path.expand("../assets", __DIR__)
  ],
  # Map bundle - conditional loading
  map_js: [
    args:
      ~w(js/map-bundle-entry.js --bundle --outfile=../priv/static/assets/vendor/js/map-bundle.js --minify --target=es2017),
    cd: Path.expand("../assets", __DIR__)
  ],
  # Chart bundle - conditional loading  
  chart_js: [
    args:
      ~w(vendor/js/chart-minimal.js --outfile=../priv/static/assets/vendor/js/chart-bundle.js --minify --target=es2017),
    cd: Path.expand("../assets", __DIR__)
  ],
  # Date adapter - separate file
  date_adapter: [
    args:
      ~w(vendor/js/date-adapter.js --outfile=../priv/static/assets/vendor/js/date-adapter.js --minify --target=es2017),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configure Exq for background jobs
# Redis connection settings are configured in runtime.exs
config :exq,
  name: Exq,
  concurrency: :infinite,
  queues: [
    {"default", 10},
    {"maintenance", 2}
  ],
  scheduler_enable: true,
  scheduler_poll_timeout: 200,
  poll_timeout: 50,
  redis_timeout: 5000

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
