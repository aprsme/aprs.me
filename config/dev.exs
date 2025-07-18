import Config

# Configure your database
config :aprsme, Aprsme.Repo,
  # For development, we disable any cache and enable
  # debugging and code reloading.
  #
  # The watchers configuration can be used to run external
  # watchers to your application. For example, we use it
  # with esbuild to bundle .js and .css sources.
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "aprsme_dev",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  # Optimized pool settings for development
  pool_size: 15,
  pool_timeout: 10_000,
  timeout: 30_000,
  queue_target: 100,
  queue_interval: 1_000,
  #  log: :debug,
  log: false,
  types: Aprsme.PostgresTypes,
  # Use unnamed prepared statements for better development flexibility
  prepare: :unnamed,
  # Socket options for better performance
  socket_options: [
    keepalive: true,
    nodelay: true
  ],
  # Development-specific parameters matching PostgreSQL config
  parameters: [
    application_name: "aprsme_dev",
    work_mem: "16MB",
    statement_timeout: "60s"
  ]

config :aprsme, AprsmeWeb.Endpoint,
  # Binding to loopback ipv4 address prevents access from other machines.
  # Change to `ip: {0, 0, 0, 0}` to allow access from other machines.
  adapter: Bandit.PhoenixAdapter,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "Vv8Uh9w7tSoac1wBo7A8MyTSE9J+RzNeAzPTGcsW2InUBHpPBgt1dACJrIZorfdH",
  watchers: [
    esbuild:
      {Esbuild, :install_and_run,
       [
         :default,
         ~w(--sourcemap=inline --watch --loader:.ts=ts)
       ]},
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]}
  ]

# Watch static and templates for browser reloading.
config :aprsme, AprsmeWeb.Endpoint,
  live_reload: [
    web_console_logger: true,
    patterns: [
      ~r"priv/static/(?!uploads/).*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/temp_web/(?:controllers|live|components|router)/?.*\.(ex|heex)$"
    ]
  ]

# Enable dev routes for dashboard and mailbox
#
# Run `mix help phx.gen.cert` for more information.
config :aprsme, dev_routes: true

# Mailer configuration for development
# Using Swoosh.Adapters.Local from config.exs which stores emails locally
# Access sent emails at http://localhost:4000/dev/mailbox

# Configure Hammer for development environment
config :hammer,
  backend: {Hammer.Backend.ETS, [expiry_ms: 60_000 * 60 * 4, cleanup_interval_ms: 60_000 * 10]}

# Do not include metadata nor timestamps in development logs
#
# The `http:` config above can be replaced with:
#
#     https: [
config :logger, :console, format: "[$level] $message\n"

# Initialize plugs at runtime for faster development compilation
#       port: 4001,
#       cipher_suite: :strong,
#       keyfile: "priv/cert/selfsigned_key.pem",
config :phoenix, :plug_init_mode, :runtime

# Set a higher stacktrace during development. Avoid configuring such
#       certfile: "priv/cert/selfsigned.pem"
# Disable swoosh api client as it is only required for production adapters.
# in production as building large stacktraces may be expensive.
# different ports.

#     ],
#
# If desired, both `http:` and `https:` keys can be
# configured to run both http and https servers on
config :phoenix, :stacktrace_depth, 20

config :swoosh, :api_client, false
