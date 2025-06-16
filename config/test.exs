import Config

# In test we don't send emails.
config :aprs, Aprs.Mailer, adapter: Swoosh.Adapters.Test

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :aprs, Aprs.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "aprs_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10,
  types: Aprs.PostgresTypes

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :aprs, AprsWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "IV9+ENaw9i8xjReRk4sULRvRgsmFVTGQwQGGrf4G+Q/SFMeHBCNWRlPXQ2YvT36R",
  server: false

# Disable APRS-IS external connections in test environment
config :aprs,
  aprs_is_server: nil,
  aprs_is_port: nil,
  aprs_is_default_filter: nil,
  aprs_is_login_id: nil,
  aprs_is_password: nil,
  disable_aprs_connection: true

# Disable automatic migrations during tests
config :aprs, auto_migrate: false

# Only in tests, remove the complexity from the password hashing algorithm
config :bcrypt_elixir, :log_rounds, 1

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Disable swoosh api client as it is only required for production adapters.
config :swoosh, :api_client, false
