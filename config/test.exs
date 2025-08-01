import Config

# In test we don't send emails.
config :aprsme, Aprsme.Mailer, adapter: Swoosh.Adapters.Test

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :aprsme, Aprsme.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "aprsme_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: System.schedulers_online() * 4,
  types: Aprsme.PostgresTypes,
  ownership_timeout: 60_000,
  timeout: 15_000,
  queue_target: 50,
  queue_interval: 100

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :aprsme, AprsmeWeb.Endpoint,
  adapter: Bandit.PhoenixAdapter,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "IV9+ENaw9i8xjReRk4sULRvRgsmFVTGQwQGGrf4G+Q/SFMeHBCNWRlPXQ2YvT36R",
  server: true

# Disable Prometheus telemetry in test mode to avoid port conflicts
config :aprsme, AprsmeWeb.Telemetry, enabled: false

# Disable cleanup scheduler in test environment
config :aprsme, :cleanup_scheduler, enabled: false

# Disable initialize replay delay in test environment
config :aprsme, :initialize_replay_delay, 0

# Configure the packets module to use the mock in tests
config :aprsme, :packets_module, Aprsme.PacketsMock

# Disable APRS-IS external connections in test environment
config :aprsme,
  aprs_is_server: "mock.aprs.test",
  aprs_is_port: 14_580,
  aprs_is_default_filter: "r/33/-96/100",
  aprs_is_login_id: "TEST",
  aprs_is_password: "-1",
  disable_aprs_connection: true

# Disable automatic migrations during tests
config :aprsme, auto_migrate: false

# Only in tests, remove the complexity from the password hashing algorithm
config :bcrypt_elixir, :log_rounds, 1

# Disable ErrorTracker in test environment to avoid database ownership issues
config :error_tracker, enabled: false

# Disable Exq during tests to prevent background job execution
config :exq,
  start_on_application: false,
  mode: :inline

# Configure ExVCR
config :exvcr,
  vcr_cassette_library_dir: "test/fixtures/vcr_cassettes",
  custom_cassette_library_dir: "test/fixtures/vcr_cassettes",
  filter_request_headers: ["Authorization"],
  filter_url_params: false,
  response_headers_blacklist: []

# Configure Hammer for test environment
config :hammer,
  backend: {Hammer.Backend.ETS, [expiry_ms: 60_000 * 60 * 4, cleanup_interval_ms: 60_000 * 10]}

# Disable logging during test for better performance
config :logger, level: :error

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Disable swoosh api client as it is only required for production adapters.
config :swoosh, :api_client, false

# Configure Wallaby
config :wallaby,
  driver: Wallaby.Chrome,
  chromedriver: [
    headless: true,
    # Additional Chrome options for CI stability
    args: [
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--disable-gpu",
      "--window-size=1280,720"
    ]
  ],
  screenshot_on_failure: true,
  screenshot_dir: "test/screenshots",
  base_url: "http://localhost:4002",
  # Increase default timeouts for CI environment
  max_wait_time: "WALLABY_MAX_WAIT_TIME" |> System.get_env("5000") |> String.to_integer(),
  # Pool configuration for concurrent tests
  pool_size: 1
