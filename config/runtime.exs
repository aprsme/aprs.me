import Config

# config/runtime.exs is executed for all environments, including
# during releases. It is executed after compilation and before the
# system starts, so it is typically used to load production configuration
# and secrets from environment variables or elsewhere. Do not define
# any compile-time configuration in here, as it won't be applied.
# The block below contains prod specific runtime configuration.

# ## Using releases
#
# If you use `mix release`, you need to explicitly enable the server
# by passing the PHX_SERVER=true when you start it:
#
#     PHX_SERVER=true bin/aprsme start
#
# Alternatively, you can use `mix phx.gen.release` to generate a `bin/server`
# script that automatically sets the env var above.
# Always start the server in production/docker environments
if System.get_env("PHX_SERVER") || config_env() == :prod do
  config :aprsme, AprsmeWeb.Endpoint, server: true
end

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  maybe_ipv6 = if System.get_env("ECTO_IPV6") in ~w(true 1), do: [:inet6], else: []

  # The secret key base is used to sign/encrypt cookies and other secrets.
  # A default value is used in config/dev.exs and config/test.exs but you
  # want to use a different value for prod and you most likely don't want
  # to check this value into version control, so we use an environment
  # variable instead.
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  host = System.get_env("PHX_HOST") || "example.com"
  port = String.to_integer(System.get_env("PORT") || "4000")

  config :aprsme, Aprsme.Repo,
    # ssl: true,
    url: database_url,
    # Optimized for max_connections=100 with other apps on server
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "25"),
    # Increased timeout for ARM system under load
    pool_timeout: String.to_integer(System.get_env("POOL_TIMEOUT") || "10000"),
    # Match PostgreSQL statement timeout capabilities
    timeout: String.to_integer(System.get_env("DB_TIMEOUT") || "30000"),
    socket_options:
      maybe_ipv6 ++
        [
          # TCP optimizations for better connection handling
          keepalive: true,
          nodelay: true,
          recbuf: 8192,
          sndbuf: 8192
        ],
    types: Aprsme.PostgresTypes,
    # Reduced queue target for faster response
    queue_target: 100,
    # Check queue more frequently
    queue_interval: 1_000,
    # Optimize for unnamed prepared statements (better for dynamic queries)
    prepare: :unnamed,
    # Connection parameters to leverage PostgreSQL settings
    parameters: [
      # Application name for monitoring
      application_name: "aprsme",
      # Leverage synchronous_commit=off in postgresql.conf
      synchronous_commit: "off",
      # Match work_mem setting
      work_mem: "16MB",
      # Statement timeout as safety net
      statement_timeout: "30s",
      # Prevent idle transactions
      idle_in_transaction_session_timeout: "60s"
    ]

  config :aprsme, AprsmeWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      # Bind on all IPv4 interfaces for better container compatibility
      # Use {0, 0, 0, 0} for IPv4 or {0, 0, 0, 0, 0, 0, 0, 0} for IPv6
      # See the documentation on https://hexdocs.pm/plug_cowboy/Plug.Cowboy.html
      # for details about using IPv6 vs IPv4 and loopback vs public addresses.
      ip: {0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base,
    server: true,
    check_origin: ["https://#{host}", "http://10.0.19.222:33897", "https://s.aprs.me"]

  config :aprsme,
    ecto_repos: [Aprsme.Repo],
    aprs_is_server: System.get_env("APRS_SERVER", "dallas.aprs2.net"),
    # config :aprsme, :dns_cluster_query, System.get_env("DNS_CLUSTER_QUERY")
    aprs_is_port: 14_580,
    aprs_is_default_filter: System.get_env("APRS_FILTER"),
    aprs_is_login_id: System.get_env("APRS_CALLSIGN"),
    aprs_is_password: System.get_env("APRS_PASSCODE"),
    env: :prod

  # Configure Hammer for production environment
  config :hammer,
    backend: {Hammer.Backend.ETS, [expiry_ms: 60_000 * 60 * 4, cleanup_interval_ms: 60_000 * 10]}

  # Configure libcluster for Dokku clustering
  config :libcluster,
    topologies: [
      dokku: [
        strategy: Cluster.Strategy.Gossip,
        config: [
          port: 45_892,
          if_addr: "0.0.0.0",
          multicast_addr: "230.1.1.1",
          multicast_ttl: 1
        ]
      ]
    ]

  # ## Configuring the mailer
  #
  # In production you need to configure the mailer to use a different adapter.
  # Also, you may need to configure the Swoosh API client of your choice if you
  # are not using SMTP. Here is an example of the configuration:
  #
  #     config :aprsme, Aprsme.Mailer,
  #       adapter: Swoosh.Adapters.Mailgun,
  #       api_key: System.get_env("MAILGUN_API_KEY"),
  #       domain: System.get_env("MAILGUN_DOMAIN")
  #
  # For this example you need include a HTTP client required by Swoosh API client.
  # Swoosh supports Hackney, Finch, and Req out of the box:
  #
  #     config :swoosh, :api_client, Swoosh.ApiClient.Hackney
  #
  # See https://hexdocs.pm/swoosh/Swoosh.html#module-installation for details.
end
