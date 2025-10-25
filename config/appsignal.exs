import Config

# Only enable AppSignal in production
# Use config_env() which is available at runtime
env = config_env()
active = env == :prod

config :appsignal, :config,
  otp_app: :aprsme,
  name: "aprsme",
  push_api_key: "070601ca-6102-4214-82ad-d0c40df7f6ee",
  env: env,
  active: active
