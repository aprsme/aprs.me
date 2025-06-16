ExUnit.start()
Ecto.Adapters.SQL.Sandbox.mode(Aprs.Repo, :manual)

# Ensure no external APRS connections during tests
Application.put_env(:aprs, :disable_aprs_connection, true)
Application.put_env(:aprs, :aprs_is_server, nil)
Application.put_env(:aprs, :aprs_is_port, nil)
Application.put_env(:aprs, :aprs_is_login_id, "TEST")
Application.put_env(:aprs, :aprs_is_password, "-1")
Application.put_env(:aprs, :aprs_is_default_filter, "r/0/0/1")

# Load the APRS mock for testing
Code.require_file("support/aprs_is_mock.ex", __DIR__)
