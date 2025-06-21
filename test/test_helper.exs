ExUnit.start()
Ecto.Adapters.SQL.Sandbox.mode(Aprs.Repo, :manual)

# Configure Mox
Mox.defmock(Aprs.PacketsMock, for: Aprs.PacketsBehaviour)
Mox.defmock(Aprs.PacketReplayMock, for: Aprs.PacketReplayBehaviour)

# Ensure no external APRS connections during tests
Application.put_env(:aprs, :disable_aprs_connection, true)
Application.put_env(:aprs, :aprs_is_server, "mock.aprs.test")
Application.put_env(:aprs, :aprs_is_port, 14_580)
Application.put_env(:aprs, :aprs_is_login_id, "TEST")
Application.put_env(:aprs, :aprs_is_password, "-1")
Application.put_env(:aprs, :aprs_is_default_filter, "r/0/0/1")

# AprsIsMock is automatically loaded from test/support via elixirc_paths
