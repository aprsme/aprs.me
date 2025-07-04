ExUnit.start()
Ecto.Adapters.SQL.Sandbox.mode(Aprsme.Repo, :manual)

# Configure Mox
Mox.defmock(Aprsme.PacketsMock, for: Aprsme.PacketsBehaviour)
Mox.defmock(Aprsme.PacketReplayMock, for: Aprsme.PacketReplayBehaviour)
Mox.defmock(PacketsMock, for: Aprsme.PacketsBehaviour)

# Ensure no external APRS connections during tests
Application.put_env(:aprsme, :disable_aprs_connection, true)
Application.put_env(:aprsme, :aprs_is_server, "mock.aprs.test")
Application.put_env(:aprsme, :aprsme_is_port, 14_580)
Application.put_env(:aprsme, :aprsme_is_login_id, "TEST")
Application.put_env(:aprsme, :aprsme_is_password, "-1")
Application.put_env(:aprsme, :aprsme_is_default_filter, "r/0/0/1")
Application.put_env(:aprsme, :packets_module, Aprsme.PacketsMock)

# AprsIsMock is automatically loaded from test/support via elixirc_paths
