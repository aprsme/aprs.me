defmodule Aprs.ConnectionPreventionTest do
  @moduledoc """
  Simple test to verify APRS connections are prevented in test environment.
  """
  use ExUnit.Case, async: false
  use Aprs.DataCase

  test "APRS connection is disabled in test environment" do
    # Verify we're in test environment
    assert Mix.env() == :test
    assert Application.get_env(:aprs, :env) == :test

    # Verify APRS connection is explicitly disabled
    assert Application.get_env(:aprs, :disable_aprs_connection) == true

    # Verify APRS.Is process is not running
    assert Process.whereis(Aprs.Is) == nil
  end

  test "APRS configuration is safe for testing" do
    # Verify server configuration is neutralized
    server = Application.get_env(:aprs, :aprs_is_server)
    assert server == nil or server == "mock.aprs.test"

    # Verify test credentials are used
    login_id = Application.get_env(:aprs, :aprs_is_login_id)
    assert login_id == "TEST"

    # Verify no real APRS servers in config
    forbidden_servers = [
      "rotate.aprs2.net",
      "dallas.aprs2.net",
      "seattle.aprs2.net"
    ]

    if server do
      server_str = to_string(server)

      refute Enum.any?(forbidden_servers, fn forbidden ->
               String.contains?(server_str, forbidden)
             end)
    end
  end

  test "get_status works without external connection" do
    # This should not attempt external connections
    status = Aprs.Is.get_status()

    # Should return disconnected state
    assert status.connected == false
    assert status.uptime_seconds == 0
    assert status.connected_at == nil
  end
end
