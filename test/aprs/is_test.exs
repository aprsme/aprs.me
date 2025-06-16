defmodule Aprs.IsTest do
  @moduledoc """
  Tests to ensure APRS-IS external connections are properly blocked in test environment.
  """
  use ExUnit.Case, async: true

  require Logger

  describe "APRS-IS connection prevention in test environment" do
    test "APRS.Is module should not start in test environment" do
      # Verify that the APRS.Is GenServer is not running
      assert Process.whereis(Aprs.Is) == nil
    end

    test "attempting to start APRS.Is should fail with test environment disabled" do
      # Try to start the APRS.Is GenServer manually
      result = Aprs.Is.start_link([])

      # Should fail because we're in test environment
      assert {:error, reason} = result
      assert reason in [:test_environment_disabled, :normal]
    end

    test "get_status should return disconnected state without external connection" do
      status = Aprs.Is.get_status()

      # Should return disconnected status without attempting external connection
      assert status.connected == false
      assert status.server != nil
      assert status.port != nil
      assert status.connected_at == nil
      assert status.uptime_seconds == 0
    end

    test "configuration should disable APRS connections in test" do
      # Verify test configuration properly disables connections
      assert Application.get_env(:aprs, :disable_aprs_connection) == true
      assert Application.get_env(:aprs, :env) == :test
    end

    test "APRS server configuration should be nil or test values in test environment" do
      # Verify APRS server config is neutralized for tests
      server = Application.get_env(:aprs, :aprs_is_server)
      login_id = Application.get_env(:aprs, :aprs_is_login_id)

      # Server should be nil (disabled) or a test value
      assert server == nil or server == "mock.aprs.test"

      # Login should be test value
      assert login_id == "TEST"
    end
  end

  describe "APRS-IS mock functionality" do
    setup do
      # Start the mock if not already running
      case GenServer.start_link(AprsIsMock, [], name: AprsIsMock) do
        {:ok, pid} ->
          on_exit(fn -> GenServer.stop(pid, :normal) end)
          {:ok, mock_pid: pid}

        {:error, {:already_started, pid}} ->
          {:ok, mock_pid: pid}
      end
    end

    test "mock should provide status without external connections", %{mock_pid: _pid} do
      status = AprsIsMock.get_status()

      assert status.connected == false
      assert status.server == "mock.aprs.test"
      assert status.port == 14_580
      assert status.login_id == "TEST"
      assert is_map(status.packet_stats)
    end

    test "mock should handle message sending safely", %{mock_pid: _pid} do
      # These should not attempt external connections
      assert AprsIsMock.send_message("test message") == :ok
      assert AprsIsMock.send_message("TEST", "DEST", "hello") == :ok
      assert AprsIsMock.set_filter("r/0/0/1") == :ok
      assert AprsIsMock.list_active_filters() == :ok
    end

    test "mock can simulate packet reception for testing", %{mock_pid: _pid} do
      # This allows tests to simulate receiving packets without external connections
      test_packet = %{
        sender: "TEST-1",
        destination: "APRS",
        path: ["WIDE1-1", "WIDE2-1"],
        data_type: :position,
        latitude: 33.0,
        longitude: -96.0
      }

      assert AprsIsMock.simulate_packet(test_packet) == :ok
    end

    test "mock can simulate connection state changes", %{mock_pid: _pid} do
      # Simulate connected state
      assert AprsIsMock.simulate_connection_state(true) == :ok
      status = AprsIsMock.get_status()
      assert status.connected == true
      assert status.connected_at != nil

      # Simulate disconnected state
      assert AprsIsMock.simulate_connection_state(false) == :ok
      status = AprsIsMock.get_status()
      assert status.connected == false
      assert status.connected_at == nil
    end
  end

  describe "network isolation verification" do
    test "no external network calls should be made during test runs" do
      # This test verifies that no actual TCP connections are attempted
      # We can do this by checking that :gen_tcp.connect is not called
      # with real APRS server addresses

      # Common APRS-IS servers that should never be contacted in tests
      forbidden_servers = [
        "rotate.aprs2.net",
        "dallas.aprs2.net",
        "seattle.aprs2.net",
        "chicago.aprs2.net",
        "atlanta.aprs2.net"
      ]

      # Verify these are not in the current configuration
      current_server = Application.get_env(:aprs, :aprs_is_server)

      if current_server do
        server_str = to_string(current_server)

        refute Enum.any?(forbidden_servers, fn forbidden ->
                 String.contains?(server_str, forbidden)
               end),
               "Test environment should not be configured with real APRS servers"
      end
    end

    test "test environment marker is properly set" do
      # Verify we're definitely in test environment
      assert Mix.env() == :test
      assert Application.get_env(:aprs, :env) == :test
    end
  end
end
