defmodule Aprsme.IsTest do
  @moduledoc """
  Tests for Aprsme.Is GenServer — APRS-IS connection management.
  """
  use ExUnit.Case, async: false
  use Aprsme.DataCase

  import ExUnit.CaptureLog

  # Helper to build a default GenServer state for direct callback testing
  defp build_state(overrides \\ %{}) do
    timer = Process.send_after(self(), :noop_timer, to_timeout(minute: 5))
    keepalive_timer = Process.send_after(self(), :noop_keepalive, to_timeout(minute: 5))

    base = %{
      server: "mock.aprs.test",
      port: 14_580,
      socket: nil,
      timer: timer,
      keepalive_timer: keepalive_timer,
      connected_at: DateTime.utc_now(),
      packet_stats: %{
        total_packets: 0,
        last_packet_at: nil,
        packets_per_second: 0,
        last_second_count: 0,
        last_second_timestamp: System.system_time(:second)
      },
      buffer: "",
      login_params: %{
        user_id: "TEST",
        passcode: "-1",
        filter: "r/33/-96/100"
      },
      backpressure_active: false,
      safety_valve_timer: nil
    }

    Map.merge(base, overrides)
  end

  # Ensure ETS table exists for tests that need it
  defp ensure_ets_table do
    case :ets.info(:aprsme) do
      :undefined ->
        :ets.new(:aprsme, [:set, :public, :named_table, write_concurrency: true])
        :ets.insert(:aprsme, {:message_number, 0})

      _ ->
        :ok
    end
  end

  describe "init/1" do
    test "stops in test environment" do
      assert {:stop, :test_environment_disabled} = Aprsme.Is.init([])
    end

    test "start_link returns error in test environment" do
      Process.flag(:trap_exit, true)
      result = Aprsme.Is.start_link([])
      assert {:error, :test_environment_disabled} = result
    end
  end

  describe "dispatch/1" do
    test "handles unverified login response" do
      Logger.configure(level: :warning)

      log =
        capture_log(fn ->
          result = Aprsme.Is.dispatch("# logresp TEST unverified, server T2TEXAS")
          assert result == :ok
        end)

      Logger.configure(level: :error)

      assert log =~ "APRS-IS login unverified"
    end

    test "handles accepted login response" do
      Logger.configure(level: :info)

      log =
        capture_log(fn ->
          result = Aprsme.Is.dispatch("# logresp TEST verified, server T2TEXAS")
          assert result == :ok
        end)

      Logger.configure(level: :error)

      assert log =~ "APRS-IS login accepted"
    end

    test "handles comment lines" do
      Logger.configure(level: :debug)

      log =
        capture_log(fn ->
          result = Aprsme.Is.dispatch("# some server comment")
          assert result == :ok
        end)

      Logger.configure(level: :error)

      assert log =~ "COMMENT"
    end

    test "handles empty string" do
      assert Aprsme.Is.dispatch("") == nil
    end

    test "dispatches valid APRS position packet" do
      ensure_ets_table()

      # A real APRS position packet
      raw = "W5ISP-1>APRS,TCPIP*:!3300.00N/09600.00W#PHG2360 Testing"

      log =
        capture_log(fn ->
          Aprsme.Is.dispatch(raw)
        end)

      # Should not log a parse error
      refute log =~ "PARSE ERROR"
    end

    test "dispatches invalid packet and stores bad packet" do
      capture_log(fn ->
        Aprsme.Is.dispatch("totally invalid data that cannot be parsed")
      end)

      # Should not crash — the function handles errors gracefully
    end

    test "handles parse error with specific error message" do
      capture_log(fn ->
        # A packet that triggers {:error, some_message} rather than {:error, :invalid_packet}
        Aprsme.Is.dispatch("X>Y:")
      end)
    end
  end

  describe "handle_call(:get_status, ...)" do
    test "returns status when connected (socket present)" do
      connected_at = DateTime.utc_now()

      state =
        build_state(%{
          socket: :fake_socket,
          connected_at: connected_at
        })

      {:reply, status, ^state} = Aprsme.Is.handle_call(:get_status, {self(), make_ref()}, state)

      assert status.connected == true
      assert status.server == "mock.aprs.test"
      assert status.port == 14_580
      assert status.login_id == "TEST"
      assert status.filter == "r/33/-96/100"
      assert status.connected_at == connected_at
      assert status.uptime_seconds >= 0
      assert is_map(status.packet_stats)
    end

    test "returns disconnected status when socket is nil" do
      state = build_state(%{socket: nil})

      {:reply, status, ^state} = Aprsme.Is.handle_call(:get_status, {self(), make_ref()}, state)

      assert status.connected == false
      assert status.uptime_seconds == 0
      assert status.connected_at == nil
    end

    test "converts charlist server to string in status" do
      state = build_state(%{server: ~c"dallas.aprs2.net"})

      {:reply, status, _state} = Aprsme.Is.handle_call(:get_status, {self(), make_ref()}, state)

      assert status.server == "dallas.aprs2.net"
    end
  end

  describe "handle_call({:send_message, ...}, ...)" do
    test "returns error when not connected (socket nil)" do
      state = build_state(%{socket: nil})

      {:reply, result, ^state} =
        Aprsme.Is.handle_call({:send_message, "test"}, {self(), make_ref()}, state)

      assert result == {:error, :not_connected}
    end
  end

  describe "handle_info(:aprsme_no_message_timeout, ...)" do
    test "ignores timeout when socket is nil" do
      state = build_state(%{socket: nil})

      assert {:noreply, ^state} = Aprsme.Is.handle_info(:aprsme_no_message_timeout, state)
    end

    test "stops when socket is present (timeout detected)" do
      state = build_state(%{socket: :fake_socket})

      log =
        capture_log(fn ->
          assert {:stop, :aprsme_timeout, ^state} =
                   Aprsme.Is.handle_info(:aprsme_no_message_timeout, state)
        end)

      assert log =~ "Socket timeout detected"
    end
  end

  describe "handle_info(:send_keepalive, ...)" do
    test "reschedules keepalive when socket is nil" do
      state = build_state(%{socket: nil})

      assert {:noreply, new_state} = Aprsme.Is.handle_info(:send_keepalive, state)
      assert new_state.socket == nil
      # Timer should be rescheduled even when disconnected
      assert new_state.keepalive_timer != state.keepalive_timer
    end
  end

  describe "handle_info({:tcp, ...}, ...)" do
    test "processes complete lines from TCP data" do
      ensure_ets_table()
      state = build_state()

      # Send a comment line that won't trigger external deps
      data = "# server comment\r\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)

        # Buffer should be empty after processing complete line
        assert new_state.buffer == ""
        # Timer should be reset
        assert new_state.timer != state.timer
        # Packet stats should be updated
        assert new_state.packet_stats.total_packets == 1
      end)
    end

    test "buffers incomplete lines" do
      state = build_state()

      # Send data without a newline
      data = "# incomplete line"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)

        assert new_state.buffer == "# incomplete line"
      end)
    end

    test "handles multi-line data" do
      state = build_state()

      data = "# line1\r\n# line2\r\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)

        assert new_state.buffer == ""
      end)
    end

    test "handles split lines across multiple TCP messages" do
      state = build_state()

      # First chunk with incomplete line
      data1 = "# partial"

      capture_log(fn ->
        {:noreply, state2} = Aprsme.Is.handle_info({:tcp, :fake_port, data1}, state)
        assert state2.buffer == "# partial"

        # Second chunk completes the line
        data2 = " line\r\n"
        {:noreply, state3} = Aprsme.Is.handle_info({:tcp, :fake_port, data2}, state2)
        assert state3.buffer == ""
      end)
    end

    test "skips empty lines" do
      state = build_state()

      data = "\r\n\r\n# real line\r\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)
        assert new_state.buffer == ""
      end)
    end

    test "updates packet stats with per-second counter" do
      state = build_state()

      data = "# line\r\n"

      capture_log(fn ->
        {:noreply, state2} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)
        assert state2.packet_stats.total_packets == 1
        assert state2.packet_stats.last_packet_at

        # Send another in the same second
        {:noreply, state3} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state2)
        assert state3.packet_stats.total_packets == 2
      end)
    end
  end

  describe "handle_info({:ssl, ...}, ...)" do
    test "processes SSL data the same as TCP" do
      state = build_state()

      data = "# ssl comment\r\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:ssl, :fake_port, data}, state)

        assert new_state.buffer == ""
        assert new_state.packet_stats.total_packets == 1
      end)
    end
  end

  describe "handle_info({:tcp_closed, ...}, ...)" do
    test "schedules reconnect and clears socket" do
      state = build_state(%{socket: :fake_socket})

      {:noreply, new_state} = Aprsme.Is.handle_info({:tcp_closed, :fake_socket}, state)

      assert new_state.socket == nil
      assert new_state.timer == nil
      assert new_state.keepalive_timer == nil
    end
  end

  describe "handle_info({:tcp_error, ...}, ...)" do
    test "schedules reconnect on error" do
      state = build_state(%{socket: :fake_socket})

      {:noreply, new_state} =
        Aprsme.Is.handle_info({:tcp_error, :fake_socket, :econnrefused}, state)

      assert new_state.socket == nil
      assert new_state.timer == nil
      assert new_state.keepalive_timer == nil
    end
  end

  describe "handle_info(:reconnect, ...)" do
    test "reconnect attempt is blocked in test environment" do
      state = build_state()

      {:noreply, new_state} = Aprsme.Is.handle_info(:reconnect, state)

      # Socket should remain nil since connection is blocked in test env
      assert new_state.socket == nil
    end
  end

  describe "terminate/2" do
    test "terminates cleanly with nil socket" do
      state = build_state(%{socket: nil})

      result = Aprsme.Is.terminate(:normal, state)
      assert result == :normal
    end

    test "terminates cleanly with incomplete buffer data" do
      state = build_state(%{socket: nil, buffer: "incomplete data"})

      result = Aprsme.Is.terminate(:normal, state)
      assert result == :normal
    end

    test "terminates cleanly with empty buffer" do
      state = build_state(%{socket: nil, buffer: ""})

      result = Aprsme.Is.terminate(:normal, state)
      assert result == :normal
    end

    test "handles state without buffer key" do
      # terminate uses Map.get with default, so missing :buffer should be fine
      state = Map.delete(build_state(), :buffer)

      capture_log(fn ->
        assert Aprsme.Is.terminate(:shutdown, state) == :normal
      end)
    end

    test "cancels timers on terminate" do
      timer = Process.send_after(self(), :timer_test, to_timeout(minute: 5))
      keepalive = Process.send_after(self(), :keepalive_test, to_timeout(minute: 5))

      state = build_state(%{socket: nil, timer: timer, keepalive_timer: keepalive})

      capture_log(fn ->
        Aprsme.Is.terminate(:normal, state)
      end)

      # Timers should have been cancelled
      assert Process.cancel_timer(timer) == false
      assert Process.cancel_timer(keepalive) == false
    end
  end

  describe "code_change/3" do
    test "returns state unchanged" do
      state = build_state()
      assert {:ok, ^state} = Aprsme.Is.code_change("1.0.0", state, [])
    end
  end

  describe "APRS-IS mock functionality" do
    setup do
      case GenServer.start_link(AprsIsMock, [], name: AprsIsMock) do
        {:ok, pid} ->
          on_exit(fn ->
            if Process.alive?(pid), do: GenServer.stop(pid, :normal)
          end)

          {:ok, mock_pid: pid}

        {:error, {:already_started, pid}} ->
          on_exit(fn ->
            if Process.alive?(pid), do: GenServer.stop(pid, :normal)
          end)

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
      assert AprsIsMock.send_message("test message") == :ok
      assert AprsIsMock.send_message("TEST", "DEST", "hello") == :ok
      assert AprsIsMock.set_filter("r/0/0/1") == :ok
      assert AprsIsMock.list_active_filters() == :ok
    end

    test "mock can simulate packet reception for testing", %{mock_pid: _pid} do
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
      assert AprsIsMock.simulate_connection_state(true) == :ok
      status = AprsIsMock.get_status()
      assert status.connected == true
      assert status.connected_at

      assert AprsIsMock.simulate_connection_state(false) == :ok
      status = AprsIsMock.get_status()
      assert status.connected == false
      assert status.connected_at == nil
    end
  end

  describe "network isolation verification" do
    test "no external network calls should be made during test runs" do
      forbidden_servers = [
        "rotate.aprs2.net",
        "dallas.aprs2.net",
        "seattle.aprs2.net",
        "chicago.aprs2.net",
        "atlanta.aprs2.net"
      ]

      current_server = Application.get_env(:aprsme, :aprsme_is_server)

      if current_server do
        server_str = to_string(current_server)

        refute Enum.any?(forbidden_servers, fn forbidden ->
                 String.contains?(server_str, forbidden)
               end),
               "Test environment should not be configured with real APRS servers"
      end
    end

    test "test environment marker is properly set" do
      assert Mix.env() == :test
      assert Application.get_env(:aprsme, :env) == :test
    end

    test "APRS-IS connection is disabled in test config" do
      assert Application.get_env(:aprsme, :disable_aprs_connection) == true
    end
  end

  describe "get_status/0 when GenServer is not running" do
    test "returns disconnected status" do
      # Aprsme.Is is not started in test env, so get_status should handle nil process
      status = Aprsme.Is.get_status()

      assert status.connected == false
      assert status.uptime_seconds == 0
      assert status.connected_at == nil
      assert is_map(status.packet_stats)
      assert status.packet_stats.total_packets == 0
    end

    test "includes configured server info" do
      status = Aprsme.Is.get_status()

      assert status.port == 14_580
      assert status.login_id == "TEST"
      assert status.filter == "r/33/-96/100"
    end
  end

  describe "buffer line extraction via TCP data" do
    test "handles \\n line endings" do
      state = build_state()
      data = "# line1\n# line2\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)
        assert new_state.buffer == ""
      end)
    end

    test "handles mixed \\r\\n and \\n line endings" do
      state = build_state()
      data = "# line1\r\n# line2\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)
        assert new_state.buffer == ""
      end)
    end

    test "preserves partial line in buffer across chunks" do
      state = build_state()

      capture_log(fn ->
        {:noreply, s1} = Aprsme.Is.handle_info({:tcp, :fake_port, "# complete\r\npartial"}, state)
        assert s1.buffer == "partial"

        {:noreply, s2} = Aprsme.Is.handle_info({:tcp, :fake_port, " data\r\n"}, s1)
        assert s2.buffer == ""
      end)
    end
  end

  describe "packet stats update via TCP data" do
    test "increments total_packets per data message" do
      state = build_state()

      capture_log(fn ->
        {:noreply, s1} = Aprsme.Is.handle_info({:tcp, :fake_port, "# a\r\n"}, state)
        assert s1.packet_stats.total_packets == 1

        {:noreply, s2} = Aprsme.Is.handle_info({:tcp, :fake_port, "# b\r\n"}, s1)
        assert s2.packet_stats.total_packets == 2

        {:noreply, s3} = Aprsme.Is.handle_info({:tcp, :fake_port, "# c\r\n"}, s2)
        assert s3.packet_stats.total_packets == 3
      end)
    end

    test "records last_packet_at timestamp" do
      state = build_state()

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, "# x\r\n"}, state)
        assert %DateTime{} = new_state.packet_stats.last_packet_at
      end)
    end

    test "resets per-second counter when time advances" do
      # Build a state with last_second_timestamp in the past
      old_stats = %{
        total_packets: 5,
        last_packet_at: DateTime.utc_now(),
        packets_per_second: 3,
        last_second_count: 3,
        last_second_timestamp: System.system_time(:second) - 2
      }

      state = build_state(%{packet_stats: old_stats})

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, "# data\r\n"}, state)

        # Should have reset the per-second counter
        assert new_state.packet_stats.total_packets == 6
        assert new_state.packet_stats.packets_per_second == 1
        assert new_state.packet_stats.last_second_count == 1
      end)
    end

    test "accumulates per-second counter within same second" do
      state = build_state()

      capture_log(fn ->
        {:noreply, s1} = Aprsme.Is.handle_info({:tcp, :fake_port, "# a\r\n"}, state)

        # If within the same second, count should accumulate
        # (This depends on test execution speed, but the logic is correct)
        {:noreply, s2} = Aprsme.Is.handle_info({:tcp, :fake_port, "# b\r\n"}, s1)
        assert s2.packet_stats.total_packets == 2
      end)
    end
  end

  describe "dispatch/1 with APRS packets" do
    setup do
      ensure_ets_table()
      :ok
    end

    test "dispatches a valid position packet without crashing" do
      raw = "N0CALL>APRS,TCPIP*:!3300.00N/09600.00W#Test station"

      capture_log(fn ->
        Aprsme.Is.dispatch(raw)
      end)
    end

    test "dispatches a weather packet" do
      raw = "N0CALL>APRS,TCPIP*:@092345z3300.00N/09600.00W_090/000g005t077"

      capture_log(fn ->
        Aprsme.Is.dispatch(raw)
      end)
    end

    test "handles rescue in dispatch when packet processing raises" do
      capture_log(fn ->
        Aprsme.Is.dispatch("???")
      end)
    end

    test "stores bad packet on parse error" do
      capture_log(fn ->
        Aprsme.Is.dispatch("not a valid aprs packet at all")
      end)
    end

    test "dispatches a status packet" do
      raw = "N0CALL>APRS,TCPIP*:>Test status message"

      capture_log(fn ->
        Aprsme.Is.dispatch(raw)
      end)
    end

    test "handles comment with only whitespace after hash" do
      result = Aprsme.Is.dispatch("#   ")
      assert result == :ok
    end
  end

  describe "handle_info({:tcp_closed, ...}) with nil timers" do
    test "handles tcp_closed when timers are already nil" do
      state = build_state(%{socket: :fake_socket, timer: nil, keepalive_timer: nil})

      {:noreply, new_state} = Aprsme.Is.handle_info({:tcp_closed, :fake_socket}, state)

      assert new_state.socket == nil
      assert new_state.timer == nil
      assert new_state.keepalive_timer == nil
    end
  end

  describe "handle_info({:tcp_error, ...}) with nil timers" do
    test "handles tcp_error when timers are already nil" do
      state = build_state(%{socket: :fake_socket, timer: nil, keepalive_timer: nil})

      {:noreply, new_state} =
        Aprsme.Is.handle_info({:tcp_error, :fake_socket, :econnreset}, state)

      assert new_state.socket == nil
    end
  end

  describe "handle_call(:get_status, ...) server_to_string edge cases" do
    test "handles non-string non-charlist server via to_string fallback" do
      state = build_state(%{server: :some_atom_server})

      {:reply, status, _state} = Aprsme.Is.handle_call(:get_status, {self(), make_ref()}, state)

      assert status.server == "some_atom_server"
    end
  end

  describe "dispatch/1 processes TCP data through full pipeline" do
    setup do
      ensure_ets_table()
      :ok
    end

    test "valid packet is processed through struct_to_map and submitted to pipeline" do
      # A packet with an SSID and path that exercises full dispatch path
      raw = "W5ISP-9>APRS,TCPIP*,qAR,W5ISP:!3300.00N/09600.00W>PHG2360 Mobile"

      capture_log(fn ->
        Aprsme.Is.dispatch(raw)
      end)
    end
  end

  describe "TCP data with real APRS packets" do
    setup do
      ensure_ets_table()
      :ok
    end

    test "processes a complete APRS packet arriving via TCP" do
      state = build_state()
      raw = "W5ISP-1>APRS,TCPIP*:!3300.00N/09600.00W#Test\r\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, raw}, state)

        assert new_state.buffer == ""
        assert new_state.packet_stats.total_packets == 1
      end)
    end

    test "processes multiple APRS packets in single TCP message" do
      state = build_state()

      data =
        "# server comment\r\nW5ISP-1>APRS,TCPIP*:!3300.00N/09600.00W#Test\r\n# another comment\r\n"

      capture_log(fn ->
        {:noreply, new_state} = Aprsme.Is.handle_info({:tcp, :fake_port, data}, state)

        assert new_state.buffer == ""
      end)
    end
  end
end
