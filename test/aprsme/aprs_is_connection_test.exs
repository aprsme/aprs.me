defmodule Aprsme.AprsIsConnectionTest do
  use ExUnit.Case, async: false

  alias Aprsme.AprsIsConnection

  setup do
    # Stop any existing AprsIsConnection process
    case Process.whereis(AprsIsConnection) do
      nil -> :ok
      pid -> GenServer.stop(pid, :normal, 5000)
    end

    # Ensure disable_aprs_connection is true so we don't try real connections
    original_env = Application.get_env(:aprsme, :disable_aprs_connection)
    Application.put_env(:aprsme, :disable_aprs_connection, true)

    on_exit(fn ->
      # Stop the process if it's still running
      case Process.whereis(AprsIsConnection) do
        nil ->
          :ok

        pid ->
          try do
            GenServer.stop(pid, :normal, 5000)
          catch
            :exit, _ -> :ok
          end
      end

      # Reset env
      if original_env do
        Application.put_env(:aprsme, :disable_aprs_connection, original_env)
      else
        Application.delete_env(:aprsme, :disable_aprs_connection)
      end
    end)

    :ok
  end

  describe "init/1 when disabled" do
    test "starts with socket: nil" do
      {:ok, pid} = AprsIsConnection.start_link([])
      state = :sys.get_state(pid)

      assert state.socket == nil
    end
  end

  describe "handle_info {:tcp, socket, data}" do
    test "broadcasts received data via PubSub" do
      {:ok, pid} = AprsIsConnection.start_link([])
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "aprs_is:raw")

      send(pid, {:tcp, make_ref(), "test data"})

      assert_receive {:aprsme_is_line, "test data"}
    end
  end

  describe "handle_info {:tcp_closed, socket}" do
    test "sets socket to nil in state" do
      {:ok, pid} = AprsIsConnection.start_link([])

      send(pid, {:tcp_closed, make_ref()})

      # Allow the message to be processed
      :sys.get_state(pid)
      state = :sys.get_state(pid)

      assert state.socket == nil
    end
  end

  describe "handle_info {:tcp_error, socket, reason}" do
    test "sets socket to nil in state" do
      {:ok, pid} = AprsIsConnection.start_link([])

      send(pid, {:tcp_error, make_ref(), :etimedout})

      # Allow the message to be processed
      :sys.get_state(pid)
      state = :sys.get_state(pid)

      assert state.socket == nil
    end
  end

  describe "handle_call {:send, packet} without socket" do
    test "returns {:error, :not_connected}" do
      {:ok, pid} = AprsIsConnection.start_link([])

      assert GenServer.call(pid, {:send, "test"}) == {:error, :not_connected}
    end
  end

  describe "handle_call {:send, packet} with socket" do
    test "sends data over the socket and returns :ok" do
      {:ok, pid} = AprsIsConnection.start_link([])

      {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
      {:ok, port} = :inet.port(listen)
      {:ok, client} = :gen_tcp.connect(~c"127.0.0.1", port, [:binary, active: false])
      {:ok, server} = :gen_tcp.accept(listen)

      :sys.replace_state(pid, fn state -> %{state | socket: client} end)

      assert GenServer.call(pid, {:send, "test packet"}) == :ok

      {:ok, data} = :gen_tcp.recv(server, 0, 1000)
      assert data =~ "test packet"

      :gen_tcp.close(server)
      :gen_tcp.close(client)
      :gen_tcp.close(listen)
    end
  end

  describe "terminate/2 with socket" do
    test "closes the socket without crashing" do
      {:ok, pid} = AprsIsConnection.start_link([])

      {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
      {:ok, port} = :inet.port(listen)
      {:ok, client} = :gen_tcp.connect(~c"127.0.0.1", port, [:binary, active: false])
      {:ok, _server} = :gen_tcp.accept(listen)

      :sys.replace_state(pid, fn state -> %{state | socket: client} end)

      assert GenServer.stop(pid) == :ok

      :gen_tcp.close(listen)
    end
  end

  describe "send_packet/1" do
    test "delegates to GenServer.call with {:send, packet}" do
      {:ok, _pid} = AprsIsConnection.start_link([])

      # Without a socket it returns not_connected
      assert AprsIsConnection.send_packet("test") == {:error, :not_connected}
    end
  end

  describe "handle_call {:send, packet} with closed socket" do
    test "returns error when send fails on a closed socket" do
      {:ok, pid} = AprsIsConnection.start_link([])

      # Create a socket then close it to simulate a broken connection
      {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
      {:ok, port} = :inet.port(listen)
      {:ok, client} = :gen_tcp.connect(~c"127.0.0.1", port, [:binary, active: false])
      {:ok, _server} = :gen_tcp.accept(listen)

      :gen_tcp.close(client)

      :sys.replace_state(pid, fn state -> %{state | socket: client} end)

      assert {:error, _reason} = GenServer.call(pid, {:send, "test packet"})

      # State should have socket set to nil after send failure
      state = :sys.get_state(pid)
      assert state.socket == nil

      :gen_tcp.close(listen)
    end
  end

  describe "terminate/2 without socket" do
    test "returns :ok when socket is nil" do
      {:ok, pid} = AprsIsConnection.start_link([])

      # Socket is nil by default in disabled mode
      assert GenServer.stop(pid) == :ok
    end
  end

  describe "handle_info :connect" do
    test "handles connection attempt with circuit breaker" do
      {:ok, pid} = AprsIsConnection.start_link([])

      # Send :connect message - it will fail since we can't connect to APRS-IS in tests
      # but the process should handle the error gracefully
      send(pid, :connect)

      # Allow the message to be processed
      Process.sleep(200)

      # Process should still be alive after failed connection attempt
      assert Process.alive?(pid)
      state = :sys.get_state(pid)
      assert state.socket == nil
    end
  end

  describe "code_change/3" do
    test "returns {:ok, state}" do
      state = %{socket: nil, backoff: 2000}

      assert AprsIsConnection.code_change(:old, state, :extra) == {:ok, state}
    end
  end
end
