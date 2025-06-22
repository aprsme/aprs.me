defmodule Aprs.Is do
  @moduledoc false
  use GenServer

  require Logger

  @aprs_timeout 60 * 1000
  @keepalive_interval 20 * 1000

  @type state :: %{
          server: charlist() | String.t(),
          port: pos_integer(),
          socket: :ssl.sslsocket() | nil,
          timer: reference() | nil,
          keepalive_timer: reference() | nil,
          connected_at: DateTime.t(),
          packet_stats: map(),
          buffer: String.t(),
          login_params: %{
            user_id: String.t(),
            passcode: String.t(),
            filter: String.t()
          }
        }

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    env = Application.get_env(:aprs, :env)
    disable_connection = Application.get_env(:aprs, :disable_aprs_connection, false)

    do_init(env, disable_connection)
  end

  defp do_init(:test, _) do
    Logger.warning("APRS-IS connection disabled in test environment")
    {:stop, :test_environment_disabled}
  end

  defp do_init(_, true) do
    Logger.warning("APRS-IS connection disabled in test environment")
    {:stop, :test_environment_disabled}
  end

  defp do_init(_env, false) do
    # Trap exits so we can gracefully shut down
    Process.flag(:trap_exit, true)

    # Add a small delay to prevent rapid reconnection attempts
    Process.sleep(2000)

    # Get startup parameters
    server = Application.get_env(:aprs, :aprs_is_server, ~c"rotate.aprs2.net")
    port = Application.get_env(:aprs, :aprs_is_port, 14_580)
    default_filter = Application.get_env(:aprs, :aprs_is_default_filter, "r/33/-96/100")
    aprs_user_id = Application.get_env(:aprs, :aprs_is_login_id, "W5ISP")
    aprs_passcode = Application.get_env(:aprs, :aprs_is_password, "-1")

    # Record connection start time
    connected_at = DateTime.utc_now()

    # Initialize packet statistics
    packet_stats = %{
      total_packets: 0,
      last_packet_at: nil,
      packets_per_second: 0,
      last_second_count: 0,
      last_second_timestamp: System.system_time(:second)
    }

    with {:ok, socket} <- connect_to_aprs_is(server, port),
         :ok <- send_login_string(socket, aprs_user_id, aprs_passcode, default_filter) do
      timer = create_timer(@aprs_timeout)
      keepalive_timer = create_keepalive_timer(@keepalive_interval)

      {:ok,
       %{
         server: server,
         port: port,
         socket: socket,
         timer: timer,
         keepalive_timer: keepalive_timer,
         connected_at: connected_at,
         packet_stats: packet_stats,
         buffer: "",
         login_params: %{
           user_id: aprs_user_id,
           passcode: aprs_passcode,
           filter: default_filter
         }
       }}
    else
      _ ->
        Logger.error("Unable to establish connection or log in to APRS-IS")
        {:stop, :aprs_connection_failed}
    end
  end

  # Client API

  def stop do
    Logger.info("Stopping Server")
    GenServer.stop(__MODULE__, :stop)
  end

  def get_status do
    case Process.whereis(__MODULE__) do
      nil ->
        # GenServer is not running (disconnected)
        server = Application.get_env(:aprs, :aprs_is_server, ~c"rotate.aprs2.net")
        port = Application.get_env(:aprs, :aprs_is_port, 14_580)

        %{
          connected: false,
          server: server_to_string(server),
          port: port,
          connected_at: nil,
          uptime_seconds: 0,
          login_id: Application.get_env(:aprs, :aprs_is_login_id, "W5ISP"),
          filter: Application.get_env(:aprs, :aprs_is_default_filter, "r/33/-96/100"),
          packet_stats: default_packet_stats(),
          stored_packet_count: Aprs.Packets.get_total_packet_count()
        }

      _pid ->
        try do
          GenServer.call(__MODULE__, :get_status, 5000)
        catch
          :exit, _ ->
            # GenServer exists but not responding
            server = Application.get_env(:aprs, :aprs_is_server, ~c"rotate.aprs2.net")
            port = Application.get_env(:aprs, :aprs_is_port, 14_580)

            %{
              connected: false,
              server: server_to_string(server),
              port: port,
              connected_at: nil,
              uptime_seconds: 0,
              login_id: Application.get_env(:aprs, :aprs_is_login_id, "W5ISP"),
              filter: Application.get_env(:aprs, :aprs_is_default_filter, "r/33/-96/100"),
              packet_stats: default_packet_stats(),
              stored_packet_count: Aprs.Packets.get_total_packet_count()
            }
        end
    end
  end

  def set_filter(filter_string), do: send_message("#filter #{filter_string}")
  def list_active_filters, do: send_message("#filter?")

  def send_message(from, to, message) do
    padded_callsign = String.pad_trailing(to, 9)
    send_message("#{from}>APRS,TCPIP*::#{padded_callsign}:#{message}")
  end

  def send_message(message) do
    GenServer.call(__MODULE__, {:send_message, message})
  end

  # Server methods

  @spec connect_to_aprs_is(String.t() | charlist(), pos_integer()) ::
          {:ok, :ssl.sslsocket()} | {:error, any()}
  defp connect_to_aprs_is(server, port) do
    # Additional safeguard: prevent connections in test environment
    env = Application.get_env(:aprs, :env)
    disable_connection = Application.get_env(:aprs, :disable_aprs_connection, false)

    do_connect_to_aprs_is(server, port, env, disable_connection)
  end

  defp do_connect_to_aprs_is(_server, _port, :test, _) do
    Logger.warning("Attempted APRS-IS connection blocked in test environment")
    {:error, :test_environment_blocked}
  end

  defp do_connect_to_aprs_is(_server, _port, _, true) do
    Logger.warning("Attempted APRS-IS connection blocked in test environment")
    {:error, :test_environment_blocked}
  end

  defp do_connect_to_aprs_is(server, port, _env, false) do
    Logger.debug("Connecting to: #{server}:#{port}")
    opts = [:binary, active: true]
    :gen_tcp.connect(String.to_charlist(server), port, opts)
  end

  @spec send_login_string(:ssl.sslsocket(), String.t(), String.t(), String.t()) ::
          :ok | {:error, any()}
  defp send_login_string(socket, aprs_user_id, aprs_passcode, filter) do
    login_string =
      "user #{aprs_user_id} pass #{aprs_passcode} vers aprs.me 0.1 filter #{filter}\r\n"

    Logger.info("Sending login string: user #{aprs_user_id} pass ***** vers aprs.me 0.1 filter #{filter}")

    :gen_tcp.send(socket, login_string)
  end

  @spec create_timer(non_neg_integer()) :: reference()
  defp create_timer(timeout) do
    Process.send_after(self(), :aprs_no_message_timeout, timeout)
  end

  @spec create_keepalive_timer(non_neg_integer()) :: reference()
  defp create_keepalive_timer(interval) do
    Process.send_after(self(), :send_keepalive, interval)
  end

  @impl true
  def handle_call({:send_message, message}, _from, state) do
    next_ack_number = :ets.update_counter(:aprs, :message_number, 1)
    # Append ack number
    message = message <> "{" <> to_string(next_ack_number) <> "\r"

    Logger.info("Sending message: #{inspect(message)}")
    :gen_tcp.send(state.socket, message)
    {:reply, :ok, state}
  end

  def handle_call(:get_status, _from, state) do
    status = %{
      connected: true,
      server: server_to_string(state.server),
      port: state.port,
      connected_at: state.connected_at,
      uptime_seconds: DateTime.diff(DateTime.utc_now(), state.connected_at),
      login_id: state.login_params.user_id,
      filter: state.login_params.filter,
      packet_stats: state.packet_stats,
      stored_packet_count: Aprs.Packets.get_total_packet_count()
    }

    {:reply, status, state}
  end

  @impl true
  def handle_info(:aprs_no_message_timeout, state) do
    Logger.error("Socket timeout detected. Killing genserver.")
    {:stop, :aprs_timeout, state}
  end

  def handle_info(:send_keepalive, state) do
    # Send a comment line as keepalive (APRS-IS standard)
    case :gen_tcp.send(state.socket, "# keepalive\r\n") do
      :ok ->
        Logger.debug("Sent keepalive")
        keepalive_timer = create_keepalive_timer(@keepalive_interval)
        {:noreply, %{state | keepalive_timer: keepalive_timer}}

      {:error, reason} ->
        Logger.error("Failed to send keepalive: #{inspect(reason)}")
        {:stop, :normal, state}
    end
  end

  @impl true
  @spec handle_info({:ssl, port(), binary()} | any(), state()) :: {:noreply, state()}
  def handle_info({:ssl, _socket, data}, state) do
    # Cancel the previous timer
    Process.cancel_timer(state.timer)

    # Update packet statistics
    current_time = System.system_time(:second)
    packet_stats = update_packet_stats(state.packet_stats, current_time)

    # Append new packet data to buffer
    buffer = state.buffer <> data

    # Process complete lines (ending with \r\n or \n)
    {complete_lines, remaining_buffer} = extract_complete_lines(buffer)

    # Dispatch each complete line
    Enum.each(complete_lines, fn line ->
      trimmed = String.trim(line)

      if trimmed != "" do
        dispatch(trimmed)
      end
    end)

    # Start a new timer
    timer = Process.send_after(self(), :aprs_no_message_timeout, @aprs_timeout)

    state =
      state
      |> Map.put(:timer, timer)
      |> Map.put(:packet_stats, packet_stats)
      |> Map.put(:buffer, remaining_buffer)

    {:noreply, state}
  end

  def handle_info({:tcp, _socket, data}, state) do
    # Cancel the previous timer
    Process.cancel_timer(state.timer)

    # Update packet statistics
    current_time = System.system_time(:second)
    packet_stats = update_packet_stats(state.packet_stats, current_time)

    # Append new packet data to buffer
    buffer = state.buffer <> data

    # Process complete lines (ending with \r\n or \n)
    {complete_lines, remaining_buffer} = extract_complete_lines(buffer)

    # Dispatch each complete line
    Enum.each(complete_lines, fn line ->
      trimmed = String.trim(line)

      if trimmed != "" do
        dispatch(trimmed)
      end
    end)

    # Start a new timer
    timer = Process.send_after(self(), :aprs_no_message_timeout, @aprs_timeout)

    state =
      state
      |> Map.put(:timer, timer)
      |> Map.put(:packet_stats, packet_stats)
      |> Map.put(:buffer, remaining_buffer)

    {:noreply, state}
  end

  def handle_info({:tcp_closed, _socket}, state) do
    Logger.warning("Socket has been closed by remote server - will reconnect")
    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _socket, reason}, state) do
    Logger.error("Connection error: #{inspect(reason)}")
    {:stop, :normal, state}
  end

  # Extract complete lines from buffer, returning {complete_lines, remaining_buffer}
  @spec extract_complete_lines(String.t()) :: {[String.t()], String.t()}
  defp extract_complete_lines(buffer) do
    # Split by both \r\n and \n to handle different line endings
    parts = String.split(buffer, ~r/\r?\n/, parts: :infinity)

    # The last part might be incomplete
    case parts do
      [] ->
        {[], ""}

      [single] ->
        # No newline found, entire buffer is incomplete
        {[], single}

      parts ->
        # Last element might be incomplete line
        {complete, [maybe_incomplete]} = Enum.split(parts, -1)

        # If buffer ended with newline, maybe_incomplete will be empty string
        if String.ends_with?(buffer, "\n") or String.ends_with?(buffer, "\r\n") do
          {complete ++ [maybe_incomplete], ""}
        else
          {complete, maybe_incomplete}
        end
    end
  end

  @impl true
  def terminate(reason, state) do
    # Do Shutdown Stuff
    Logger.info("Terminating APRS-IS connection: #{inspect(reason)}")

    # Log any remaining buffered data
    case Map.get(state, :buffer, "") do
      "" -> :ok
      buffer -> Logger.warning("Terminating with incomplete packet in buffer: #{inspect(buffer)}")
    end

    # Cancel timers
    if Map.has_key?(state, :timer), do: Process.cancel_timer(state.timer)
    if Map.has_key?(state, :keepalive_timer), do: Process.cancel_timer(state.keepalive_timer)

    # Close socket
    case Map.get(state, :socket) do
      nil ->
        :ok

      socket ->
        Logger.info("Closing socket")
        :gen_tcp.close(socket)
    end

    :normal
  end

  @impl true
  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end

  @spec dispatch(binary) :: nil | :ok
  def dispatch("#" <> comment_text) do
    Logger.debug("COMMENT: " <> String.trim(comment_text))
  end

  def dispatch(""), do: nil

  def dispatch(message) do
    case Parser.parse(message) do
      {:ok, parsed_message} ->
        # Store the packet in the database for future replay
        # Use Task to avoid slowing down the main process
        Task.start(fn ->
          require Logger

          try do
            # Always set received_at timestamp to ensure consistency
            current_time = DateTime.truncate(DateTime.utc_now(), :microsecond)
            packet_data = Map.put(parsed_message, :received_at, current_time)

            # Convert to map before storing to avoid struct conversion issues
            attrs = struct_to_map(packet_data)

            # Extract additional data from the parsed packet including raw packet
            attrs = Aprs.Packet.extract_additional_data(attrs, message)

            # Normalize data_type to string if it's an atom
            attrs = normalize_data_type(attrs)

            # Store in database through the Packets context
            case Aprs.Packets.store_packet(attrs) do
              {:ok, _packet} ->
                # Packet stored successfully
                :ok

              {:error, :storage_exception} ->
                Logger.error("Storage exception while storing packet from #{inspect(parsed_message.sender)}")

                Logger.debug("Packet attributes that failed: #{inspect(attrs)}")

              {:error, :validation_error} ->
                Logger.error("Validation error while storing packet from #{inspect(parsed_message.sender)}")

                Logger.debug("Packet attributes that failed: #{inspect(attrs)}")
            end
          rescue
            error ->
              Logger.error("Exception while storing packet from #{inspect(parsed_message.sender)}: #{inspect(error)}")

              Logger.debug("Raw message: #{inspect(message)}")
              Logger.debug("Parsed message: #{inspect(parsed_message)}")
          end
        end)

        # Broadcast to live clients
        AprsWeb.Endpoint.broadcast("aprs_messages", "packet", parsed_message)

      {:error, :invalid_packet} ->
        Logger.debug("PARSE ERROR: invalid packet")

        Aprs.Packets.store_bad_packet(message, %{
          message: "Invalid packet format",
          type: "ParseError"
        })

      {:error, error} ->
        Logger.debug("PARSE ERROR: " <> error)
        Aprs.Packets.store_bad_packet(message, %{message: error, type: "ParseError"})
    end
  end

  # Normalize data_type to ensure proper storage
  @spec normalize_data_type(map()) :: map()
  defp normalize_data_type(%{data_type: data_type} = attrs) when is_atom(data_type) do
    %{attrs | data_type: to_string(data_type)}
  end

  defp normalize_data_type(attrs), do: attrs

  @spec update_packet_stats(map(), integer()) :: map()
  defp update_packet_stats(stats, current_time) do
    new_total = stats.total_packets + 1

    # Check if we need to reset the per-second counter
    if current_time - stats.last_second_timestamp >= 1 do
      %{
        total_packets: new_total,
        last_packet_at: DateTime.utc_now(),
        packets_per_second: 1,
        last_second_count: 1,
        last_second_timestamp: current_time
      }
    else
      new_second_count = stats.last_second_count + 1

      %{
        total_packets: new_total,
        last_packet_at: DateTime.utc_now(),
        packets_per_second: new_second_count,
        last_second_count: new_second_count,
        last_second_timestamp: stats.last_second_timestamp
      }
    end
  end

  @spec server_to_string(String.t() | charlist() | any()) :: String.t()
  defp server_to_string(server) when is_list(server), do: List.to_string(server)
  defp server_to_string(server) when is_binary(server), do: server
  defp server_to_string(server), do: to_string(server)

  @spec default_packet_stats() :: map()
  defp default_packet_stats do
    %{
      total_packets: 0,
      last_packet_at: nil,
      packets_per_second: 0,
      last_second_count: 0,
      last_second_timestamp: System.system_time(:second)
    }
  end

  # Helper function to recursively convert structs to maps
  # This handles nested structs that Map.from_struct/1 cannot handle
  @spec struct_to_map(any()) :: any()
  defp struct_to_map(%{__struct__: struct_type} = struct) do
    converted_map =
      struct
      |> Map.from_struct()
      |> Map.new(fn {k, v} -> {k, struct_to_map(v)} end)

    # Add type information to help with later processing
    Map.put(converted_map, :__original_struct__, struct_type)
  end

  defp struct_to_map(value) when is_list(value) do
    Enum.map(value, &struct_to_map/1)
  end

  defp struct_to_map(value), do: value
end
