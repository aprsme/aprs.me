defmodule Aprs.Is do
  @moduledoc false
  use GenServer

  require Logger

  @aprs_timeout 60 * 1000
  @keepalive_interval 20 * 1000

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
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

  defp connect_to_aprs_is(server, port) do
    Logger.debug("Connecting to: #{server}:#{port}")
    opts = [:binary, active: true]
    :gen_tcp.connect(String.to_charlist(server), port, opts)
  end

  defp send_login_string(socket, aprs_user_id, aprs_passcode, filter) do
    login_string =
      "user #{aprs_user_id} pass #{aprs_passcode} vers aprs.me 0.1 filter #{filter}\r\n"

    Logger.info("Sending login string: user #{aprs_user_id} pass ***** vers aprs.me 0.1 filter #{filter}")

    :gen_tcp.send(socket, login_string)
  end

  defp create_timer(timeout) do
    Process.send_after(self(), :aprs_no_message_timeout, timeout)
  end

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

  def handle_info({:tcp, _socket, packet}, state) do
    # Cancel the previous timer
    Process.cancel_timer(state.timer)

    # Update packet statistics
    current_time = System.system_time(:second)
    packet_stats = update_packet_stats(state.packet_stats, current_time)

    # Handle the incoming message
    # Task.start(Aprs, :dispatch, [packet])

    if String.contains?(packet, "\n") or String.contains?(packet, "\r") do
      packet
      |> String.split("\r\n")
      |> Enum.each(&dispatch(String.trim(&1)))
    else
      dispatch(packet)
    end

    # Start a new timer
    timer = Process.send_after(self(), :aprs_no_message_timeout, @aprs_timeout)
    state = state |> Map.put(:timer, timer) |> Map.put(:packet_stats, packet_stats)

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

  @impl true
  def terminate(reason, state) do
    # Do Shutdown Stuff
    Logger.info("Terminating APRS-IS connection: #{inspect(reason)}")

    # Cancel timers
    if Map.has_key?(state, :timer), do: Process.cancel_timer(state.timer)
    if Map.has_key?(state, :keepalive_timer), do: Process.cancel_timer(state.keepalive_timer)

    # Close socket
    if Map.has_key?(state, :socket) do
      Logger.info("Closing socket")
      :gen_tcp.close(state.socket)
    end

    :normal
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
          # Store the packet if it has position data
          if has_position_data?(parsed_message) do
            Logger.info("Storing packet with position data: #{inspect(parsed_message.sender)}")
            # Always set received_at timestamp to ensure consistency
            current_time = DateTime.truncate(DateTime.utc_now(), :microsecond)
            packet_data = Map.put(parsed_message, :received_at, current_time)

            # Convert to map before storing to avoid struct conversion issues
            attrs = struct_to_map(packet_data)

            # Extract additional data from the parsed packet including raw packet
            attrs = Aprs.Packet.extract_additional_data(attrs, message)

            # Normalize data_type to string if it's an atom
            attrs = normalize_data_type(attrs)

            # Ensure SSID is never nil
            attrs =
              if Map.has_key?(attrs, :ssid) and is_nil(attrs.ssid) do
                Map.put(attrs, :ssid, "0")
              else
                attrs
              end

            # Store in database through the Packets context
            case Aprs.Packets.store_packet(attrs) do
              {:ok, packet} ->
                Logger.info("Successfully stored packet from #{packet.sender}")

              {:error, changeset} ->
                Logger.error("Failed to store packet: #{inspect(changeset.errors)}")
            end
          else
            Logger.debug("Skipping packet without position data: #{inspect(parsed_message.sender)}")
          end
        end)

        # Broadcast to live clients
        AprsWeb.Endpoint.broadcast("aprs_messages", "packet", parsed_message)

      # Logger.debug("BROADCAST: " <> inspect(parsed_message))

      # Phoenix.PubSub.broadcast(
      #   Aprs.PubSub,
      #   "aprs_messages",
      #   {:packet, parsed_message}
      # )

      # IO.inspect(parsed_message)
      # Logger.debug("SERVER:" <> message)
      {:error, :invalid_packet} ->
        Logger.debug("PARSE ERROR: invalid packet")

      {:error, error} ->
        Logger.debug("PARSE ERROR: " <> error)

      x ->
        Logger.debug("PARSE ERROR: " <> x)
    end
  end

  # Helper to check if a packet has position data worth storing
  defp has_position_data?(packet) do
    require Logger

    result =
      case packet do
        %{data_extended: nil} ->
          Logger.debug("Packet has nil data_extended: #{inspect(packet.sender)}")
          false

        %{data_extended: %{latitude: lat, longitude: lon}} when not is_nil(lat) and not is_nil(lon) ->
          # Check if coordinates are valid numbers
          valid = are_valid_coords?(lat, lon)

          if !valid do
            Logger.debug("Invalid coordinates: lat=#{inspect(lat)}, lon=#{inspect(lon)}")
          end

          valid

        %{data_extended: %Parser.Types.MicE{} = mic_e} ->
          # MicE packets have lat/lon in separate components
          valid =
            is_number(mic_e.lat_degrees) and is_number(mic_e.lat_minutes) and
              is_number(mic_e.lon_degrees) and is_number(mic_e.lon_minutes)

          Logger.debug("MicE packet position check: #{valid} for #{inspect(packet.sender)}")
          valid

        %{lat: lat, lon: lon} when not is_nil(lat) and not is_nil(lon) ->
          # Handle case where coordinates are at top level
          are_valid_coords?(lat, lon)

        _other ->
          Logger.debug("Unrecognized packet format: #{inspect(Map.keys(packet))}")
          false
      end

    Logger.debug("Position data check for #{inspect(packet.sender)}: #{result}")
    result
  end

  # Helper to validate coordinate values
  defp are_valid_coords?(lat, lon) do
    require Logger

    # Convert to float if possible
    lat_float = to_float(lat)
    lon_float = to_float(lon)

    # Log coordinate conversion
    if !is_number(lat_float) do
      Logger.debug("Could not convert latitude to float: #{inspect(lat)}")
    end

    if !is_number(lon_float) do
      Logger.debug("Could not convert longitude to float: #{inspect(lon)}")
    end

    # Check if conversion succeeded and values are in valid ranges
    result =
      is_number(lat_float) and is_number(lon_float) and
        lat_float >= -90 and lat_float <= 90 and lon_float >= -180 and lon_float <= 180

    if !result do
      Logger.debug("Invalid coordinates: lat=#{inspect(lat_float)}, lon=#{inspect(lon_float)}")
    end

    result
  end

  # Helper to convert various types to float
  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_integer(value), do: value * 1.0
  defp to_float(%Decimal{} = value), do: Decimal.to_float(value)

  defp to_float(value) when is_binary(value) do
    case Float.parse(value) do
      {float, _} -> float
      :error -> nil
    end
  end

  defp to_float(_), do: nil

  # Normalize data_type to ensure proper storage
  defp normalize_data_type(%{data_type: data_type} = attrs) when is_atom(data_type) do
    %{attrs | data_type: to_string(data_type)}
  end

  defp normalize_data_type(attrs), do: attrs

  #   total_spec = [{{:"$1", :_, :"$2"}, [{:andalso, callsign_guard, timestamp_guard}], [true]}]
  #   :ets.select_count(:aprs_messages, total_spec)
  # end

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
        stats
        | total_packets: new_total,
          last_packet_at: DateTime.utc_now(),
          packets_per_second: new_second_count,
          last_second_count: new_second_count
      }
    end
  end

  defp server_to_string(server) when is_list(server), do: List.to_string(server)
  defp server_to_string(server) when is_binary(server), do: server
  defp server_to_string(server), do: to_string(server)

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
