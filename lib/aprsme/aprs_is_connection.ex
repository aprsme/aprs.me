defmodule Aprsme.AprsIsConnection do
  @moduledoc """
  Maintains a supervised TCP connection to APRS-IS, with reconnection logic and
  exponential backoff. Broadcasts each received line via Phoenix.PubSub and emits
  telemetry events for connection, disconnection, errors, and packet receipt.
  """
  use GenServer

  alias Aprsme.LogSanitizer

  require Logger

  @type state :: %{
          socket: port() | nil,
          backoff: non_neg_integer()
        }

  @reconnect_initial 2_000
  @reconnect_max 60_000
  @pubsub_topic "aprs_is:raw"

  # Public API
  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc """
  Send a raw string to APRS-IS.
  """
  @spec send_packet(String.t()) :: :ok | {:error, term()}
  def send_packet(packet) do
    GenServer.call(__MODULE__, {:send, packet})
  end

  # GenServer callbacks
  @impl true
  def init(state) do
    # Check if APRS connection is disabled (e.g., in test mode)
    disable_connection = Application.get_env(:aprsme, :disable_aprs_connection, false)

    if disable_connection do
      Logger.info("APRS-IS connection disabled (test mode)")
      {:ok, Map.merge(%{socket: nil, backoff: @reconnect_initial}, state)}
    else
      schedule_connect(0)
      {:ok, Map.merge(%{socket: nil, backoff: @reconnect_initial}, state)}
    end
  end

  @impl true
  def handle_info(:connect, state) do
    # Use circuit breaker for connection attempts
    case Aprsme.CircuitBreaker.call(:aprs_is, &connect_aprs_is/0, 15_000) do
      {:ok, {:ok, socket}} ->
        Logger.info("Connected to APRS-IS")
        :telemetry.execute([:aprsme, :is, :connected], %{}, %{})
        {:noreply, %{state | socket: socket, backoff: @reconnect_initial}}

      {:ok, {:error, reason}} ->
        handle_connection_error(reason, state)

      {:error, :circuit_open} ->
        Logger.warning("APRS-IS circuit breaker is open, delaying reconnection")
        # Use longer backoff when circuit is open
        schedule_connect(@reconnect_max)
        {:noreply, %{state | socket: nil, backoff: @reconnect_max}}

      {:error, reason} ->
        handle_connection_error(reason, state)
    end
  end

  @impl true
  def handle_info({:tcp, _socket, data}, state) do
    # Each line received from APRS-IS
    Phoenix.PubSub.broadcast(Aprsme.PubSub, @pubsub_topic, {:aprsme_is_line, data})
    :telemetry.execute([:aprsme, :is, :packet], %{size: byte_size(data)}, %{data: data})
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, _socket}, state) do
    Logger.warning("APRS-IS connection closed, initiating reconnection",
      connection_status:
        LogSanitizer.log_data(
          event: "connection_closed",
          reconnect_delay_ms: @reconnect_initial,
          backoff_reset: true
        )
    )

    :telemetry.execute([:aprsme, :is, :disconnected], %{}, %{})
    schedule_connect(@reconnect_initial)
    {:noreply, %{state | socket: nil, backoff: @reconnect_initial}}
  end

  @impl true
  def handle_info({:tcp_error, _socket, reason}, state) do
    Logger.error("APRS-IS TCP error detected, reconnecting",
      tcp_error:
        LogSanitizer.log_data(
          reason: reason,
          reconnect_delay_ms: @reconnect_initial,
          socket_reset: true
        )
    )

    :telemetry.execute([:aprsme, :is, :tcp_error], %{}, %{reason: reason})
    schedule_connect(@reconnect_initial)
    {:noreply, %{state | socket: nil, backoff: @reconnect_initial}}
  end

  @impl true
  def handle_call({:send, packet}, _from, %{socket: socket} = state) when is_port(socket) do
    # Safely send to socket with error handling
    case :gen_tcp.send(socket, packet <> "\r\n") do
      :ok ->
        {:reply, :ok, state}

      {:error, reason} = error ->
        Logger.error("Failed to send packet: #{inspect(reason)}")
        # Socket is likely closed, reset state
        {:reply, error, %{state | socket: nil}}
    end
  end

  def handle_call({:send, _packet}, _from, state) do
    {:reply, {:error, :not_connected}, state}
  end

  @impl true
  def terminate(_reason, %{socket: socket}) when is_port(socket) do
    :gen_tcp.close(socket)
    :ok
  end

  def terminate(_reason, _state), do: :ok

  defp schedule_connect(delay) do
    Process.send_after(self(), :connect, delay)
  end

  defp handle_connection_error(reason, state) do
    Logger.error("APRS-IS connection failed, retrying with backoff",
      connection_error:
        LogSanitizer.log_data(
          reason: reason,
          backoff_ms: state.backoff,
          next_attempt: DateTime.add(DateTime.utc_now(), state.backoff, :millisecond)
        )
    )

    :telemetry.execute([:aprsme, :is, :connect_error], %{}, %{reason: reason})
    schedule_connect(state.backoff)
    {:noreply, %{state | socket: nil, backoff: min(state.backoff * 2, @reconnect_max)}}
  end

  defp connect_aprs_is do
    host = Application.get_env(:aprsme, :aprs_is_server, ~c"rotate.aprs2.net")
    port = Application.get_env(:aprsme, :aprs_is_port, 14_580)
    callsign = Application.get_env(:aprsme, :aprs_is_login_id, "N0CALL")
    passcode = Application.get_env(:aprsme, :aprs_is_password, "00000")
    filter = Application.get_env(:aprsme, :aprs_is_default_filter, "")

    # Add timeout to prevent indefinite hanging
    opts = [:binary, active: true, packet: :line, keepalive: true, send_timeout: 5000]
    # 10 seconds
    connect_timeout = 10_000

    case :gen_tcp.connect(host, port, opts, connect_timeout) do
      {:ok, socket} ->
        login = "user #{callsign} pass #{passcode} vers aprs.me 0.1 #{filter}\r\n"
        :ok = :gen_tcp.send(socket, login)
        {:ok, socket}

      error ->
        error
    end
  end

  @impl true
  def code_change(_old_vsn, state, _extra) do
    {:ok, state}
  end
end
