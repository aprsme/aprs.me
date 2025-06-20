defmodule Aprs.AprsIsConnection do
  @moduledoc """
  Maintains a supervised TCP connection to APRS-IS, with reconnection logic and
  exponential backoff. Broadcasts each received line via Phoenix.PubSub and emits
  telemetry events for connection, disconnection, errors, and packet receipt.
  """
  use GenServer

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
    schedule_connect(0)
    {:ok, Map.merge(%{socket: nil, backoff: @reconnect_initial}, state)}
  end

  @impl true
  def handle_info(:connect, state) do
    case connect_aprs_is() do
      {:ok, socket} ->
        Logger.info("Connected to APRS-IS")
        :telemetry.execute([:aprs, :is, :connected], %{}, %{})
        {:noreply, %{state | socket: socket, backoff: @reconnect_initial}}

      {:error, reason} ->
        Logger.error("APRS-IS connection failed: #{inspect(reason)}. Retrying in #{state.backoff} ms.")

        :telemetry.execute([:aprs, :is, :connect_error], %{}, %{reason: reason})
        schedule_connect(state.backoff)
        {:noreply, %{state | socket: nil, backoff: min(state.backoff * 2, @reconnect_max)}}
    end
  end

  @impl true
  def handle_info({:tcp, _socket, data}, state) do
    # Each line received from APRS-IS
    Phoenix.PubSub.broadcast(Aprs.PubSub, @pubsub_topic, {:aprs_is_line, data})
    :telemetry.execute([:aprs, :is, :packet], %{size: byte_size(data)}, %{data: data})
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, _socket}, state) do
    Logger.warning("APRS-IS connection closed. Reconnecting...")
    :telemetry.execute([:aprs, :is, :disconnected], %{}, %{})
    schedule_connect(@reconnect_initial)
    {:noreply, %{state | socket: nil, backoff: @reconnect_initial}}
  end

  @impl true
  def handle_info({:tcp_error, _socket, reason}, state) do
    Logger.error("APRS-IS TCP error: #{inspect(reason)}. Reconnecting...")
    :telemetry.execute([:aprs, :is, :tcp_error], %{}, %{reason: reason})
    schedule_connect(@reconnect_initial)
    {:noreply, %{state | socket: nil, backoff: @reconnect_initial}}
  end

  @impl true
  def handle_call({:send, packet}, _from, %{socket: socket} = state) when is_port(socket) do
    :ok = :gen_tcp.send(socket, packet <> "\r\n")
    {:reply, :ok, state}
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

  defp connect_aprs_is do
    host = Application.get_env(:aprs, :aprs_is_host, ~c"rotate.aprs2.net")
    port = Application.get_env(:aprs, :aprs_is_port, 14_580)
    callsign = Application.get_env(:aprs, :aprs_is_callsign, "N0CALL")
    passcode = Application.get_env(:aprs, :aprs_is_passcode, "00000")
    filter = Application.get_env(:aprs, :aprs_is_filter, "")

    opts = [:binary, active: true, packet: :line, keepalive: true]

    case :gen_tcp.connect(host, port, opts) do
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
