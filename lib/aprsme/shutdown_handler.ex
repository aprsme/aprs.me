defmodule Aprsme.ShutdownHandler do
  @moduledoc """
  Handles graceful shutdown of the application, ensuring connections are properly drained
  before the process terminates.
  """

  use GenServer

  require Logger

  # 30 seconds
  @default_drain_timeout_ms 30_000
  # 15 seconds to mark unhealthy - gives load balancer more time to detect
  @health_check_grace_ms 15_000

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    # Trap exit signals to handle shutdown gracefully
    Process.flag(:trap_exit, true)

    # Register for SIGTERM handling
    # Note: In Elixir/OTP, we handle this through the application shutdown
    # The VM will send terminate callbacks when receiving SIGTERM

    state = %{
      shutting_down: false,
      drain_timeout: "DRAIN_TIMEOUT_MS" |> System.get_env("#{@default_drain_timeout_ms}") |> String.to_integer()
    }

    {:ok, state}
  end

  @doc """
  Initiates graceful shutdown process
  """
  def shutdown do
    GenServer.call(__MODULE__, :shutdown, :infinity)
  end

  @doc """
  Check if the application is shutting down
  """
  def shutting_down? do
    GenServer.call(__MODULE__, :shutting_down?)
  rescue
    _ -> false
  end

  # Server callbacks

  def handle_call(:shutdown, _from, state) do
    if state.shutting_down do
      {:reply, :already_shutting_down, state}
    else
      Logger.info("Initiating graceful shutdown...")
      new_state = initiate_shutdown(state)
      {:reply, :ok, new_state}
    end
  end

  def handle_call(:shutting_down?, _from, state) do
    {:reply, state.shutting_down, state}
  end

  # Handle terminate callback from OTP when SIGTERM is received
  def terminate(reason, state) do
    Logger.info("ShutdownHandler terminating: #{inspect(reason)}")

    if not state.shutting_down do
      initiate_shutdown(state)
      # Give some time for the shutdown process
      Process.sleep(state.drain_timeout)
    end

    :ok
  end

  def handle_info({:EXIT, _pid, reason}, state) do
    Logger.debug("Received EXIT signal: #{inspect(reason)}")
    {:noreply, state}
  end

  def handle_info(:begin_connection_drain, state) do
    Logger.info("Beginning connection drain phase...")

    # Stop accepting new connections
    stop_accepting_connections()

    # Don't notify users - let connections drain naturally
    # Only broadcast to internal systems if needed

    # Give connections time to drain
    Process.send_after(self(), :force_shutdown, state.drain_timeout)

    {:noreply, state}
  end

  def handle_info(:force_shutdown, state) do
    Logger.info("Grace period expired, forcing shutdown...")

    # Get connection stats before shutdown
    log_connection_stats()

    # Terminate remaining connections
    terminate_remaining_connections()

    # Exit the application
    System.stop(0)

    {:noreply, state}
  end

  # Private functions

  defp initiate_shutdown(state) do
    # Mark as shutting down
    new_state = %{state | shutting_down: true}

    # First, mark the application as unhealthy for load balancer
    mark_unhealthy()

    # Give load balancer time to stop sending new requests
    Process.send_after(self(), :begin_connection_drain, @health_check_grace_ms)

    new_state
  end

  defp mark_unhealthy do
    # This will cause health checks to fail, prompting the load balancer
    # to stop sending new traffic to this instance
    Application.put_env(:aprsme, :health_status, :draining)
    Logger.info("Marked application as unhealthy for load balancer")
  end

  defp stop_accepting_connections do
    # Stop the endpoint from accepting new connections
    # Existing connections continue to be served
    if Application.get_env(:aprsme, AprsmeWeb.Endpoint)[:server] do
      # Find and suspend the acceptor processes
      AprsmeWeb.Endpoint
      |> Process.whereis()
      |> suspend_acceptors()
    else
      :ok
    end
  end

  defp suspend_acceptors(nil), do: :ok

  defp suspend_acceptors(_endpoint_pid) do
    # With Bandit (not Ranch), we need a different approach
    # We'll use the health check to stop new connections
    Logger.info("New connection acceptance disabled via health checks")
    :ok
  end

  defp log_connection_stats do
    # Log current connection statistics
    websocket_count = count_websocket_connections()
    http_count = count_http_connections()

    Logger.info("Current connections - WebSockets: #{websocket_count}, HTTP: #{http_count}")
  end

  # Count active WebSocket connections through presence tracking
  defp count_websocket_connections do
    presences = Aprsme.Presence.list("map:live")
    map_size(presences)
  rescue
    _ -> 0
  end

  # Count active HTTP connections
  # Note: This is challenging to get accurately with Bandit
  # We'll use a simple estimate based on process count
  defp count_http_connections do
    endpoint_pid = Process.whereis(AprsmeWeb.Endpoint)

    if endpoint_pid do
      # Count child processes which include connections
      Supervisor.count_children(endpoint_pid).active
    else
      0
    end
  rescue
    _ -> 0
  end

  defp terminate_remaining_connections do
    Logger.info("Terminating remaining connections...")

    # Don't broadcast to users - just terminate quietly
    # The load balancer should have already moved traffic to other pods

    # Give a brief moment for final cleanup
    Process.sleep(500)

    # Stop the endpoint
    Application.stop(:aprsme)
  end
end
