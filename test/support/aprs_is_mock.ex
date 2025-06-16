defmodule AprsIsMock do
  @moduledoc """
  Mock implementation of Aprs.Is for testing purposes.
  This ensures no external APRS connections are made during tests.
  """

  use GenServer

  require Logger

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    Logger.info("Starting APRS.Is mock for testing")

    # Mock connection state
    initial_state = %{
      connected: false,
      server: "mock.aprs.test",
      port: 14_580,
      connected_at: nil,
      login_id: "TEST",
      filter: "r/33/-96/100",
      packet_stats: %{
        total_packets: 0,
        last_packet_at: nil,
        packets_per_second: 0,
        last_second_count: 0,
        last_second_timestamp: System.system_time(:second)
      },
      stored_packet_count: 0
    }

    {:ok, initial_state}
  end

  # Client API - Mock implementations

  def stop do
    Logger.info("Stopping APRS.Is mock")
    GenServer.stop(__MODULE__, :normal)
  end

  def get_status do
    case Process.whereis(__MODULE__) do
      nil ->
        # Mock disconnected state
        %{
          connected: false,
          server: "mock.aprs.test",
          port: 14_580,
          connected_at: nil,
          uptime_seconds: 0,
          login_id: "TEST",
          filter: "r/33/-96/100",
          packet_stats: %{
            total_packets: 0,
            last_packet_at: nil,
            packets_per_second: 0,
            last_second_count: 0,
            last_second_timestamp: System.system_time(:second)
          },
          stored_packet_count: 0
        }

      _pid ->
        try do
          GenServer.call(__MODULE__, :get_status, 5000)
        catch
          :exit, _ ->
            # Fallback mock state
            %{
              connected: false,
              server: "mock.aprs.test",
              port: 14_580,
              connected_at: nil,
              uptime_seconds: 0,
              login_id: "TEST",
              filter: "r/33/-96/100",
              packet_stats: %{
                total_packets: 0,
                last_packet_at: nil,
                packets_per_second: 0,
                last_second_count: 0,
                last_second_timestamp: System.system_time(:second)
              },
              stored_packet_count: 0
            }
        end
    end
  end

  def set_filter(filter_string) do
    Logger.debug("Mock: Setting filter to #{filter_string}")
    :ok
  end

  def list_active_filters do
    Logger.debug("Mock: Listing active filters")
    :ok
  end

  def send_message(from, to, message) do
    Logger.debug("Mock: Sending message from #{from} to #{to}: #{message}")
    :ok
  end

  def send_message(message) do
    Logger.debug("Mock: Sending message: #{message}")
    GenServer.call(__MODULE__, {:send_message, message})
  end

  # Server callbacks

  @impl true
  def handle_call({:send_message, message}, _from, state) do
    Logger.debug("Mock: Handling send message: #{message}")
    {:reply, :ok, state}
  end

  def handle_call(:get_status, _from, state) do
    uptime_seconds =
      if state.connected_at do
        DateTime.diff(DateTime.utc_now(), state.connected_at, :second)
      else
        0
      end

    mock_status =
      Map.put(state, :uptime_seconds, uptime_seconds)

    {:reply, mock_status, state}
  end

  def handle_call({:set_connection_state, connected}, _from, state) do
    new_state = %{
      state
      | connected: connected,
        connected_at: if(connected, do: DateTime.utc_now())
    }

    {:reply, :ok, new_state}
  end

  @impl true
  def handle_info(msg, state) do
    Logger.debug("Mock: Received unexpected message: #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def terminate(reason, _state) do
    Logger.info("Mock APRS.Is terminating: #{inspect(reason)}")
    :ok
  end

  # Helper functions for testing

  def simulate_packet(packet_data) do
    # Simulate receiving an APRS packet for testing purposes.
    # This can be used in tests to trigger packet processing without
    # connecting to external servers.
    Logger.debug("Mock: Simulating packet: #{inspect(packet_data)}")

    # Broadcast to live clients like the real implementation would
    AprsWeb.Endpoint.broadcast("aprs_messages", "packet", packet_data)

    :ok
  end

  def simulate_connection_state(connected \\ true) do
    # Simulate connection state changes for testing.
    GenServer.call(__MODULE__, {:set_connection_state, connected})
  end
end
