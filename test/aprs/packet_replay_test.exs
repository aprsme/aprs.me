defmodule Aprs.PacketReplayTest do
  use Aprs.DataCase, async: false

  import Mox

  alias Aprs.PacketReplay
  alias AprsWeb.Endpoint
  alias Phoenix.Socket.Broadcast

  setup :verify_on_exit!

  # Define a simple test packet struct
  defmodule TestPacket do
    @moduledoc false
    defstruct [:id, :received_at, :lat, :lon, :sender, :data_type]
  end

  setup do
    # Mock the Packets module functions
    Mox.defmock(MockPackets, for: Aprs.PacketsBehaviour)
    Application.put_env(:aprs, :packets_module, MockPackets)

    # Start a test registry for the packet replay processes
    start_supervised!({Registry, keys: :unique, name: Aprs.ReplayRegistry})

    # Example test data
    user_id = "test_user_#{:rand.uniform(10_000)}"
    # New York area
    bounds = [-74.0, 40.0, -73.0, 41.0]

    mock_packet = %TestPacket{
      id: "test_packet_1",
      received_at: DateTime.utc_now(),
      lat: 40.5,
      lon: -73.5,
      sender: "N0CALL",
      data_type: "position"
    }

    %{
      user_id: user_id,
      bounds: bounds,
      mock_packet: mock_packet
    }
  end

  describe "start_replay/1" do
    test "starts replay with required options", %{user_id: user_id, bounds: bounds} do
      opts = [user_id: user_id, bounds: bounds]

      assert {:ok, pid} = PacketReplay.start_replay(opts)
      assert is_pid(pid)
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end

    test "raises error when bounds are missing", %{user_id: user_id} do
      opts = [user_id: user_id]

      assert_raise ArgumentError, "Map bounds are required for packet replay", fn ->
        PacketReplay.start_replay(opts)
      end
    end

    test "starts replay with all optional parameters", %{user_id: user_id, bounds: bounds} do
      opts = [
        user_id: user_id,
        bounds: bounds,
        callsign: "N0CALL",
        start_time: DateTime.add(DateTime.utc_now(), -3600, :second),
        end_time: DateTime.utc_now(),
        replay_speed: 2.0,
        limit: 1000,
        with_position: true
      ]

      assert {:ok, pid} = PacketReplay.start_replay(opts)
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end

    test "fails when user_id is missing", %{bounds: bounds} do
      opts = [bounds: bounds]

      assert_raise KeyError, fn ->
        PacketReplay.start_replay(opts)
      end
    end

    test "registers process with unique name per user", %{bounds: bounds} do
      user_id1 = "user1"
      user_id2 = "user2"

      {:ok, pid1} = PacketReplay.start_replay(user_id: user_id1, bounds: bounds)
      {:ok, pid2} = PacketReplay.start_replay(user_id: user_id2, bounds: bounds)

      assert pid1 != pid2
      assert Process.alive?(pid1)
      assert Process.alive?(pid2)

      # Clean up
      GenServer.stop(pid1)
      GenServer.stop(pid2)
    end
  end

  describe "stop_replay/1" do
    test "returns error when no replay session exists" do
      assert {:error, :not_found} = PacketReplay.stop_replay("nonexistent_user")
    end
  end

  describe "set_replay_speed/2" do
    test "validates speed is positive number" do
      user_id = "test_user"

      # These should raise function clause errors
      assert_raise FunctionClauseError, fn ->
        PacketReplay.set_replay_speed(user_id, 0)
      end

      assert_raise FunctionClauseError, fn ->
        PacketReplay.set_replay_speed(user_id, -1.0)
      end

      assert_raise FunctionClauseError, fn ->
        PacketReplay.set_replay_speed(user_id, "invalid")
      end
    end
  end

  describe "update_filters/2" do
    test "validates filters must be a list" do
      user_id = "test_user"

      assert_raise FunctionClauseError, fn ->
        PacketReplay.update_filters(user_id, %{callsign: "N0CALL"})
      end
    end
  end

  describe "init/1" do
    test "initializes with default values", %{user_id: user_id, bounds: bounds} do
      opts = [user_id: user_id, bounds: bounds]

      assert {:ok, state} = PacketReplay.init(opts)

      assert state.user_id == user_id
      assert state.bounds == bounds
      # default
      assert state.replay_speed == 5.0
      # default
      assert state.limit == 5000
      # default
      assert state.with_position == true
      assert state.paused == false
      assert state.packets_sent == 0
      assert is_nil(state.callsign)
      assert is_nil(state.region)
      assert is_nil(state.replay_timer)
      assert is_nil(state.last_packet_time)
      assert %DateTime{} = state.start_time
      assert %DateTime{} = state.end_time
      assert %DateTime{} = state.replay_started_at
    end

    test "respects custom options", %{user_id: user_id, bounds: bounds} do
      start_time = DateTime.add(DateTime.utc_now(), -1800, :second)
      end_time = DateTime.utc_now()

      opts = [
        user_id: user_id,
        bounds: bounds,
        callsign: "N0CALL",
        start_time: start_time,
        end_time: end_time,
        replay_speed: 2.0,
        limit: 1000,
        with_position: false
      ]

      assert {:ok, state} = PacketReplay.init(opts)

      assert state.callsign == "N0CALL"
      assert state.start_time == start_time
      assert state.end_time == end_time
      assert state.replay_speed == 2.0
      assert state.limit == 1000
      assert state.with_position == false
    end

    test "limits start_time to 1 hour ago maximum", %{user_id: user_id, bounds: bounds} do
      # Try to set start time to 2 hours ago
      old_start_time = DateTime.add(DateTime.utc_now(), -7200, :second)

      opts = [
        user_id: user_id,
        bounds: bounds,
        start_time: old_start_time
      ]

      assert {:ok, state} = PacketReplay.init(opts)

      # Should be limited to 1 hour ago (3600 seconds)
      one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)
      assert DateTime.diff(state.start_time, one_hour_ago, :second) >= -10
    end

    test "sets replay topic correctly", %{user_id: user_id, bounds: bounds} do
      opts = [user_id: user_id, bounds: bounds]

      assert {:ok, state} = PacketReplay.init(opts)

      assert state.replay_topic == "replay:#{user_id}"
    end
  end

  describe "handle_info(:start_replay)" do
    test "handles empty packet stream", %{user_id: user_id, bounds: bounds} do
      # Set up mock expectations for empty stream
      MockPackets
      |> expect(:get_historical_packet_count, fn _opts -> 0 end)
      |> expect(:stream_packets_for_replay, fn _opts ->
        Stream.unfold([], fn
          [] -> nil
        end)
      end)

      state = %{
        user_id: user_id,
        replay_topic: "replay:#{user_id}",
        replay_speed: 5.0,
        start_time: DateTime.add(DateTime.utc_now(), -3600, :second),
        end_time: DateTime.utc_now(),
        region: nil,
        bounds: bounds,
        callsign: nil,
        with_position: true,
        limit: 5000,
        paused: false,
        packets_sent: 0,
        replay_started_at: DateTime.utc_now(),
        replay_timer: nil,
        last_packet_time: nil
      }

      # Subscribe to the broadcast topic
      Endpoint.subscribe("replay:#{user_id}")

      result = PacketReplay.handle_info(:start_replay, state)

      assert {:stop, :normal, ^state} = result

      # Should have received completion broadcast
      assert_receive %Broadcast{
        topic: "replay:" <> _,
        event: "replay_complete",
        payload: %{packets_sent: 0, message: "No matching packets found for replay"}
      }
    end
  end

  describe "handle_info({:send_packet, packet, stream})" do
    test "reschedules packet when paused", %{mock_packet: mock_packet} do
      stream = Stream.unfold([], fn _ -> nil end)

      state = %{
        user_id: "test_user",
        replay_topic: "replay:test_user",
        paused: true,
        replay_timer: nil,
        packets_sent: 0,
        last_packet_time: nil
      }

      result = PacketReplay.handle_info({:send_packet, mock_packet, stream}, state)

      assert {:noreply, new_state} = result
      assert is_reference(new_state.replay_timer)
    end

    test "completes replay when no more packets", %{mock_packet: mock_packet} do
      # Empty stream - no more packets
      stream = Stream.unfold([], fn _ -> nil end)

      state = %{
        user_id: "test_user",
        replay_topic: "replay:test_user",
        paused: false,
        replay_timer: nil,
        packets_sent: 5,
        last_packet_time: nil
      }

      # Subscribe to broadcasts
      Endpoint.subscribe("replay:test_user")

      result = PacketReplay.handle_info({:send_packet, mock_packet, stream}, state)

      assert {:stop, :normal, new_state} = result
      assert new_state.packets_sent == 6

      # Should have received completion broadcast
      assert_receive %Broadcast{
        event: "replay_complete",
        payload: %{packets_sent: 6, message: "Replay complete"}
      }
    end
  end

  describe "terminate/2" do
    test "cleans up timers and broadcasts stop message" do
      timer_ref = make_ref()

      state = %{
        user_id: "test_user",
        replay_topic: "replay:test_user",
        replay_timer: timer_ref,
        packets_sent: 10
      }

      # Subscribe to broadcasts
      Endpoint.subscribe("replay:test_user")

      result = PacketReplay.terminate(:normal, state)

      assert result == :ok

      # Should have received stop broadcast
      assert_receive %Broadcast{
        event: "replay_stopped",
        payload: %{packets_sent: 10, message: "Replay stopped"}
      }
    end

    test "handles state without timer" do
      state = %{
        user_id: "test_user",
        replay_topic: "replay:test_user",
        replay_timer: nil,
        packets_sent: 0
      }

      # Subscribe to broadcasts
      Endpoint.subscribe("replay:test_user")

      result = PacketReplay.terminate(:shutdown, state)

      assert result == :ok

      # Should still broadcast stop message
      assert_receive %Broadcast{
        event: "replay_stopped"
      }
    end
  end
end
