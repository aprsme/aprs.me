defmodule Aprs.PacketReplayTest do
  use Aprs.DataCase, async: false

  import Mox

  alias Aprs.PacketReplay
  alias AprsWeb.Endpoint
  alias Phoenix.Socket.Broadcast

  setup :verify_on_exit!

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

    mock_packet = %{
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
    test "stops an active replay session", %{user_id: user_id, bounds: bounds} do
      {:ok, pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)
      assert Process.alive?(pid)

      assert :ok = PacketReplay.stop_replay(user_id)

      # Give it a moment to stop
      Process.sleep(10)
      refute Process.alive?(pid)
    end

    test "returns error when no replay session exists" do
      assert {:error, :not_found} = PacketReplay.stop_replay("nonexistent_user")
    end
  end

  describe "pause_replay/1" do
    test "pauses an active replay session", %{user_id: user_id, bounds: bounds} do
      {:ok, _pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      assert :ok = PacketReplay.pause_replay(user_id)

      # Verify state
      info = PacketReplay.get_replay_info(user_id)
      assert info.paused == true

      # Clean up
      PacketReplay.stop_replay(user_id)
    end

    test "fails when replay session doesn't exist" do
      assert_raise RuntimeError, fn ->
        PacketReplay.pause_replay("nonexistent_user")
      end
    end
  end

  describe "resume_replay/1" do
    test "resumes a paused replay session", %{user_id: user_id, bounds: bounds} do
      {:ok, _pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      # First pause it
      :ok = PacketReplay.pause_replay(user_id)
      info = PacketReplay.get_replay_info(user_id)
      assert info.paused == true

      # Then resume it
      assert :ok = PacketReplay.resume_replay(user_id)

      info = PacketReplay.get_replay_info(user_id)
      assert info.paused == false

      # Clean up
      PacketReplay.stop_replay(user_id)
    end

    test "acknowledges resume on already running session", %{user_id: user_id, bounds: bounds} do
      {:ok, _pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      # Resume without pausing first
      assert :ok = PacketReplay.resume_replay(user_id)

      info = PacketReplay.get_replay_info(user_id)
      assert info.paused == false

      # Clean up
      PacketReplay.stop_replay(user_id)
    end
  end

  describe "set_replay_speed/2" do
    test "sets replay speed", %{user_id: user_id, bounds: bounds} do
      {:ok, _pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      assert :ok = PacketReplay.set_replay_speed(user_id, 10.0)

      info = PacketReplay.get_replay_info(user_id)
      assert info.replay_speed == 10.0

      # Clean up
      PacketReplay.stop_replay(user_id)
    end

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
    test "updates filters and restarts replay", %{user_id: user_id, bounds: bounds} do
      {:ok, _pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      new_filters = [callsign: "N1CALL", limit: 2000]
      assert :ok = PacketReplay.update_filters(user_id, new_filters)

      info = PacketReplay.get_replay_info(user_id)
      assert info.callsign == "N1CALL"

      # Clean up
      PacketReplay.stop_replay(user_id)
    end

    test "validates bounds format in filters", %{user_id: user_id, bounds: bounds} do
      {:ok, _pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      # Valid bounds update
      new_bounds = [-75.0, 39.0, -72.0, 42.0]
      assert :ok = PacketReplay.update_filters(user_id, bounds: new_bounds)

      info = PacketReplay.get_replay_info(user_id)
      assert info.bounds == new_bounds

      # Invalid bounds should keep old bounds
      assert :ok = PacketReplay.update_filters(user_id, bounds: "invalid")

      info = PacketReplay.get_replay_info(user_id)
      # Should keep the valid bounds
      assert info.bounds == new_bounds

      # Clean up
      PacketReplay.stop_replay(user_id)
    end

    test "validates filters must be a list" do
      user_id = "test_user"

      assert_raise FunctionClauseError, fn ->
        PacketReplay.update_filters(user_id, %{callsign: "N0CALL"})
      end
    end
  end

  describe "get_replay_info/1" do
    test "returns replay session information", %{user_id: user_id, bounds: bounds} do
      start_time = DateTime.add(DateTime.utc_now(), -1800, :second)
      end_time = DateTime.utc_now()

      opts = [
        user_id: user_id,
        bounds: bounds,
        callsign: "N0CALL",
        start_time: start_time,
        end_time: end_time,
        replay_speed: 3.0,
        limit: 2500,
        with_position: true
      ]

      {:ok, _pid} = PacketReplay.start_replay(opts)

      info = PacketReplay.get_replay_info(user_id)

      assert info.user_id == user_id
      assert info.bounds == bounds
      assert info.callsign == "N0CALL"
      assert info.start_time == start_time
      assert info.end_time == end_time
      assert info.replay_speed == 3.0
      assert info.with_position == true
      assert info.packets_sent == 0
      assert info.paused == false
      assert %DateTime{} = info.replay_started_at

      # Clean up
      PacketReplay.stop_replay(user_id)
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
    test "handles start_replay message with mock packets", %{mock_packet: mock_packet} do
      # Set up mock expectations
      MockPackets
      |> expect(:get_historical_packet_count, fn _opts -> 1 end)
      |> expect(:stream_packets_for_replay, fn _opts ->
        Stream.unfold([{0.1, mock_packet}], fn
          [packet | rest] -> {packet, rest}
          [] -> nil
        end)
      end)

      user_id = "test_user"
      bounds = [-74.0, 40.0, -73.0, 41.0]

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

      # Subscribe to the broadcast topic to verify messages
      Endpoint.subscribe("replay:#{user_id}")

      result = PacketReplay.handle_info(:start_replay, state)

      assert {:noreply, new_state} = result
      assert new_state.last_packet_time == mock_packet.received_at
      assert is_reference(new_state.replay_timer)

      # Should have received broadcast
      assert_receive %Broadcast{
        topic: "replay:" <> _,
        event: "replay_started"
      }
    end

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

    test "broadcasts packet and schedules next when not paused", %{mock_packet: mock_packet} do
      next_packet = %{mock_packet | id: "test_packet_2"}

      stream =
        Stream.unfold([{0.1, next_packet}], fn
          [packet | rest] -> {packet, rest}
          [] -> nil
        end)

      state = %{
        user_id: "test_user",
        replay_topic: "replay:test_user",
        paused: false,
        replay_timer: nil,
        packets_sent: 0,
        last_packet_time: nil
      }

      # Subscribe to broadcasts
      Endpoint.subscribe("replay:test_user")

      result = PacketReplay.handle_info({:send_packet, mock_packet, stream}, state)

      assert {:noreply, new_state} = result
      assert new_state.packets_sent == 1
      assert new_state.last_packet_time == next_packet.received_at
      assert is_reference(new_state.replay_timer)

      # Should have received packet broadcast
      assert_receive %Broadcast{
        event: "historical_packet",
        payload: %{is_historical: true}
      }
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

  describe "integration tests" do
    test "full replay lifecycle", %{user_id: user_id, bounds: bounds, mock_packet: mock_packet} do
      # Set up mock for full lifecycle
      MockPackets
      |> expect(:get_historical_packet_count, fn _opts -> 1 end)
      |> expect(:stream_packets_for_replay, fn _opts ->
        # Single packet stream
        Stream.unfold([{0.001, mock_packet}], fn
          [packet | rest] -> {packet, rest}
          [] -> nil
        end)
      end)

      # Start replay
      {:ok, pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      # Subscribe to events
      Endpoint.subscribe("replay:#{user_id}")

      # Should receive start event
      assert_receive %Broadcast{
                       event: "replay_started"
                     },
                     1000

      # Should receive packet event quickly (1ms delay)
      assert_receive %Broadcast{
                       event: "historical_packet"
                     },
                     1000

      # Should receive completion event
      assert_receive %Broadcast{
                       event: "replay_complete"
                     },
                     1000

      # Process should terminate
      Process.sleep(50)
      refute Process.alive?(pid)
    end

    test "pause and resume functionality", %{user_id: user_id, bounds: bounds} do
      {:ok, pid} = PacketReplay.start_replay(user_id: user_id, bounds: bounds)

      # Subscribe to events
      Endpoint.subscribe("replay:#{user_id}")

      # Pause
      :ok = PacketReplay.pause_replay(user_id)

      assert_receive %Broadcast{
        event: "replay_paused"
      }

      info = PacketReplay.get_replay_info(user_id)
      assert info.paused == true

      # Resume
      :ok = PacketReplay.resume_replay(user_id)

      assert_receive %Broadcast{
        event: "replay_resumed"
      }

      info = PacketReplay.get_replay_info(user_id)
      assert info.paused == false

      # Clean up
      GenServer.stop(pid)
    end
  end
end
