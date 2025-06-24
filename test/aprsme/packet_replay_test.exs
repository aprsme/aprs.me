defmodule Aprsme.PacketReplayTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.PacketReplay

  setup do
    start_supervised!({Registry, keys: :unique, name: Aprsme.ReplayRegistry})
    :ok
  end

  describe "start_replay/1" do
    test "raises error when bounds are missing" do
      user_id = "test_user"
      opts = [user_id: user_id]

      assert_raise ArgumentError, "Map bounds are required for packet replay", fn ->
        PacketReplay.start_replay(opts)
      end
    end

    test "fails when user_id is missing" do
      bounds = [-74.0, 40.0, -73.0, 41.0]
      opts = [bounds: bounds]

      assert_raise KeyError, fn ->
        PacketReplay.start_replay(opts)
      end
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
    test "initializes with default values" do
      user_id = "test_user"
      bounds = [-74.0, 40.0, -73.0, 41.0]
      opts = [user_id: user_id, bounds: bounds]

      assert {:ok, state} = PacketReplay.init(opts)

      assert state.user_id == user_id
      assert state.bounds == bounds
      assert state.replay_speed == 5.0
      assert state.limit == 5000
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

    test "respects custom options" do
      user_id = "test_user"
      bounds = [-74.0, 40.0, -73.0, 41.0]
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

    test "limits start_time to 1 hour ago maximum" do
      user_id = "test_user"
      bounds = [-74.0, 40.0, -73.0, 41.0]
      old_start_time = DateTime.add(DateTime.utc_now(), -7200, :second)

      opts = [
        user_id: user_id,
        bounds: bounds,
        start_time: old_start_time
      ]

      assert {:ok, state} = PacketReplay.init(opts)

      one_hour_ago = DateTime.add(DateTime.utc_now(), -3600, :second)
      assert DateTime.diff(state.start_time, one_hour_ago, :second) >= -10
    end

    test "sets replay topic correctly" do
      user_id = "test_user"
      bounds = [-74.0, 40.0, -73.0, 41.0]
      opts = [user_id: user_id, bounds: bounds]

      assert {:ok, state} = PacketReplay.init(opts)

      assert state.replay_topic == "replay:#{user_id}"
    end
  end

  describe "module constants and specs" do
    test "has correct topic constant" do
      assert Code.ensure_loaded?(PacketReplay)
    end

    test "has correct typespec for init" do
      result = PacketReplay.init(user_id: "test", bounds: [0, 0, 1, 1])
      assert {:ok, _state} = result

      result2 = PacketReplay.init(user_id: "test", bounds: [0, 0, 1, 1], replay_speed: 2.0)
      assert {:ok, _state} = result2
    end
  end
end
