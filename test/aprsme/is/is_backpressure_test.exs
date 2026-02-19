defmodule Aprsme.Is.BackpressureTest do
  use ExUnit.Case, async: true

  alias Aprsme.Is

  # These tests call handle_info directly with constructed state,
  # so they don't need a running GenServer or real socket.

  defp base_state(overrides \\ []) do
    defaults = %{
      server: "test.aprs2.net",
      port: 14_580,
      socket: nil,
      timer: nil,
      keepalive_timer: nil,
      connected_at: DateTime.utc_now(),
      packet_stats: %{
        total_packets: 0,
        last_packet_at: nil,
        packets_per_second: 0,
        last_second_count: 0,
        last_second_timestamp: System.system_time(:second)
      },
      buffer: "",
      login_params: %{
        user_id: "TEST",
        passcode: "-1",
        filter: "r/33/-96/100"
      },
      backpressure_active: false,
      safety_valve_timer: nil
    }

    Enum.reduce(overrides, defaults, fn {k, v}, acc -> Map.put(acc, k, v) end)
  end

  describe "handle_info {:backpressure, :activate}" do
    test "no-op when socket is nil" do
      state = base_state()

      {:noreply, new_state} = Is.handle_info({:backpressure, :activate}, state)

      assert new_state.backpressure_active == false
      assert new_state.safety_valve_timer == nil
    end

    test "no-op when already active" do
      timer_ref = make_ref()
      state = base_state(backpressure_active: true, safety_valve_timer: timer_ref)

      {:noreply, new_state} = Is.handle_info({:backpressure, :activate}, state)

      # State unchanged
      assert new_state.backpressure_active == true
      assert new_state.safety_valve_timer == timer_ref
    end
  end

  describe "handle_info {:backpressure, :deactivate}" do
    test "no-op when not active" do
      state = base_state()

      {:noreply, new_state} = Is.handle_info({:backpressure, :deactivate}, state)

      assert new_state.backpressure_active == false
    end

    test "clears flag when socket is nil and active" do
      state = base_state(backpressure_active: true, safety_valve_timer: make_ref())

      {:noreply, new_state} = Is.handle_info({:backpressure, :deactivate}, state)

      assert new_state.backpressure_active == false
      assert new_state.safety_valve_timer == nil
    end
  end

  describe "handle_info :backpressure_safety_valve" do
    test "no-op when not active" do
      state = base_state()

      {:noreply, new_state} = Is.handle_info(:backpressure_safety_valve, state)

      assert new_state.backpressure_active == false
    end

    test "clears state when socket is nil" do
      state = base_state(backpressure_active: true, safety_valve_timer: make_ref())

      {:noreply, new_state} = Is.handle_info(:backpressure_safety_valve, state)

      assert new_state.backpressure_active == false
      assert new_state.safety_valve_timer == nil
    end
  end
end
