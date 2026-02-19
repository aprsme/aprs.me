defmodule Aprsme.PacketProducerBackpressureTest do
  use ExUnit.Case, async: false

  alias Aprsme.PacketProducer

  # We register self() as Aprsme.Is to receive backpressure messages
  setup do
    Process.register(self(), Aprsme.Is)

    on_exit(fn ->
      try do
        Process.unregister(Aprsme.Is)
      rescue
        _ -> :ok
      end
    end)

    :ok
  end

  describe "init/1 water marks" do
    test "computes water marks from config ratios" do
      # Config has high_water_ratio: 0.8, low_water_ratio: 0.3
      {:producer, state} = PacketProducer.init(max_buffer_size: 1000)

      assert state.high_water_mark == 800
      assert state.low_water_mark == 300
      assert state.backpressure_active == false
      assert state.is_monitor_ref == nil
    end

    test "uses defaults when config ratios absent" do
      # Temporarily clear the config
      original = Application.get_env(:aprsme, :packet_pipeline)
      pipeline_without_ratios = Keyword.drop(original, [:high_water_ratio, :low_water_ratio])
      Application.put_env(:aprsme, :packet_pipeline, pipeline_without_ratios)

      {:producer, state} = PacketProducer.init(max_buffer_size: 1000)

      # Defaults: 0.8 and 0.3
      assert state.high_water_mark == 800
      assert state.low_water_mark == 300

      Application.put_env(:aprsme, :packet_pipeline, original)
    end
  end

  describe "backpressure activation on packet buffering" do
    test "sends activate when buffer crosses high water mark" do
      buffer = build_queue(799)

      state = base_state(800, 300, buffer: buffer, buffer_size: 799)

      # Adding one more packet brings us to 800 == high_water_mark
      {:noreply, [], new_state} =
        PacketProducer.handle_cast({:packet, %{sender: "TRIGGER"}}, state)

      assert_received {:backpressure, :activate}
      assert new_state.backpressure_active == true
      assert new_state.is_monitor_ref
    end

    test "does not send duplicate activate when already active" do
      buffer = build_queue(800)

      state =
        base_state(800, 300,
          buffer: buffer,
          buffer_size: 800,
          backpressure_active: true,
          is_monitor_ref: make_ref()
        )

      # Buffer overflows (drops oldest), but backpressure already active
      {:noreply, [], new_state} =
        PacketProducer.handle_cast({:packet, %{sender: "EXTRA"}}, state)

      refute_received {:backpressure, :activate}
      assert new_state.backpressure_active == true
    end

    test "skips activation when Aprsme.Is is not running" do
      # Unregister so Process.whereis returns nil
      Process.unregister(Aprsme.Is)

      buffer = build_queue(799)
      state = base_state(800, 300, buffer: buffer, buffer_size: 799)

      {:noreply, [], new_state} =
        PacketProducer.handle_cast({:packet, %{sender: "TRIGGER"}}, state)

      # No message sent, but state should still reflect we tried
      refute new_state.backpressure_active
    end
  end

  describe "backpressure deactivation on demand" do
    test "sends deactivate when demand drains buffer below low water mark" do
      # Buffer at 301, demand of 2 will drain to 299 (below low_water 300)
      buffer = build_queue(301)

      state =
        base_state(800, 300,
          buffer: buffer,
          buffer_size: 301,
          backpressure_active: true,
          is_monitor_ref: make_ref()
        )

      {:noreply, _events, new_state} = PacketProducer.handle_demand(2, state)

      assert_received {:backpressure, :deactivate}
      assert new_state.backpressure_active == false
      assert new_state.is_monitor_ref == nil
    end

    test "does not deactivate when buffer still above low water mark" do
      buffer = build_queue(305)

      state =
        base_state(800, 300,
          buffer: buffer,
          buffer_size: 305,
          backpressure_active: true,
          is_monitor_ref: make_ref()
        )

      # Demand of 2 drains to 303, still above 300
      {:noreply, _events, new_state} = PacketProducer.handle_demand(2, state)

      refute_received {:backpressure, :deactivate}
      assert new_state.backpressure_active == true
    end
  end

  describe "handle_info :DOWN" do
    test "resets backpressure state when Is process dies" do
      ref = make_ref()

      state =
        base_state(800, 300,
          backpressure_active: true,
          is_monitor_ref: ref
        )

      {:noreply, [], new_state} =
        PacketProducer.handle_info({:DOWN, ref, :process, self(), :normal}, state)

      assert new_state.backpressure_active == false
      assert new_state.is_monitor_ref == nil
    end

    test "ignores DOWN for unrelated monitors" do
      ref = make_ref()
      unrelated_ref = make_ref()

      state =
        base_state(800, 300,
          backpressure_active: true,
          is_monitor_ref: ref
        )

      {:noreply, [], new_state} =
        PacketProducer.handle_info({:DOWN, unrelated_ref, :process, self(), :normal}, state)

      # Should not reset — different ref
      assert new_state.backpressure_active == true
      assert new_state.is_monitor_ref == ref
    end
  end

  # Helper to build a queue of N dummy packets
  defp build_queue(n) do
    Enum.reduce(1..n, :queue.new(), fn i, q ->
      :queue.in(%{sender: "P#{i}"}, q)
    end)
  end

  defp base_state(high, low, overrides) do
    defaults = %{
      demand: 0,
      buffer: :queue.new(),
      buffer_size: 0,
      max_buffer_size: 1000,
      high_water_mark: high,
      low_water_mark: low,
      backpressure_active: false,
      is_monitor_ref: nil
    }

    Enum.reduce(overrides, defaults, fn {k, v}, acc -> Map.put(acc, k, v) end)
  end
end
