defmodule Aprsme.PacketConsumerPoolTest do
  use ExUnit.Case, async: true

  alias Aprsme.PacketConsumerPool

  describe "child_spec/1" do
    test "returns expected spec" do
      spec = PacketConsumerPool.child_spec([])

      assert spec.id == PacketConsumerPool
      assert spec.type == :supervisor
    end
  end

  describe "init/1" do
    test "returns supervisor flags with one_for_one strategy and 3 children by default" do
      {:ok, {sup_flags, children}} = PacketConsumerPool.init([])

      assert sup_flags.strategy == :one_for_one
      assert length(children) == 3
    end

    test "each child has a unique id of {Aprsme.PacketConsumer, index}" do
      {:ok, {_sup_flags, children}} = PacketConsumerPool.init([])

      ids = Enum.map(children, & &1.id)

      assert {Aprsme.PacketConsumer, 1} in ids
      assert {Aprsme.PacketConsumer, 2} in ids
      assert {Aprsme.PacketConsumer, 3} in ids
      assert length(ids) == length(Enum.uniq(ids))
    end

    test "creates 5 children when num_consumers: 5 is passed in opts" do
      {:ok, {_sup_flags, children}} = PacketConsumerPool.init(num_consumers: 5)

      assert length(children) == 5

      ids = Enum.map(children, & &1.id)

      for index <- 1..5 do
        assert {Aprsme.PacketConsumer, index} in ids
      end
    end

    test "max_demand is divided evenly among consumers" do
      original_config = Application.get_env(:aprsme, :packet_pipeline, [])

      on_exit(fn ->
        Application.put_env(:aprsme, :packet_pipeline, original_config)
      end)

      Application.put_env(:aprsme, :packet_pipeline, max_demand: 500)

      {:ok, {_sup_flags, children}} = PacketConsumerPool.init(num_consumers: 5)

      expected_demand = div(500, 5)

      for child <- children do
        {Aprsme.PacketConsumer, :start_link, [opts]} = child.start
        assert opts[:max_demand] == expected_demand
        assert opts[:subscribe_to] == [{Aprsme.PacketProducer, max_demand: expected_demand}]
      end
    end

    test "uses default max_demand of 250 when not configured" do
      original_config = Application.get_env(:aprsme, :packet_pipeline, [])

      on_exit(fn ->
        Application.put_env(:aprsme, :packet_pipeline, original_config)
      end)

      Application.delete_env(:aprsme, :packet_pipeline)

      {:ok, {_sup_flags, children}} = PacketConsumerPool.init([])

      expected_demand = div(250, 3)

      child = List.first(children)
      {Aprsme.PacketConsumer, :start_link, [opts]} = child.start
      assert opts[:max_demand] == expected_demand
    end
  end
end
