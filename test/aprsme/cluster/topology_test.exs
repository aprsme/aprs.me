defmodule Aprsme.Cluster.TopologyTest do
  use ExUnit.Case, async: false

  alias Aprsme.Cluster.Topology
  alias Cluster.Strategy.Gossip

  setup do
    on_exit(fn ->
      Application.put_env(:aprsme, :cluster_enabled, false)
      Application.put_env(:libcluster, :topologies, [])
    end)

    :ok
  end

  describe "start_link/1" do
    test "returns :ignore" do
      assert :ignore = Topology.start_link([])
    end
  end

  describe "child_spec/1" do
    test "returns no-op spec when cluster is disabled" do
      Application.put_env(:aprsme, :cluster_enabled, false)

      spec = Topology.child_spec([])

      assert %{id: Topology, start: {Topology, :start_link, [[]]}, type: :worker} = spec
    end

    test "returns no-op spec when cluster is enabled but no topologies configured" do
      Application.put_env(:aprsme, :cluster_enabled, true)
      Application.put_env(:libcluster, :topologies, [])

      spec = Topology.child_spec([])

      assert %{id: Topology, start: {Topology, :start_link, [[]]}, type: :worker} = spec
    end

    test "returns Cluster.Supervisor tuple when cluster is enabled with valid topologies" do
      Application.put_env(:aprsme, :cluster_enabled, true)
      Application.put_env(:libcluster, :topologies, gossip: [strategy: Gossip])

      spec = Topology.child_spec([])

      assert {Cluster.Supervisor, [topologies, supervisor_opts]} = spec
      assert topologies == [gossip: [strategy: Gossip]]
      assert Keyword.get(supervisor_opts, :name) == Aprsme.ClusterSupervisor
    end
  end
end
