defmodule Aprsme.SpatialPubSubTest do
  use ExUnit.Case, async: false

  alias Aprsme.SpatialPubSub

  describe "viewport registration" do
    test "date-line-crossing viewport registers without timeout" do
      bounds = %{north: 10.0, south: -10.0, east: -170.0, west: 170.0}
      client_id = "test_dateline_#{:rand.uniform(100_000)}"

      # Before the fix, west=170 east=-170 generated ~341 grid cells.
      # Now it correctly generates ~21 cells.
      assert {:ok, _topic} = SpatialPubSub.register_viewport(client_id, bounds)

      SpatialPubSub.unregister_client(client_id)
    end

    test "normal viewport registers successfully" do
      bounds = %{north: 41.0, south: 40.0, east: -73.0, west: -74.0}
      client_id = "test_normal_#{:rand.uniform(100_000)}"

      assert {:ok, _topic} = SpatialPubSub.register_viewport(client_id, bounds)

      SpatialPubSub.unregister_client(client_id)
    end

    test "update_viewport succeeds after registration" do
      bounds = %{north: 41.0, south: 40.0, east: -73.0, west: -74.0}
      new_bounds = %{north: 42.0, south: 41.0, east: -72.0, west: -73.0}
      client_id = "test_update_#{:rand.uniform(100_000)}"

      {:ok, _topic} = SpatialPubSub.register_viewport(client_id, bounds)
      assert :ok = SpatialPubSub.update_viewport(client_id, new_bounds)

      SpatialPubSub.unregister_client(client_id)
    end

    test "re-registering an existing client replaces the old viewport without inflating counts" do
      client_id = "test_reregister_#{:rand.uniform(100_000)}"
      bounds_1 = %{north: 41.0, south: 40.0, east: -73.0, west: -74.0}
      bounds_2 = %{north: 36.0, south: 35.0, east: -79.0, west: -80.0}

      before_stats = SpatialPubSub.get_stats()

      assert {:ok, _topic} = SpatialPubSub.register_viewport(client_id, bounds_1)
      mid_stats = SpatialPubSub.get_stats()
      assert mid_stats.clients_count == before_stats.clients_count + 1

      assert {:ok, _topic} = SpatialPubSub.register_viewport(client_id, bounds_2)
      after_stats = SpatialPubSub.get_stats()

      assert after_stats.clients_count == before_stats.clients_count + 1

      state = :sys.get_state(SpatialPubSub)
      assert state.clients[client_id].bounds == bounds_2

      SpatialPubSub.unregister_client(client_id)
    end
  end

  describe "duplicate packet prevention" do
    test "does not subscribe to postgres PubSub topic" do
      # SpatialPubSub used to subscribe to "postgres:aprsme_packets" causing
      # duplicate broadcasts (once from PacketConsumer direct cast, once from PubSub).
      # Verify it no longer subscribes.
      # The SpatialPubSub process should NOT be in the subscriber list for postgres topic
      spatial_pid = Process.whereis(SpatialPubSub)

      subscribed? =
        Aprsme.PubSub
        |> Registry.lookup("postgres:aprsme_packets")
        |> Enum.any?(fn {pid, _} -> pid == spatial_pid end)

      refute subscribed?, "SpatialPubSub should not subscribe to postgres:aprsme_packets"
    end
  end

  describe "stats" do
    test "get_stats returns expected keys" do
      stats = SpatialPubSub.get_stats()

      assert is_map(stats)
      assert Map.has_key?(stats, :total_packets)
      assert Map.has_key?(stats, :grid_cells)
    end

    test "grid cells increase after registering viewport" do
      bounds = %{north: 41.0, south: 40.0, east: -73.0, west: -74.0}
      client_id = "test_stats_#{:rand.uniform(100_000)}"

      before_stats = SpatialPubSub.get_stats()
      {:ok, _topic} = SpatialPubSub.register_viewport(client_id, bounds)
      after_stats = SpatialPubSub.get_stats()

      assert after_stats.grid_cells >= before_stats.grid_cells

      SpatialPubSub.unregister_client(client_id)
    end
  end
end
