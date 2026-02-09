defmodule Aprsme.PacketCounterTest do
  use Aprsme.DataCase, async: true

  import Ecto.Query

  alias Aprsme.PacketCounter
  alias Aprsme.Repo

  describe "get_count/0" do
    test "returns 0 when no records exist" do
      Repo.delete_all(PacketCounter)

      assert PacketCounter.get_count() == 0
    end
  end

  describe "get_count/1" do
    test "returns the count after inserting a record" do
      # Update the existing seeded row rather than inserting a duplicate
      Repo.update_all(from(pc in PacketCounter, where: pc.counter_type == "total_packets"), set: [count: 42])
      assert PacketCounter.get_count("total_packets") == 42
    end

    test "returns 0 for a counter_type that does not exist" do
      assert PacketCounter.get_count("nonexistent_type") == 0
    end

    test "returns the correct count for a custom counter_type" do
      Repo.insert!(%PacketCounter{counter_type: "daily_packets", count: 100})

      assert PacketCounter.get_count("daily_packets") == 100
    end
  end

  describe "subscribe_to_changes/0" do
    test "returns :ok" do
      assert PacketCounter.subscribe_to_changes() == :ok
    end
  end
end
