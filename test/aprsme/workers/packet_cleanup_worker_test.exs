defmodule Aprsme.Workers.PacketCleanupWorkerTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.BadPacket
  alias Aprsme.PartitionManager
  alias Aprsme.Workers.PacketCleanupWorker

  describe "perform/1 with cleanup_days" do
    test "drops old partitions via PartitionManager" do
      # Ensure current partitions exist
      {:ok, _} = PartitionManager.ensure_partitions_exist()

      # Create an old partition (30 days ago)
      old_date = Date.add(Date.utc_today(), -30)
      name = PartitionManager.partition_name(old_date)
      {from_dt, to_dt} = PartitionManager.partition_range(old_date)

      Repo.query!(
        "CREATE TABLE IF NOT EXISTS #{name} PARTITION OF packets FOR VALUES FROM ('#{DateTime.to_iso8601(from_dt)}') TO ('#{DateTime.to_iso8601(to_dt)}')"
      )

      # Verify old partition exists
      assert name in PartitionManager.list_partitions()

      assert :ok = PacketCleanupWorker.perform(%{"cleanup_days" => 7})

      # Old partition should be dropped
      refute name in PartitionManager.list_partitions()
    end

    test "does not drop recent partitions" do
      {:ok, _} = PartitionManager.ensure_partitions_exist()

      today_name = PartitionManager.partition_name(Date.utc_today())

      assert :ok = PacketCleanupWorker.perform(%{"cleanup_days" => 7})

      assert today_name in PartitionManager.list_partitions()
    end

    test "drops the partition exactly at the cleanup cutoff" do
      cutoff_date = Date.add(Date.utc_today(), -7)
      name = PartitionManager.partition_name(cutoff_date)
      {from_dt, to_dt} = PartitionManager.partition_range(cutoff_date)

      Repo.query!(
        "CREATE TABLE IF NOT EXISTS #{name} PARTITION OF packets FOR VALUES FROM ('#{DateTime.to_iso8601(from_dt)}') TO ('#{DateTime.to_iso8601(to_dt)}')"
      )

      assert :ok = PacketCleanupWorker.perform(%{"cleanup_days" => 7})
      refute name in PartitionManager.list_partitions()
    end
  end

  describe "perform/1 without cleanup_days" do
    test "drops old partitions and cleans up bad packets" do
      {:ok, _} = PartitionManager.ensure_partitions_exist()

      # Create an old bad packet
      retention_days = Application.get_env(:aprsme, :packet_retention_days, 7)

      old_time =
        DateTime.utc_now()
        |> DateTime.add(-(retention_days + 10) * 86_400, :second)
        |> DateTime.truncate(:microsecond)

      Repo.insert!(%BadPacket{raw_packet: "bad", error_message: "test", attempted_at: old_time})

      # Create a recent bad packet
      recent_time =
        DateTime.utc_now()
        |> DateTime.add(-3600, :second)
        |> DateTime.truncate(:microsecond)

      Repo.insert!(%BadPacket{raw_packet: "recent_bad", error_message: "test", attempted_at: recent_time})

      assert :ok = PacketCleanupWorker.perform(%{})

      # Only recent bad packet should remain
      assert Repo.aggregate(BadPacket, :count) == 1
    end
  end

  describe "perform/1 with negative cleanup_days" do
    test "falls through to default perform and succeeds" do
      {:ok, _} = PartitionManager.ensure_partitions_exist()

      # Negative days don't match the guard, so falls through to the catch-all
      assert :ok = PacketCleanupWorker.perform(%{"cleanup_days" => -1})
    end
  end
end
