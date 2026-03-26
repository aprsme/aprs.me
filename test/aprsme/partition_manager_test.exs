defmodule Aprsme.PartitionManagerTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.PartitionManager

  describe "partition_name/1" do
    test "generates correct name for a date" do
      assert PartitionManager.partition_name(~D[2026-02-20]) == "packets_20260220"
      assert PartitionManager.partition_name(~D[2026-01-01]) == "packets_20260101"
      assert PartitionManager.partition_name(~D[2025-12-31]) == "packets_20251231"
    end
  end

  describe "partition_range/1" do
    test "returns start and end timestamps for a date" do
      {start_dt, end_dt} = PartitionManager.partition_range(~D[2026-02-20])
      assert start_dt == ~U[2026-02-20 00:00:00Z]
      assert end_dt == ~U[2026-02-21 00:00:00Z]
    end

    test "handles month boundaries" do
      {_start_dt, end_dt} = PartitionManager.partition_range(~D[2026-01-31])
      assert end_dt == ~U[2026-02-01 00:00:00Z]
    end

    test "handles year boundaries" do
      {_start_dt, end_dt} = PartitionManager.partition_range(~D[2025-12-31])
      assert end_dt == ~U[2026-01-01 00:00:00Z]
    end
  end

  describe "ensure_partitions_exist/0" do
    test "creates partitions for today and next 2 days" do
      today = Date.utc_today()

      {:ok, created} = PartitionManager.ensure_partitions_exist()

      assert is_list(created)

      # Verify partitions exist in the database
      for offset <- 0..2 do
        date = Date.add(today, offset)
        name = PartitionManager.partition_name(date)

        assert {:ok, %{num_rows: 1}} =
                 Repo.query(
                   "SELECT 1 FROM pg_tables WHERE tablename = $1 AND schemaname = 'public'",
                   [name]
                 )
      end
    end

    test "is idempotent - calling twice doesn't error" do
      {:ok, _created1} = PartitionManager.ensure_partitions_exist()
      {:ok, _created2} = PartitionManager.ensure_partitions_exist()
    end
  end

  describe "drop_old_partitions/1" do
    test "drops partitions older than retention days" do
      # Create a partition for 30 days ago
      old_date = Date.add(Date.utc_today(), -30)
      name = PartitionManager.partition_name(old_date)
      {from_dt, to_dt} = PartitionManager.partition_range(old_date)

      Repo.query!(
        "CREATE TABLE IF NOT EXISTS #{name} PARTITION OF packets FOR VALUES FROM ('#{DateTime.to_iso8601(from_dt)}') TO ('#{DateTime.to_iso8601(to_dt)}')"
      )

      # Verify it exists
      assert {:ok, %{num_rows: 1}} =
               Repo.query(
                 "SELECT 1 FROM pg_tables WHERE tablename = $1 AND schemaname = 'public'",
                 [name]
               )

      # Drop partitions older than 7 days
      {:ok, dropped} = PartitionManager.drop_old_partitions(7)

      assert name in dropped

      # Verify it's gone
      assert {:ok, %{num_rows: 0}} =
               Repo.query(
                 "SELECT 1 FROM pg_tables WHERE tablename = $1 AND schemaname = 'public'",
                 [name]
               )
    end

    test "does not drop recent partitions" do
      # Ensure today's partition exists
      {:ok, _} = PartitionManager.ensure_partitions_exist()

      today_name = PartitionManager.partition_name(Date.utc_today())

      {:ok, dropped} = PartitionManager.drop_old_partitions(7)

      refute today_name in dropped

      # Verify today's partition still exists
      assert {:ok, %{num_rows: 1}} =
               Repo.query(
                 "SELECT 1 FROM pg_tables WHERE tablename = $1 AND schemaname = 'public'",
                 [today_name]
               )
    end

    test "drops the partition at the retention cutoff boundary" do
      cutoff_date = Date.add(Date.utc_today(), -7)
      name = PartitionManager.partition_name(cutoff_date)
      {from_dt, to_dt} = PartitionManager.partition_range(cutoff_date)

      Repo.query!(
        "CREATE TABLE IF NOT EXISTS #{name} PARTITION OF packets FOR VALUES FROM ('#{DateTime.to_iso8601(from_dt)}') TO ('#{DateTime.to_iso8601(to_dt)}')"
      )

      {:ok, dropped} = PartitionManager.drop_old_partitions(7)

      assert name in dropped
    end
  end

  describe "list_partitions/0" do
    test "returns list of existing partition names" do
      {:ok, _} = PartitionManager.ensure_partitions_exist()

      partitions = PartitionManager.list_partitions()

      assert is_list(partitions)
      assert length(partitions) >= 3

      today_name = PartitionManager.partition_name(Date.utc_today())
      assert today_name in partitions
    end
  end
end
