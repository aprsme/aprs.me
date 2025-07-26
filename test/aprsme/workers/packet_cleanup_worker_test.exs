defmodule Aprsme.Workers.PacketCleanupWorkerTest do
  use Aprsme.DataCase, async: true

  alias Aprsme.Packet
  alias Aprsme.Workers.PacketCleanupWorker

  defp insert_packet(attrs) do
    attrs =
      Map.merge(
        %{
          sender: "TEST-#{System.unique_integer([:positive])}",
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          has_position: false
        },
        attrs
      )

    {:ok, packet} = %Packet{} |> Map.merge(attrs) |> Repo.insert()
    packet
  end

  defp insert_packets(count, attrs) do
    for _ <- 1..count, do: insert_packet(attrs)
  end

  describe "perform/1 with cleanup_days" do
    test "cleans up packets older than the specified number of days" do
      days = 30
      job_args = %{"cleanup_days" => days}

      # Create old packets that should be deleted
      old_time = DateTime.utc_now() |> DateTime.add(-(days + 1) * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(5, %{received_at: old_time})

      # Create recent packets that should NOT be deleted
      recent_time = DateTime.utc_now() |> DateTime.add(-5 * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(3, %{received_at: recent_time})

      assert :ok = PacketCleanupWorker.perform(job_args)

      # Verify only recent packets remain
      assert Repo.aggregate(Packet, :count, :id) == 3
    end
  end

  describe "perform/1 without cleanup_days" do
    test "cleans up packets using the default retention period" do
      job_args = %{}

      # Create very old packets that should be deleted (older than 365 days)
      old_time = DateTime.utc_now() |> DateTime.add(-400 * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(10, %{received_at: old_time})

      # Create recent packets that should NOT be deleted
      recent_time = DateTime.utc_now() |> DateTime.add(-5 * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(3, %{received_at: recent_time})

      assert :ok = PacketCleanupWorker.perform(job_args)

      # Verify only recent packets remain
      assert Repo.aggregate(Packet, :count, :id) == 3
    end
  end

  describe "cleanup_packets_older_than/1" do
    test "cleans up packets older than the given number of days" do
      days = 60

      # Create old packets that should be deleted
      old_time = DateTime.utc_now() |> DateTime.add(-(days + 10) * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(15, %{received_at: old_time})

      # Create recent packets that should NOT be deleted
      recent_time = DateTime.utc_now() |> DateTime.add(-30 * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(5, %{received_at: recent_time})

      deleted_count = PacketCleanupWorker.cleanup_packets_older_than(days)

      assert deleted_count == 15
      assert Repo.aggregate(Packet, :count, :id) == 5
    end
  end

  describe "cleanup_packets_older_than_batched/1" do
    test "returns correct tuple format with deleted count and batches" do
      days = 30

      # Create old packets that should be deleted
      old_time = DateTime.utc_now() |> DateTime.add(-(days + 1) * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(5, %{received_at: old_time})

      # Create recent packets that should NOT be deleted
      recent_time = DateTime.utc_now() |> DateTime.add(-5 * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(3, %{received_at: recent_time})

      {deleted_count, batches} = PacketCleanupWorker.cleanup_packets_older_than_batched(days)

      assert is_integer(deleted_count)
      assert is_integer(batches)
      assert deleted_count == 5
      assert batches >= 1
      assert Repo.aggregate(Packet, :count, :id) == 3
    end
  end

  describe "get_packet_ids_for_deletion/2" do
    test "returns list of packet IDs within batch size limit" do
      cutoff_time = DateTime.utc_now() |> DateTime.add(-30, :day) |> DateTime.truncate(:second)
      batch_size = 10

      # Create old packets that should be included
      old_time = DateTime.utc_now() |> DateTime.add(-40 * 86_400, :second) |> DateTime.truncate(:second)
      old_packets = insert_packets(15, %{received_at: old_time})

      # Create recent packets that should NOT be included
      recent_time = DateTime.utc_now() |> DateTime.add(-5 * 86_400, :second) |> DateTime.truncate(:second)
      insert_packets(5, %{received_at: recent_time})

      result = PacketCleanupWorker.get_packet_ids_for_deletion(cutoff_time, batch_size)

      assert is_list(result)
      assert length(result) == batch_size

      # Verify that returned IDs are from old packets
      old_packet_ids = Enum.map(old_packets, & &1.id)
      assert Enum.all?(result, &(&1 in old_packet_ids))
    end
  end
end
