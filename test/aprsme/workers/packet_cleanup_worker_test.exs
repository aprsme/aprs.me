defmodule Aprsme.Workers.PacketCleanupWorkerTest do
  use Aprsme.DataCase, async: true

  import Mox

  alias Aprsme.PacketsMock
  alias Aprsme.Workers.PacketCleanupWorker

  # Make sure mocks are verified when the test exits
  setup :verify_on_exit!

  describe "perform/1 with cleanup_days" do
    test "cleans up packets older than the specified number of days" do
      days = 30
      job = %Oban.Job{args: %{"cleanup_days" => days}}

      # Mock the Repo.all call for get_packet_ids_for_deletion
      expect(Aprsme.Repo, :all, fn _query ->
        # Return some packet IDs
        [1, 2, 3, 4, 5]
      end)

      # Mock the Repo.delete_all call
      expect(Aprsme.Repo, :delete_all, fn _query ->
        # Return deleted count
        {5, nil}
      end)

      assert :ok = PacketCleanupWorker.perform(job)
    end
  end

  describe "perform/1 without cleanup_days" do
    test "cleans up packets using the default retention period" do
      job = %Oban.Job{args: %{}}

      # Mock the Repo.all call for get_packet_ids_for_deletion
      expect(Aprsme.Repo, :all, fn _query ->
        # Return some packet IDs
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      end)

      # Mock the Repo.delete_all call
      expect(Aprsme.Repo, :delete_all, fn _query ->
        # Return deleted count
        {10, nil}
      end)

      assert :ok = PacketCleanupWorker.perform(job)
    end
  end

  describe "cleanup_packets_older_than/1" do
    test "cleans up packets older than the given number of days" do
      days = 60
      deleted_count = 15

      # Mock the Repo.all call for get_packet_ids_for_deletion
      expect(Aprsme.Repo, :all, fn _query ->
        # Return packet IDs
        Enum.to_list(1..deleted_count)
      end)

      # Mock the Repo.delete_all call
      expect(Aprsme.Repo, :delete_all, fn _query ->
        # Return deleted count
        {deleted_count, nil}
      end)

      assert ^deleted_count = PacketCleanupWorker.cleanup_packets_older_than(days)
    end
  end

  describe "cleanup_packets_older_than_batched/1" do
    test "returns correct tuple format with deleted count and batches" do
      days = 30

      # Mock the Repo.all call for get_packet_ids_for_deletion
      expect(Aprsme.Repo, :all, fn _query ->
        # Return some packet IDs
        [1, 2, 3, 4, 5]
      end)

      # Mock the Repo.delete_all call
      expect(Aprsme.Repo, :delete_all, fn _query ->
        # Return deleted count
        {5, nil}
      end)

      {deleted_count, batches} = PacketCleanupWorker.cleanup_packets_older_than_batched(days)

      assert is_integer(deleted_count)
      assert is_integer(batches)
      assert deleted_count >= 0
      assert batches >= 0
    end
  end

  describe "get_packet_ids_for_deletion/2" do
    test "returns list of packet IDs within batch size limit" do
      cutoff_time = DateTime.add(DateTime.utc_now(), -30, :day)
      batch_size = 100

      # Mock the Repo.all call
      expect(Aprsme.Repo, :all, fn _query ->
        # Return some mock packet IDs
        [1, 2, 3, 4, 5]
      end)

      result = PacketCleanupWorker.get_packet_ids_for_deletion(cutoff_time, batch_size)

      assert is_list(result)
      assert length(result) <= batch_size
    end
  end
end
