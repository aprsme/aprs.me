defmodule Aprs.Workers.PacketCleanupWorkerTest do
  use Aprs.DataCase, async: true

  import Mox

  alias Aprs.PacketsMock
  alias Aprs.Workers.PacketCleanupWorker

  # Make sure mocks are verified when the test exits
  setup :verify_on_exit!

  describe "perform/1 with cleanup_days" do
    test "cleans up packets older than the specified number of days" do
      days = 30
      job = %Oban.Job{args: %{"cleanup_days" => days}}

      expect(PacketsMock, :clean_packets_older_than, fn ^days ->
        5
      end)

      assert :ok = PacketCleanupWorker.perform(job)
    end
  end

  describe "perform/1 without cleanup_days" do
    test "cleans up packets using the default retention period" do
      job = %Oban.Job{args: %{}}

      expect(PacketsMock, :clean_old_packets, fn ->
        10
      end)

      assert :ok = PacketCleanupWorker.perform(job)
    end
  end

  describe "cleanup_packets_older_than/1" do
    test "cleans up packets older than the given number of days" do
      days = 60
      deleted_count = 15

      expect(PacketsMock, :clean_packets_older_than, fn ^days ->
        deleted_count
      end)

      assert ^deleted_count = PacketCleanupWorker.cleanup_packets_older_than(days)
    end
  end
end
