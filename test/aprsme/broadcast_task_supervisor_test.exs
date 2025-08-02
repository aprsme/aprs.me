defmodule Aprsme.BroadcastTaskSupervisorTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  alias Aprsme.BroadcastTaskSupervisor

  setup do
    # Ensure the supervisor is started (it should be from application)
    # Create a test topic for PubSub
    test_topic = "test_topic_#{System.unique_integer()}"
    Phoenix.PubSub.subscribe(Aprsme.PubSub, test_topic)

    {:ok, test_topic: test_topic}
  end

  describe "broadcast_async/3" do
    test "broadcasts messages to multiple topics asynchronously", %{test_topic: test_topic} do
      # Create additional test topics
      topic2 = "test_topic_2_#{System.unique_integer()}"
      topic3 = "test_topic_3_#{System.unique_integer()}"

      Phoenix.PubSub.subscribe(Aprsme.PubSub, topic2)
      Phoenix.PubSub.subscribe(Aprsme.PubSub, topic3)

      topics = [test_topic, topic2, topic3]
      message = {:test_message, "Hello from broadcast"}

      # Broadcast to all topics
      {:ok, task} = BroadcastTaskSupervisor.broadcast_async(topics, message)

      # Wait for the task to complete
      Task.await(task)

      # Should receive the message on all topics
      assert_receive {:test_message, "Hello from broadcast"}
      assert_receive {:test_message, "Hello from broadcast"}
      assert_receive {:test_message, "Hello from broadcast"}
    end

    test "handles large topic lists efficiently", %{test_topic: test_topic} do
      # Create 100 topics
      topics =
        for i <- 1..100 do
          topic = "bulk_test_#{i}_#{System.unique_integer()}"
          Phoenix.PubSub.subscribe(Aprsme.PubSub, topic)
          topic
        end

      # Add our test topic
      topics = [test_topic | topics]

      message = {:bulk_test, "Bulk broadcast"}

      # Measure time
      start_time = System.monotonic_time(:millisecond)
      {:ok, task} = BroadcastTaskSupervisor.broadcast_async(topics, message)
      Task.await(task, 5000)
      end_time = System.monotonic_time(:millisecond)

      # Should complete in a reasonable time (under 1 second for 101 topics)
      # Using a more lenient timeout to avoid failures on slower systems or CI
      assert end_time - start_time < 1000

      # Verify we received the message
      assert_receive {:bulk_test, "Bulk broadcast"}
    end
  end

  describe "broadcast_one_async/3" do
    test "broadcasts a single message asynchronously", %{test_topic: test_topic} do
      message = {:single_test, "Single broadcast"}

      {:ok, _pid} = BroadcastTaskSupervisor.broadcast_one_async(test_topic, message)

      # Give it a moment to process
      Process.sleep(10)

      assert_receive {:single_test, "Single broadcast"}
    end
  end

  describe "async_execute/1" do
    test "executes arbitrary functions asynchronously" do
      test_pid = self()

      BroadcastTaskSupervisor.async_execute(fn ->
        send(test_pid, :function_executed)
      end)

      assert_receive :function_executed, 1000
    end

    test "handles errors in async functions gracefully" do
      # Capture logs to suppress the expected error output
      capture_log(fn ->
        # This should not crash the supervisor
        {:ok, _pid} =
          BroadcastTaskSupervisor.async_execute(fn ->
            raise "Test error"
          end)

        # Give it time to fail - reduced from 50ms to 10ms
        Process.sleep(10)
      end)

      # Supervisor should still be running
      stats = BroadcastTaskSupervisor.get_stats()
      assert is_map(stats)
    end
  end

  describe "get_stats/0" do
    test "returns statistics about the broadcast pool" do
      stats = BroadcastTaskSupervisor.get_stats()

      assert is_map(stats)
      assert Map.has_key?(stats, :active_tasks)
      assert Map.has_key?(stats, :pool_size)
      assert Map.has_key?(stats, :scheduler_usage)

      assert is_integer(stats.active_tasks)
      assert stats.active_tasks >= 0

      assert is_integer(stats.pool_size)
      assert stats.pool_size > 0

      assert is_float(stats.scheduler_usage)
      assert stats.scheduler_usage >= 0.0
    end
  end

  describe "performance under load" do
    test "handles concurrent broadcasts efficiently" do
      # Create test topics
      topics =
        for i <- 1..10 do
          "perf_test_#{i}"
        end

      # Subscribe to only the first topic
      Phoenix.PubSub.subscribe(Aprsme.PubSub, List.first(topics))

      # Launch 100 concurrent broadcasts
      tasks =
        for i <- 1..100 do
          message = {:perf_test, i}
          {:ok, task} = BroadcastTaskSupervisor.broadcast_async(topics, message)
          task
        end

      # Wait for all tasks to complete
      Enum.each(tasks, &Task.await(&1, 5000))

      # Each topic should receive 100 messages
      message_count = receive_all_messages()

      # We subscribed to the first topic, so we should get 100 messages
      assert message_count == 100
    end
  end

  # Helper function to receive all messages
  defp receive_all_messages(count \\ 0) do
    receive do
      {:perf_test, _} -> receive_all_messages(count + 1)
    after
      100 -> count
    end
  end
end
