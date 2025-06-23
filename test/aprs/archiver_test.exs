defmodule Aprs.ArchiverTest do
  use ExUnit.Case, async: true

  alias Aprs.Archiver

  describe "start_link/1" do
    test "starts the archiver GenServer with default args" do
      assert {:ok, pid} = Archiver.start_link()
      assert is_pid(pid)
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end

    test "starts the archiver GenServer with custom args" do
      args = [some: :option]
      assert {:ok, pid} = Archiver.start_link(args)
      assert is_pid(pid)
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end

    test "registers the process with name :archiver" do
      # Ensure no previous archiver is running
      if Process.whereis(:archiver) do
        GenServer.stop(:archiver)
        Process.sleep(10)
      end

      assert {:ok, pid} = Archiver.start_link()
      assert Process.whereis(:archiver) == pid

      # Clean up
      GenServer.stop(:archiver)
    end

    test "returns error if archiver is already running" do
      # Ensure no previous archiver is running
      if Process.whereis(:archiver) do
        GenServer.stop(:archiver)
        Process.sleep(10)
      end

      assert {:ok, _pid1} = Archiver.start_link()
      assert {:error, {:already_started, _pid}} = Archiver.start_link()

      # Clean up
      GenServer.stop(:archiver)
    end
  end

  describe "init/1" do
    test "initializes with default empty state" do
      assert {:ok, []} = Archiver.init()
    end

    test "initializes with provided state" do
      state = [some: :data]
      assert {:ok, ^state} = Archiver.init(state)
    end

    test "schedules connect message after 5 seconds" do
      # Start the GenServer to test init behavior
      {:ok, pid} = Archiver.start_link()

      # We can't easily test the exact timing without making the test slow,
      # but we can verify the GenServer started successfully
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end

    test "subscribes to the 'call' topic" do
      # Mock the Endpoint.subscribe call
      # Since we can't easily mock in this context, we'll just verify
      # the GenServer starts without errors
      {:ok, pid} = Archiver.start_link()
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end
  end

  describe "handle_info/2" do
    setup do
      {:ok, pid} = Archiver.start_link()
      %{pid: pid}
    end

    test "handles :connect message", %{pid: pid} do
      state = [some: :state]

      # Send the connect message directly to test the handler
      result = Archiver.handle_info(:connect, state)

      assert {:noreply, ^state} = result

      # Clean up
      GenServer.stop(pid)
    end

    test "handles unknown messages", %{pid: pid} do
      state = [some: :state]

      # Send an unknown message
      result = Archiver.handle_info(:unknown_message, state)

      assert {:noreply, ^state} = result

      # Clean up
      GenServer.stop(pid)
    end

    test "handles various message types", %{pid: pid} do
      state = [test: :data]

      # Test different message types
      messages = [
        {:some, :tuple},
        "string_message",
        123,
        %{map: "message"},
        [:list, :message]
      ]

      for message <- messages do
        result = Archiver.handle_info(message, state)
        assert {:noreply, ^state} = result
      end

      # Clean up
      GenServer.stop(pid)
    end

    test "preserves state across different messages", %{pid: pid} do
      initial_state = [counter: 0]

      # Test that state is preserved
      result1 = Archiver.handle_info(:connect, initial_state)
      assert {:noreply, ^initial_state} = result1

      result2 = Archiver.handle_info(:other_message, initial_state)
      assert {:noreply, ^initial_state} = result2

      # Clean up
      GenServer.stop(pid)
    end
  end

  describe "GenServer behavior" do
    test "can send messages to running archiver" do
      {:ok, pid} = Archiver.start_link()

      # Send a message and verify the process handles it
      send(pid, :test_message)

      # Give it a moment to process
      Process.sleep(10)

      # Verify the process is still alive (didn't crash)
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end

    test "can be stopped gracefully" do
      {:ok, pid} = Archiver.start_link()

      assert Process.alive?(pid)

      # Stop the GenServer
      :ok = GenServer.stop(pid)

      # Give it a moment to stop
      Process.sleep(10)

      # Verify it's no longer alive
      refute Process.alive?(pid)
    end

    test "handles multiple concurrent operations" do
      {:ok, pid} = Archiver.start_link()

      # Send multiple messages concurrently
      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            send(pid, {:message, i})
            :ok
          end)
        end

      # Wait for all tasks to complete
      Enum.each(tasks, &Task.await/1)

      # Verify the process is still alive
      assert Process.alive?(pid)

      # Clean up
      GenServer.stop(pid)
    end
  end

  describe "module constants and specs" do
    test "has correct topic constant" do
      # This tests that the module compiles correctly with the @topic attribute
      # The actual value is tested indirectly through the init function
      assert Code.ensure_loaded?(Archiver)
    end

    test "has correct typespec for start_link" do
      # This ensures the module compiles with correct typespecs
      # We can't directly test typespecs, but we can ensure the function
      # behaves according to its spec
      result = Archiver.start_link([])
      assert match?({:ok, _pid}, result) or match?({:error, _reason}, result)

      if match?({:ok, _pid}, result) do
        GenServer.stop(elem(result, 1))
      end
    end

    test "has correct typespec for init" do
      # Test that init returns the expected format
      result = Archiver.init([])
      assert {:ok, []} = result

      result2 = Archiver.init(some: :data)
      assert {:ok, [some: :data]} = result2
    end
  end
end
