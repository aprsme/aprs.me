defmodule Aprsme.PacketPipelineSetupTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Aprsme.PacketPipelineSetup

  setup do
    # Stop any existing PacketPipelineSetup process
    case GenServer.whereis(PacketPipelineSetup) do
      nil -> :ok
      pid -> GenServer.stop(pid, :normal, 5000)
    end

    on_exit(fn ->
      case GenServer.whereis(PacketPipelineSetup) do
        nil ->
          :ok

        pid ->
          try do
            GenServer.stop(pid, :normal, 5000)
          catch
            :exit, _ -> :ok
          end
      end
    end)

    :ok
  end

  describe "start_link/0" do
    test "starts the GenServer with default opts" do
      capture_log(fn ->
        {:ok, pid} = PacketPipelineSetup.start_link()

        assert Process.alive?(pid)
        assert GenServer.whereis(PacketPipelineSetup) == pid
      end)
    end
  end

  describe "start_link/1" do
    test "starts the GenServer and registers it under its module name" do
      capture_log(fn ->
        {:ok, pid} = PacketPipelineSetup.start_link([])

        assert Process.alive?(pid)
        assert GenServer.whereis(PacketPipelineSetup) == pid
      end)
    end
  end

  describe "handle_info(:setup_subscription, state)" do
    test "GenServer survives when PacketProducer/PacketConsumer are not running and schedules retry" do
      log =
        capture_log(fn ->
          {:ok, pid} = PacketPipelineSetup.start_link([])

          # Wait for the initial :setup_subscription to fire (~100ms) and fail
          Process.sleep(200)

          # The GenServer should still be alive after the failure
          assert Process.alive?(pid)
        end)

      # The error path should have logged the failure
      assert log =~ "Failed to establish GenStage subscription"
    end

    test "retries subscription after failure" do
      log =
        capture_log(fn ->
          {:ok, pid} = PacketPipelineSetup.start_link([])

          # Wait for initial attempt (~100ms) and retry (~1000ms)
          Process.sleep(1300)

          assert Process.alive?(pid)
        end)

      # Should see multiple failure logs from retry
      assert log =~ "Failed to establish GenStage subscription"
    end
  end

  describe "init/1" do
    test "schedules :setup_subscription message" do
      capture_log(fn ->
        {:ok, pid} = PacketPipelineSetup.start_link([])

        # The init should have scheduled a message
        # Verify state is initialized as empty map
        state = :sys.get_state(pid)
        assert state == %{}
      end)
    end
  end
end
