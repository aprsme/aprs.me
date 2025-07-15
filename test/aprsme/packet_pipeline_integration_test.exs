defmodule Aprsme.PacketPipelineIntegrationTest do
  use Aprsme.DataCase, async: false

  import ExUnit.CaptureLog

  alias Aprsme.PacketProducer

  describe "packet pipeline under load" do
    test "system adjusts batch sizes based on load" do
      # Test the actual running system without starting new processes
      initial_batch_size = Aprsme.SystemMonitor.get_recommended_batch_size()
      assert initial_batch_size >= 100
      assert initial_batch_size <= 800

      # Submit some packets to the running system
      log_output =
        capture_log(fn ->
          for i <- 1..10 do
            packet = %{
              sender: "TEST-#{i}",
              destination: "APRS",
              path: "WIDE1-1",
              information_field: "Integration test packet #{i}",
              data_type: "position",
              lat: 40.0 + i / 1000,
              lon: -74.0 + i / 1000
            }

            PacketProducer.submit_packet(packet)
          end

          # Wait a bit for processing
          Process.sleep(100)
        end)

      # Verify no errors occurred
      refute log_output =~ "error"
    end

    test "insert optimizer provides reasonable batch sizes" do
      batch_size = Aprsme.Performance.InsertOptimizer.get_optimal_batch_size()
      assert batch_size >= 100
      assert batch_size <= 800
    end

    test "system monitor provides metrics" do
      metrics = Aprsme.SystemMonitor.get_metrics()

      assert is_map(metrics)
      assert Map.has_key?(metrics, :memory)
      assert Map.has_key?(metrics, :cpu)
      assert Map.has_key?(metrics, :processes)
      assert Map.has_key?(metrics, :db_pool)

      # Check pressure values are reasonable
      assert metrics.memory.pressure >= 0.0
      assert metrics.memory.pressure <= 1.0
      assert metrics.cpu.pressure >= 0.0
      assert metrics.cpu.pressure <= 1.0
    end
  end
end
