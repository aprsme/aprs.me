defmodule AprsmeWeb.Telemetry do
  @moduledoc false
  use Supervisor

  import Telemetry.Metrics

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children =
      if Application.get_env(:aprsme, AprsmeWeb.Telemetry)[:enabled] == false do
        [
          # Only telemetry poller in test mode
          {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
        ]

        # Telemetry poller will execute the given period measurements
        # every 10_000ms. Learn more here: https://hexdocs.pm/telemetry_metrics
      else
        [
          {:telemetry_poller, measurements: periodic_measurements(), period: 10_000},
          # Add reporters as children of your supervision tree.
          # {Telemetry.Metrics.ConsoleReporter, metrics: metrics()}
          {TelemetryMetricsPrometheus, [metrics: metrics()]}
        ]
      end

    Supervisor.init(children, strategy: :one_for_one)
  end

  def metrics do
    [
      # Phoenix Metrics - Use distribution for latency measurements
      distribution("phoenix.endpoint.start.system_time",
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("phoenix.endpoint.stop.duration",
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("phoenix.router_dispatch.start.system_time",
        tags: [:route],
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("phoenix.router_dispatch.exception.duration",
        tags: [:route],
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("phoenix.router_dispatch.stop.duration",
        tags: [:route],
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("phoenix.socket_connected.duration",
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("phoenix.channel_join.duration",
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("phoenix.channel_handled_in.duration",
        tags: [:event],
        unit: {:native, :millisecond},
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),

      # Database Metrics - Use distribution for query times
      distribution("aprsme.repo.query.total_time",
        unit: {:native, :millisecond},
        description: "The sum of the other measurements",
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("aprsme.repo.query.decode_time",
        unit: {:native, :millisecond},
        description: "The time spent decoding the data received from the database",
        buckets: [1, 5, 10, 25, 50, 100, 250, 500, 1000]
      ),
      distribution("aprsme.repo.query.query_time",
        unit: {:native, :millisecond},
        description: "The time spent executing the query",
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),
      distribution("aprsme.repo.query.queue_time",
        unit: {:native, :millisecond},
        description: "The time spent waiting for a database connection",
        buckets: [1, 5, 10, 25, 50, 100, 250, 500, 1000]
      ),
      distribution("aprsme.repo.query.idle_time",
        unit: {:native, :millisecond},
        description: "The time the connection spent waiting before being checked out for the query",
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),

      # VM Metrics - Use last_value for current state
      last_value("vm.memory.total", unit: {:byte, :kilobyte}),
      last_value("vm.total_run_queue_lengths.total"),
      last_value("vm.total_run_queue_lengths.cpu"),
      last_value("vm.total_run_queue_lengths.io"),

      # GenStage Packet Pipeline Metrics - Use counter and distribution
      counter("aprsme.packet_pipeline.batch.count", unit: :event, description: "Total number of packets processed"),
      counter("aprsme.packet_pipeline.batch.success",
        unit: :event,
        description: "Total number of successful inserts"
      ),
      counter("aprsme.packet_pipeline.batch.error", unit: :event, description: "Total number of errors"),
      distribution("aprsme.packet_pipeline.batch.duration_ms",
        unit: :millisecond,
        description: "Batch insert duration (ms)",
        buckets: [10, 50, 100, 250, 500, 1000, 2500, 5000, 10_000]
      ),

      # Note: SystemMonitor and InsertOptimizer metrics removed after reverting performance optimizations

      # Spatial PubSub Metrics
      last_value("aprsme.spatial_pubsub.clients.count", description: "Number of connected clients"),
      last_value("aprsme.spatial_pubsub.clients.grid_cells", description: "Number of active grid cells"),
      last_value("aprsme.spatial_pubsub.clients.avg_clients_per_cell", description: "Average clients per grid cell"),
      counter("aprsme.spatial_pubsub.broadcasts.total", description: "Total spatial broadcasts sent"),
      counter("aprsme.spatial_pubsub.broadcasts.filtered", description: "Broadcasts filtered by viewport"),
      counter("aprsme.spatial_pubsub.broadcasts.packets", description: "Total packets processed"),
      last_value("aprsme.spatial_pubsub.efficiency.ratio", unit: :percent, description: "Broadcast efficiency ratio"),
      counter("aprsme.spatial_pubsub.efficiency.saved_broadcasts",
        description: "Number of broadcasts saved by filtering"
      )
    ]
  end

  defp periodic_measurements do
    [
      # A module, function and arguments to be invoked periodically.
      # This function must call :telemetry.execute/3 and a metric must be added above.
      # {AprsmeWeb, :count_users, []}
    ]
  end
end
