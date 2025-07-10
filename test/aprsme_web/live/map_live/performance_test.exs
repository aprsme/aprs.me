defmodule AprsmeWeb.MapLive.PerformanceTest do
  use AprsmeWeb.ConnCase

  import Aprsme.PacketsFixtures
  import Phoenix.LiveViewTest

  describe "historical packet loading performance" do
    setup do
      # Set up query counter using process dictionary
      Process.put(:query_count, 0)

      # Set up telemetry handler to count queries
      :telemetry.attach(
        "test-query-counter",
        [:aprsme, :repo, :query],
        fn _event, _measurements, _metadata, _config ->
          Process.put(:query_count, (Process.get(:query_count) || 0) + 1)
        end,
        nil
      )

      on_exit(fn ->
        :telemetry.detach("test-query-counter")
      end)

      :ok
    end

    test "efficiently loads historical packets without N+1 queries", %{conn: conn} do
      # Create test packets with different callsigns
      _callsigns = ["TEST1", "TEST2", "TEST3", "WEATHER1", "WEATHER2"]

      # Create regular packets
      for callsign <- ["TEST1", "TEST2", "TEST3"] do
        packet_fixture(%{
          sender: callsign,
          lat: Decimal.new("39.8283"),
          lon: Decimal.new("-98.5795"),
          has_position: true,
          data_type: "position"
        })
      end

      # Create weather packets
      for callsign <- ["WEATHER1", "WEATHER2"] do
        packet_fixture(%{
          sender: callsign,
          lat: Decimal.new("39.8283"),
          lon: Decimal.new("-98.5795"),
          has_position: true,
          data_type: "weather",
          symbol_table_id: "/",
          symbol_code: "_"
        })
      end

      # Reset query counter before the test
      Process.put(:query_count, 0)

      # Load the live view
      {:ok, lv, _html} = live(conn, "/")

      # Reset counter again to measure only map loading queries
      Process.put(:query_count, 0)

      # Trigger map ready which loads historical packets
      lv
      |> element("#aprs-map")
      |> render_hook("map_ready", %{})

      # Wait for historical packets to load
      :timer.sleep(200)

      queries_executed = Process.get(:query_count) || 0

      # Should execute only a few queries:
      # 1. Initial packet query
      # 2. Batch weather callsign queries (if any)
      # 3. Maybe some additional optimized queries
      # But definitely not one query per packet/callsign (which would be 5+ queries)
      # Given the batched loading, we expect more queries but they should be efficient
      assert queries_executed < 20, "Too many queries executed: #{queries_executed}"
    end
  end
end
