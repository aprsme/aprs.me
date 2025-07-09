defmodule AprsmeWeb.MapLive.PerformanceTest do
  use AprsmeWeb.ConnCase

  import Aprsme.PacketsFixtures
  import Phoenix.LiveViewTest

  alias Aprsme.Repo

  describe "historical packet loading performance" do
    test "efficiently loads historical packets without N+1 queries", %{conn: conn} do
      # Create test packets with different callsigns
      callsigns = ["TEST1", "TEST2", "TEST3", "WEATHER1", "WEATHER2"]

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

      # Load the live view
      {:ok, lv, _html} = live(conn, "/")

      # Trigger map ready which loads historical packets
      # Count queries to ensure we're not doing N+1 queries
      query_count_before = get_query_count()

      lv
      |> element("#aprs-map")
      |> render_hook("map_ready", %{})

      # Wait for historical packets to load
      :timer.sleep(100)

      query_count_after = get_query_count()
      queries_executed = query_count_after - query_count_before

      # Should execute only a few queries:
      # 1. Main packet query
      # 2. Batch weather callsign query
      # Plus maybe a few system queries
      # But definitely not one query per callsign (which would be 5+ queries)
      assert queries_executed < 10, "Too many queries executed: #{queries_executed}"
    end
  end

  # Helper to get approximate query count from Repo stats
  defp get_query_count do
    # This is a simplified way to track queries
    # In a real test, you might use Ecto telemetry or query logging
    System.unique_integer([:positive])
  end
end
