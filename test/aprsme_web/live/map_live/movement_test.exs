defmodule AprsmeWeb.MapLive.MovementTest do
  use AprsmeWeb.ConnCase, async: false

  import Mox
  import Phoenix.LiveViewTest

  alias Aprsme.GeoUtils

  setup :verify_on_exit!

  describe "GPS drift filtering" do
    setup do
      # Mock the Packets module to return empty results for historical queries
      stub(Aprsme.PacketsMock, :get_recent_packets_optimized, fn _args -> [] end)
      :ok
    end

    test "does not update marker for GPS drift", %{conn: conn} do
      # Start with initial location
      {:ok, view, _html} = live(conn, "/")

      # First update the map state to set zoom level
      assert render_hook(view, "update_map_state", %{
               "center" => %{"lat" => 33.16961, "lng" => -96.4921},
               "zoom" => 9
             })

      # Then update bounds
      bounds_params = %{
        "bounds" => %{
          "north" => 33.18,
          "south" => 33.16,
          "east" => -96.48,
          "west" => -96.50
        }
      }

      assert render_hook(view, "bounds_changed", bounds_params)
      :timer.sleep(100)

      # Simulate initial packet
      initial_packet = %{
        "id" => "TEST-1",
        "sender" => "TEST-1",
        "base_callsign" => "TEST",
        "lat" => 33.16961,
        "lon" => -96.4921,
        "has_position" => true,
        "received_at" => DateTime.utc_now()
      }

      send(view.pid, {:postgres_packet, initial_packet})

      # Wait for the initial packet to be processed
      :timer.sleep(100)

      # Simulate GPS drift (5 meters movement)
      drift_packet = %{
        "id" => "TEST-1",
        "sender" => "TEST-1",
        "base_callsign" => "TEST",
        # About 5 meters north
        "lat" => 33.169655,
        "lon" => -96.4921,
        "has_position" => true,
        "received_at" => DateTime.utc_now()
      }

      # Send the drift packet
      send(view.pid, {:postgres_packet, drift_packet})

      # The view should not push a new_packet event for GPS drift
      refute_push_event(view, "new_packet", %{id: "TEST-1"}, 200)
    end

    test "updates marker for significant movement", %{conn: conn} do
      # Start with initial location at a zoom level that shows individual markers
      {:ok, view, _html} = live(conn, "/?z=15")

      # Notify the map is ready
      assert render_hook(view, "map_ready", %{})

      # Update the map state to zoom level 15 (individual markers)
      assert render_hook(view, "update_map_state", %{
               "center" => %{"lat" => 33.16961, "lng" => -96.4921},
               "zoom" => 15
             })

      # Set bounds to ensure packets are visible
      bounds_params = %{
        "bounds" => %{
          "north" => 33.18,
          "south" => 33.16,
          "east" => -96.48,
          "west" => -96.50
        }
      }
      assert render_hook(view, "bounds_changed", bounds_params)

      # Wait for initial load to complete
      :timer.sleep(500)

      # Clear any events from initial load
      flush_push_events(view)

      # Simulate initial packet
      initial_packet = %{
        "id" => "TEST-2",
        "sender" => "TEST-2",
        "base_callsign" => "TEST",
        "lat" => 33.16961,
        "lon" => -96.4921,
        "has_position" => true,
        "received_at" => DateTime.utc_now()
      }

      send(view.pid, {:postgres_packet, initial_packet})

      # Wait for the initial packet to be processed
      :timer.sleep(100)
      
      # Should receive new_packet for the initial packet
      assert_push_event(view, "new_packet", %{}, 1000)

      # Simulate significant movement (20+ meters)
      moved_packet = %{
        "id" => "TEST-2",
        "sender" => "TEST-2",
        "base_callsign" => "TEST",
        # About 20 meters north
        "lat" => 33.1698,
        "lon" => -96.4921,
        "has_position" => true,
        "received_at" => DateTime.utc_now()
      }

      # Send the moved packet
      send(view.pid, {:postgres_packet, moved_packet})
      
      # Wait a bit for processing
      :timer.sleep(100)

      # The view should push a new_packet event for significant movement
      assert_push_event(view, "new_packet", %{}, 1000)
    end
    
    defp flush_push_events(view) do
      receive do
        {ref, {:push_event, _, _}} when is_reference(ref) and ref == view.ref ->
          flush_push_events(view)
      after
        0 -> :ok
      end
    end
  end

  describe "distance calculations" do
    test "calculates correct distances for GPS drift scenarios" do
      # Example from the URL provided
      lat1 = 33.16961
      lon1 = -96.4921

      # Tiny drift - should be filtered
      lat2 = 33.169615
      lon2 = -96.492095
      distance = GeoUtils.haversine_distance(lat1, lon1, lat2, lon2)
      # Less than 10 meters
      assert distance < 10

      # Slightly larger drift
      lat3 = 33.16965
      lon3 = -96.4921
      distance2 = GeoUtils.haversine_distance(lat1, lon1, lat3, lon3)
      # Still GPS drift range
      assert distance2 < 10

      # Actual movement
      lat4 = 33.17000
      lon4 = -96.4921
      distance3 = GeoUtils.haversine_distance(lat1, lon1, lat4, lon4)
      # Significant movement
      assert distance3 > 20
    end
  end
  
end
