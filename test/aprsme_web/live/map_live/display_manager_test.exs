defmodule AprsmeWeb.MapLive.DisplayManagerTest do
  use Aprsme.DataCase, async: true

  alias AprsmeWeb.MapLive.DisplayManager

  setup do
    # Create a basic socket mock for testing
    socket = %Phoenix.LiveView.Socket{
      assigns: %{
        visible_packets: %{},
        tracked_callsign: nil,
        packet_age_threshold: nil,
        map_bounds: nil,
        map_zoom: 10,
        historical_packets: %{}
      },
      private: %{live_temp: %{}}
    }

    {:ok, socket: socket}
  end

  # Helper function to assert push events
  defp assert_push_event(socket, event_name, expected_data) do
    push_events = get_in(socket.private, [:live_temp, :push_events]) || []

    matching_event = Enum.find(push_events, fn [name, _data] -> name == event_name end)

    assert matching_event != nil, "Expected to find event '#{event_name}' in push_events"

    [_name, data] = matching_event

    # For partial matching, check if all expected keys are present
    Enum.each(expected_data, fn {key, value} ->
      assert Map.get(data, key) == value,
             "Expected #{key} to be #{inspect(value)}, got #{inspect(Map.get(data, key))}"
    end)

    socket
  end

  describe "handle_zoom_threshold_crossing/2" do
    test "shows trail line for tracked callsign at low zoom", %{socket: socket} do
      packet1 = %{
        id: "1",
        sender: "W5ISP-9",
        lat: 30.123,
        lon: -97.456,
        received_at: ~U[2024-01-01 10:00:00Z]
      }

      visible_packets = %{"W5ISP-9:1" => packet1}

      socket =
        Map.put(socket, :assigns, %{
          socket.assigns
          | visible_packets: visible_packets,
            tracked_callsign: "W5ISP-9",
            packet_age_threshold: ~U[2024-01-01 09:00:00Z],
            map_zoom: 8
        })

      # Zoom from 9 to 8 (crossing threshold) - socket already has new zoom
      result_socket = DisplayManager.handle_zoom_threshold_crossing(socket, 8)

      # Should clear markers and show trail line
      assert_push_event(result_socket, "clear_all_markers", %{})
      assert_push_event(result_socket, "show_trail_line", %{callsign: "W5ISP-9"})
    end

    test "shows heat map when no tracked callsign at low zoom", %{socket: socket} do
      socket =
        Map.put(socket, :assigns, %{
          socket.assigns
          | visible_packets: %{},
            tracked_callsign: "",
            historical_packets: %{},
            map_bounds: %{north: 90, south: -90, east: 180, west: -180},
            map_zoom: 8
        })

      # Zoom from 9 to 8 (crossing threshold) - socket already has new zoom
      result_socket = DisplayManager.handle_zoom_threshold_crossing(socket, 8)

      # Should clear markers and show heat map
      assert_push_event(result_socket, "clear_all_markers", %{})
      assert_push_event(result_socket, "show_heat_map", %{heat_points: []})
    end

    test "shows markers for tracked callsign at high zoom", %{socket: socket} do
      packet1 = %{
        id: "1",
        sender: "W5ISP-9",
        lat: 30.123,
        lon: -97.456,
        received_at: ~U[2024-01-01 10:00:00Z]
      }

      visible_packets = %{"W5ISP-9:1" => packet1}

      socket =
        Map.put(socket, :assigns, %{
          socket.assigns
          | visible_packets: visible_packets,
            tracked_callsign: "W5ISP-9",
            packet_age_threshold: ~U[2024-01-01 09:00:00Z],
            map_zoom: 9
        })

      # Zoom from 8 to 9 (crossing threshold) - socket already has new zoom
      result_socket = DisplayManager.handle_zoom_threshold_crossing(socket, 9)

      # Should show markers
      assert_push_event(result_socket, "show_markers", %{})
    end
  end

  describe "send_trail_line_for_tracked_callsign/1" do
    test "pushes trail line event with sorted points", %{socket: socket} do
      packet1 = %{
        id: "1",
        sender: "W5ISP-9",
        lat: 30.123,
        lon: -97.456,
        received_at: ~U[2024-01-01 10:00:00Z]
      }

      packet2 = %{
        id: "2",
        sender: "W5ISP-9",
        lat: 30.124,
        lon: -97.457,
        received_at: ~U[2024-01-01 10:05:00Z]
      }

      packet3 = %{
        id: "3",
        sender: "W5ISP-9",
        lat: 30.125,
        lon: -97.458,
        received_at: ~U[2024-01-01 10:10:00Z]
      }

      visible_packets = %{"W5ISP-9:1" => packet1, "W5ISP-9:2" => packet2, "W5ISP-9:3" => packet3}

      socket =
        Map.put(socket, :assigns, %{
          socket.assigns
          | visible_packets: visible_packets,
            tracked_callsign: "W5ISP-9",
            packet_age_threshold: ~U[2024-01-01 09:00:00Z]
        })

      result_socket = DisplayManager.send_trail_line_for_tracked_callsign(socket)

      push_events = get_in(result_socket.private, [:live_temp, :push_events]) || []
      assert length(push_events) == 1

      [[event_name, event_data]] = push_events
      assert event_name == "show_trail_line"

      assert event_data == %{
               callsign: "W5ISP-9",
               points: [
                 %{lat: 30.123, lng: -97.456, timestamp: "2024-01-01T10:00:00Z"},
                 %{lat: 30.124, lng: -97.457, timestamp: "2024-01-01T10:05:00Z"},
                 %{lat: 30.125, lng: -97.458, timestamp: "2024-01-01T10:10:00Z"}
               ]
             }
    end

    test "filters by packet age threshold", %{socket: socket} do
      packet1 = %{
        id: "1",
        sender: "W5ISP-9",
        lat: 30.123,
        lon: -97.456,
        received_at: ~U[2024-01-01 09:00:00Z]
      }

      # Too old
      packet2 = %{
        id: "2",
        sender: "W5ISP-9",
        lat: 30.124,
        lon: -97.457,
        received_at: ~U[2024-01-01 10:05:00Z]
      }

      # Within threshold

      visible_packets = %{"W5ISP-9:1" => packet1, "W5ISP-9:2" => packet2}

      socket =
        Map.put(socket, :assigns, %{
          socket.assigns
          | visible_packets: visible_packets,
            tracked_callsign: "W5ISP-9",
            packet_age_threshold: ~U[2024-01-01 10:00:00Z]
        })

      result_socket = DisplayManager.send_trail_line_for_tracked_callsign(socket)

      push_events = get_in(result_socket.private, [:live_temp, :push_events]) || []
      assert length(push_events) == 1

      [[event_name, event_data]] = push_events
      assert event_name == "show_trail_line"

      assert event_data == %{
               callsign: "W5ISP-9",
               points: [%{lat: 30.124, lng: -97.457, timestamp: "2024-01-01T10:05:00Z"}]
             }
    end

    test "returns socket unchanged when no packets", %{socket: socket} do
      socket =
        Map.put(socket, :assigns, %{
          socket.assigns
          | visible_packets: %{},
            tracked_callsign: "W5ISP-9",
            packet_age_threshold: ~U[2024-01-01 10:00:00Z]
        })

      result_socket = DisplayManager.send_trail_line_for_tracked_callsign(socket)

      push_events = get_in(result_socket.private, [:live_temp, :push_events]) || []
      assert push_events == []
    end
  end
end
