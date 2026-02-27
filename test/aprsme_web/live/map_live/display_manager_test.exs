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
