defmodule AprsmeWeb.MapLive.PacketProcessorTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.MapLive.PacketProcessor

  defp build_socket(assigns \\ %{}) do
    defaults = %{
      map_bounds: %{north: 34.0, south: 32.0, east: -95.0, west: -97.0},
      visible_packets: %{},
      map_zoom: 10,
      station_popup_open: false,
      locale: "en"
    }

    merged = Map.merge(defaults, assigns)

    socket = %Phoenix.LiveView.Socket{
      assigns: Map.put(merged, :__changed__, %{})
    }

    socket
  end

  defp build_packet(overrides \\ %{}) do
    Map.merge(
      %{
        id: "test-packet-1",
        sender: "K5GVL-10",
        base_callsign: "K5GVL",
        ssid: "10",
        lat: 33.1,
        lon: -96.5,
        symbol_table_id: "/",
        symbol_code: "-",
        comment: "Test station",
        received_at: DateTime.utc_now()
      },
      overrides
    )
  end

  describe "process_packet_for_marker_data/2" do
    test "returns marker data for in-bounds packet at marker zoom level" do
      socket = build_socket()
      packet = build_packet()

      {updated_socket, marker_data, _removed} = PacketProcessor.process_packet_for_marker_data(packet, socket)

      assert marker_data
      assert is_map(marker_data)
      # Socket state should be updated
      assert map_size(updated_socket.assigns.visible_packets) == 1
    end

    test "returns nil marker data for out-of-bounds packet" do
      socket = build_socket()
      # Packet outside the bounds (north: 34, south: 32, east: -95, west: -97)
      packet = build_packet(%{lat: 40.0, lon: -80.0})

      {updated_socket, marker_data, _removed} = PacketProcessor.process_packet_for_marker_data(packet, socket)

      assert marker_data == nil
      assert map_size(updated_socket.assigns.visible_packets) == 0
    end

    test "returns nil marker data for packet with nil coordinates" do
      socket = build_socket()
      packet = build_packet(%{lat: nil, lon: nil})

      {updated_socket, marker_data, _removed} = PacketProcessor.process_packet_for_marker_data(packet, socket)

      assert marker_data == nil
      assert map_size(updated_socket.assigns.visible_packets) == 0
    end

    test "returns nil marker data in heat map mode (zoom <= 8)" do
      socket = build_socket(%{map_zoom: 7})
      packet = build_packet()

      {_updated_socket, marker_data, _removed} = PacketProcessor.process_packet_for_marker_data(packet, socket)

      assert marker_data == nil
    end

    test "does not push any events to socket" do
      socket = build_socket()
      packet = build_packet()

      {updated_socket, _marker_data, _removed} = PacketProcessor.process_packet_for_marker_data(packet, socket)

      # Socket should not have any push_events queued
      # In LiveView, push_event adds to socket.private[:push_events]
      push_events = get_in(updated_socket.private, [:push_events]) || []
      assert push_events == []
    end

    test "returns convert_from when replacing an existing visible packet" do
      first_packet = build_packet(%{id: "packet-1", lat: 33.1, lon: -96.5})
      socket = build_socket(%{visible_packets: %{"K5GVL-10" => first_packet}})

      # Second packet for same callsign has a different packet ID but should still
      # replace the existing marker for that station.
      second_packet = build_packet(%{id: "packet-2", lat: 33.2, lon: -96.4})

      {_updated_socket, marker_data, _removed} = PacketProcessor.process_packet_for_marker_data(second_packet, socket)

      assert marker_data
      assert marker_data["convert_from"] == "K5GVL-10"
    end

    test "returns nil convert_from for first packet from a callsign" do
      socket = build_socket()
      packet = build_packet()

      {_updated_socket, marker_data, _removed} = PacketProcessor.process_packet_for_marker_data(packet, socket)

      assert marker_data
      refute Map.has_key?(marker_data, "convert_from")
    end

    test "returns removed callsign key when out-of-bounds packet had existing marker" do
      # Set up socket with an existing visible packet
      existing_packet = build_packet(%{id: "packet-1", lat: 33.1, lon: -96.5})
      socket = build_socket(%{visible_packets: %{"K5GVL-10" => existing_packet}})

      # New packet from same callsign is out of bounds
      out_of_bounds_packet = build_packet(%{id: "packet-2", lat: 40.0, lon: -80.0})

      {updated_socket, marker_data, removed_id} =
        PacketProcessor.process_packet_for_marker_data(out_of_bounds_packet, socket)

      assert marker_data == nil
      assert removed_id == "K5GVL-10"
      # The marker should be removed from visible_packets
      refute Map.has_key?(updated_socket.assigns.visible_packets, "K5GVL-10")
    end

    test "returns nil removed_id for normal in-bounds packets" do
      socket = build_socket()
      packet = build_packet()

      {_updated_socket, _marker_data, removed_id} =
        PacketProcessor.process_packet_for_marker_data(packet, socket)

      assert removed_id == nil
    end

    test "keeps one visible packet per station even when packet IDs change" do
      first_packet = build_packet(%{id: "packet-1", lat: 33.1, lon: -96.5})
      socket = build_socket(%{visible_packets: %{"K5GVL-10" => first_packet}})
      second_packet = build_packet(%{id: "packet-2", lat: 33.2, lon: -96.4})

      {updated_socket, marker_data, _removed_id} =
        PacketProcessor.process_packet_for_marker_data(second_packet, socket)

      assert marker_data["convert_from"] == "K5GVL-10"
      assert Map.keys(updated_socket.assigns.visible_packets) == ["K5GVL-10"]
      assert updated_socket.assigns.visible_packets["K5GVL-10"].id == "packet-2"
    end
  end
end
