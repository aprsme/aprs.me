defmodule AprsmeWeb.MapLive.DataBuilderTest do
  use Aprsme.DataCase, async: true

  alias AprsmeWeb.MapLive.DataBuilder

  describe "display name for APRS objects and items" do
    test "uses object_name when packet is an object" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "DB0SDA",
        base_callsign: "DB0SDA",
        ssid: "",
        object_name: "P-K5SGD",
        is_object: true,
        is_item: false,
        lat: 33.169333,
        lon: -96.492167,
        data_type: "object",
        received_at: DateTime.utc_now(),
        symbol_table_id: "P",
        symbol_code: "#",
        comment: "DAPNET POCSAG Transmitter",
        has_position: true,
        path: "TCPIP*,qAC,FIFTH"
      }

      result = DataBuilder.build_packet_data(packet, true)

      assert result
      assert result["callsign"] == "P-K5SGD"
    end

    test "uses item_name when packet is an item" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "N0CALL",
        base_callsign: "N0CALL",
        ssid: "",
        item_name: "REPEATER1",
        is_item: true,
        is_object: false,
        lat: 40.0,
        lon: -105.0,
        data_type: "item",
        received_at: DateTime.utc_now(),
        symbol_table_id: "/",
        symbol_code: "r",
        comment: "Some repeater",
        has_position: true,
        path: "WIDE1-1"
      }

      result = DataBuilder.build_packet_data(packet, true)

      assert result
      assert result["callsign"] == "REPEATER1"
    end

    test "falls back to sender when no object_name or item_name" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "K5SGD-Y",
        base_callsign: "K5SGD",
        ssid: "Y",
        object_name: nil,
        item_name: nil,
        is_object: false,
        is_item: false,
        lat: 33.169333,
        lon: -96.492167,
        data_type: "position",
        received_at: DateTime.utc_now(),
        symbol_table_id: "/",
        symbol_code: ">",
        comment: "Test station",
        has_position: true,
        path: "WIDE1-1"
      }

      result = DataBuilder.build_packet_data(packet, true)

      assert result
      assert result["callsign"] == "K5SGD-Y"
    end

    test "uses object_name in popup for object packets" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "DB0SDA",
        base_callsign: "DB0SDA",
        ssid: "",
        object_name: "P-K5SGD",
        is_object: true,
        is_item: false,
        lat: 33.169333,
        lon: -96.492167,
        data_type: "object",
        received_at: DateTime.utc_now(),
        symbol_table_id: "P",
        symbol_code: "#",
        comment: "DAPNET POCSAG Transmitter",
        has_position: true,
        path: "TCPIP*,qAC,FIFTH"
      }

      result = DataBuilder.build_packet_data(packet, true)

      assert result["popup"] =~ "P-K5SGD"
      refute result["popup"] =~ ">DB0SDA<"
    end

    test "build_minimal_packet_data uses object_name for objects" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "DB0SDA",
        base_callsign: "DB0SDA",
        ssid: "",
        object_name: "P-K5SGD",
        is_object: true,
        is_item: false,
        lat: 33.169333,
        lon: -96.492167,
        data_type: "object",
        received_at: DateTime.utc_now(),
        symbol_table_id: "P",
        symbol_code: "#",
        comment: "DAPNET POCSAG Transmitter",
        has_position: true,
        path: "TCPIP*,qAC,FIFTH"
      }

      result = DataBuilder.build_minimal_packet_data(packet, true, false)

      assert result
      assert result["callsign"] == "P-K5SGD"
    end

    test "build_packet_data_list groups by object_name for objects" do
      now = DateTime.utc_now()
      earlier = DateTime.add(now, -600, :second)

      object_packet_1 = %{
        id: Ecto.UUID.generate(),
        sender: "DB0SDA",
        base_callsign: "DB0SDA",
        ssid: "",
        object_name: "P-K5SGD",
        is_object: true,
        is_item: false,
        lat: 33.169333,
        lon: -96.492167,
        data_type: "object",
        received_at: now,
        symbol_table_id: "P",
        symbol_code: "#",
        comment: "DAPNET POCSAG Transmitter",
        has_position: true,
        path: "TCPIP*,qAC,FIFTH"
      }

      object_packet_2 = %{
        id: Ecto.UUID.generate(),
        sender: "DB0SDA",
        base_callsign: "DB0SDA",
        ssid: "",
        object_name: "P-K5SGD",
        is_object: true,
        is_item: false,
        lat: 33.170,
        lon: -96.493,
        data_type: "object",
        received_at: earlier,
        symbol_table_id: "P",
        symbol_code: "#",
        comment: "DAPNET POCSAG Transmitter",
        has_position: true,
        path: "TCPIP*,qAC,FIFTH"
      }

      results = DataBuilder.build_packet_data_list([object_packet_1, object_packet_2])

      # Both packets should be grouped under P-K5SGD, not DB0SDA
      callsigns = Enum.map(results, & &1["callsign"])
      assert Enum.all?(callsigns, &(&1 == "P-K5SGD"))
    end
  end
end
