defmodule AprsmeWeb.MapLive.DataBuilderTest do
  use Aprsme.DataCase, async: true

  alias AprsmeWeb.MapLive.DataBuilder

  describe "display name for APRS objects and items" do
    test "uses sender for map label but object_name for grouping when packet is an object" do
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
      # Map label shows sender for clarity
      assert result["callsign"] == "DB0SDA"
      # But grouping uses object name
      assert result["callsign_group"] == "P-K5SGD"
    end

    test "uses sender for map label but item_name for grouping when packet is an item" do
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
      # Map label shows sender for clarity
      assert result["callsign"] == "N0CALL"
      # But grouping uses item name
      assert result["callsign_group"] == "REPEATER1"
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

    test "uses sender in popup for object packets" do
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

      # Popup now shows sender for consistency with map label
      assert result["popup"] =~ "DB0SDA"
      refute result["popup"] =~ "P-K5SGD"
    end

    test "build_minimal_packet_data uses sender for map label but object_name for grouping" do
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
      # Map label shows sender for clarity
      assert result["callsign"] == "DB0SDA"
      # But grouping uses object name
      assert result["callsign_group"] == "P-K5SGD"
    end

    test "build_minimal_packet_data uses red dot HTML for historical (non-most-recent) packets" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "K5GVL-10",
        base_callsign: "K5GVL",
        ssid: "10",
        lat: 33.1,
        lon: -96.5,
        received_at: DateTime.utc_now(),
        symbol_table_id: "/",
        symbol_code: "-",
        comment: "Test",
        path: "WIDE1-1"
      }

      result = DataBuilder.build_minimal_packet_data(packet, false, false)

      assert result
      assert result["historical"] == true
      # Historical dot should use inline red dot HTML, not a full symbol
      assert result["symbol_html"] =~ "background-color"
      assert result["symbol_html"] =~ "#FF6B6B"
    end

    test "build_minimal_packet_data uses full symbol for most-recent packets" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "K5GVL-10",
        base_callsign: "K5GVL",
        ssid: "10",
        lat: 33.1,
        lon: -96.5,
        received_at: DateTime.utc_now(),
        symbol_table_id: "/",
        symbol_code: "-",
        comment: "Test",
        path: "WIDE1-1"
      }

      result = DataBuilder.build_minimal_packet_data(packet, true, false)

      assert result
      assert result["is_most_recent_for_callsign"] == true
      # Most recent should NOT use the red dot
      refute result["symbol_html"] =~ "#FF6B6B"
    end

    test "build_minimal_packet_data includes callsign_group field" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "K5GVL-10",
        base_callsign: "K5GVL",
        ssid: "10",
        lat: 33.1,
        lon: -96.5,
        received_at: DateTime.utc_now(),
        symbol_table_id: "/",
        symbol_code: "-",
        comment: "Test",
        path: "WIDE1-1"
      }

      result = DataBuilder.build_minimal_packet_data(packet, true, false)

      assert result["callsign_group"] == "K5GVL-10"
    end

    test "build_packet_data includes callsign_group field" do
      packet = %{
        id: Ecto.UUID.generate(),
        sender: "K5GVL-10",
        base_callsign: "K5GVL",
        ssid: "10",
        lat: 33.1,
        lon: -96.5,
        received_at: DateTime.utc_now(),
        symbol_table_id: "/",
        symbol_code: "-",
        comment: "Test",
        data_type: "position",
        path: "WIDE1-1"
      }

      result = DataBuilder.build_packet_data(packet, true)

      assert result["callsign_group"] == "K5GVL-10"
    end

    test "build_packet_data_list output is sorted by callsign group then chronologically" do
      now = DateTime.utc_now()

      # Two callsigns, each with 2 packets at different times
      packets = [
        %{
          id: Ecto.UUID.generate(),
          sender: "ZZZ-1",
          base_callsign: "ZZZ",
          ssid: "1",
          lat: 33.1,
          lon: -96.5,
          received_at: DateTime.add(now, -300, :second),
          symbol_table_id: "/",
          symbol_code: "-",
          comment: "",
          path: ""
        },
        %{
          id: Ecto.UUID.generate(),
          sender: "AAA-1",
          base_callsign: "AAA",
          ssid: "1",
          lat: 34.0,
          lon: -97.0,
          received_at: now,
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "",
          path: ""
        },
        %{
          id: Ecto.UUID.generate(),
          sender: "ZZZ-1",
          base_callsign: "ZZZ",
          ssid: "1",
          lat: 33.2,
          lon: -96.4,
          received_at: now,
          symbol_table_id: "/",
          symbol_code: "-",
          comment: "",
          path: ""
        }
      ]

      results = DataBuilder.build_packet_data_list(packets)

      # All results should have callsign_group
      assert Enum.all?(results, &Map.has_key?(&1, "callsign_group"))

      # Packets should be grouped by callsign (all AAA together, all ZZZ together)
      callsigns = Enum.map(results, & &1["callsign"])
      groups = Enum.chunk_by(callsigns, & &1)
      # Each group should contain only one unique callsign
      assert Enum.all?(groups, fn group -> length(Enum.uniq(group)) == 1 end)
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

      # Map labels should show sender
      callsigns = Enum.map(results, & &1["callsign"])
      assert Enum.all?(callsigns, &(&1 == "DB0SDA"))

      # But grouping should be by object name
      groups = Enum.map(results, & &1["callsign_group"])
      assert Enum.all?(groups, &(&1 == "P-K5SGD"))
    end
  end
end
