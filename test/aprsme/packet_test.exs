defmodule Aprsme.PacketTest do
  use Aprsme.DataCase, async: true

  alias Aprsme.Packet

  describe "extract_additional_data/2" do
    test "position packet with course/speed should not be classified as weather" do
      # This is the problematic packet from KG5GKC-12
      raw_packet =
        "KG5GKC-12>APAT51,WIDE1-1,WIDE2-2,qAO,NI2C-10:!3310.04N/09640.40Wk038/023/A=000623AT-MOBILE KG5GKC@YAHOO.COM"

      # Simulate the parsed data from APRS parser
      attrs = %{
        sender: "KG5GKC-12",
        destination: "APAT51",
        path: "WIDE1-1,WIDE2-2,qAO,NI2C-10",
        information_field: "!3310.04N/09640.40Wk038/023/A=000623AT-MOBILE KG5GKC@YAHOO.COM",
        data_type: :position,
        base_callsign: "KG5GKC",
        ssid: "12",
        data_extended: %{
          latitude: Decimal.new("33.167333"),
          longitude: Decimal.new("-96.673333"),
          symbol_table_id: "/",
          symbol_code: "k",
          comment: "038/023/A=000623AT-MOBILE KG5GKC@YAHOO.COM",
          course: 38,
          speed: 23.0,
          data_type: :position,
          has_position: true
        }
      }

      result = Packet.extract_additional_data(attrs, raw_packet)

      # The data_type should be "position" (normalized to string)
      assert result[:data_type] == "position" or result[:data_type] == :position
      assert result[:symbol_code] == "k"
      assert result[:symbol_table_id] == "/"
      assert result[:course] == 38
      assert result[:speed] == 23.0
      assert result[:raw_packet] == raw_packet
    end

    test "actual weather packet should be classified as weather" do
      raw_packet = "KC0ABC>APRS:_10090556c220s004g005t077P000h50b09900"

      attrs = %{
        sender: "KC0ABC",
        destination: "APRS",
        path: "",
        information_field: "_10090556c220s004g005t077P000h50b09900",
        data_type: :weather,
        base_callsign: "KC0ABC",
        ssid: nil,
        data_extended: %{
          timestamp: "10090556",
          wind_direction: 220,
          wind_speed: 4.0,
          wind_gust: 5.0,
          temperature: 77.0,
          rain_since_midnight: 0.0,
          humidity: 50.0,
          pressure: 990.0,
          data_type: :weather
        }
      }

      result = Packet.extract_additional_data(attrs, raw_packet)

      # Weather packet should remain as weather (normalized to string)
      assert result[:data_type] == "weather" or result[:data_type] == :weather
      assert result[:temperature] == 77.0
      assert result[:humidity] == 50.0
      assert result[:pressure] == 990.0
    end

    test "position packet with weather symbol should use parser's determination" do
      raw_packet = "KC0ABC>APRS:=3310.04N/09640.40W_PHG5130"

      attrs = %{
        sender: "KC0ABC",
        destination: "APRS",
        path: "",
        information_field: "=3310.04N/09640.40W_PHG5130",
        # Parser determined this is position despite weather symbol
        data_type: :position,
        base_callsign: "KC0ABC",
        ssid: nil,
        data_extended: %{
          latitude: Decimal.new("33.167333"),
          longitude: Decimal.new("-96.673333"),
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "PHG5130",
          data_type: :position,
          has_position: true
        }
      }

      result = Packet.extract_additional_data(attrs, raw_packet)

      # Should trust parser's determination (normalized to string)
      assert result[:data_type] == "position" or result[:data_type] == :position
      assert result[:symbol_code] == "_"
    end
  end
end
