defmodule Aprsme.EnhancedParserTest do
  use Aprsme.DataCase, async: true

  alias Aprsme.Packet

  describe "enhanced parser field extraction" do
    test "extracts standard parser compatibility fields" do
      attrs = %{
        srccallsign: "W1AW",
        dstcallsign: "APRS",
        body: "!1234.56N/12345.67W-Test",
        origpacket: "W1AW>APRS:!1234.56N/12345.67W-Test",
        header: "W1AW>APRS",
        alive: 1,
        posambiguity: 0,
        symboltable: "/",
        symbolcode: "-",
        messaging: 0,
        sender: "W1AW",
        base_callsign: "W1AW",
        ssid: "0",
        destination: "APRS",
        data_type: "position",
        received_at: DateTime.utc_now()
      }

      result = Packet.extract_additional_data(attrs, "W1AW>APRS:!1234.56N/12345.67W-Test")

      assert result[:srccallsign] == "W1AW"
      assert result[:dstcallsign] == "APRS"
      assert result[:body] == "!1234.56N/12345.67W-Test"
      assert result[:origpacket] == "W1AW>APRS:!1234.56N/12345.67W-Test"
      assert result[:header] == "W1AW>APRS"
      assert result[:alive] == 1
      assert result[:posambiguity] == 0
      assert result[:symboltable] == "/"
      assert result[:symbolcode] == "-"
      assert result[:messaging] == 0
    end

    test "extracts radio range field" do
      attrs = %{
        radiorange: "0050",
        sender: "W1AW",
        base_callsign: "W1AW",
        ssid: "0",
        destination: "APRS",
        data_type: "position",
        received_at: DateTime.utc_now()
      }

      result = Packet.extract_additional_data(attrs)

      assert result[:radiorange] == "0050"
    end

    test "handles weather data from wx field" do
      attrs = %{
        sender: "WX1STN",
        base_callsign: "WX1STN",
        ssid: "0",
        destination: "APRS",
        data_type: "weather",
        received_at: DateTime.utc_now(),
        data_extended: %{
          wx: %{
            temperature: 72.5,
            humidity: 65,
            pressure: 1013.2,
            wind_speed: 12.5,
            wind_direction: 180
          }
        }
      }

      result = Packet.extract_additional_data(attrs)

      assert result[:temperature] == 72.5
      assert result[:humidity] == 65
      assert result[:pressure] == 1013.2
      assert result[:wind_speed] == 12.5
      assert result[:wind_direction] == 180
    end

    test "handles PHG data in string format" do
      attrs = %{
        sender: "W1AW",
        base_callsign: "W1AW",
        ssid: "0",
        destination: "APRS",
        data_type: "position",
        received_at: DateTime.utc_now(),
        data_extended: %{
          phg: "5430"
        }
      }

      result = Packet.extract_additional_data(attrs)

      # PHG 5430: power=5^2=25W, height=4->160ft, gain=3dB, dir=0->omni  
      assert result[:phg_power] == 25
      assert result[:phg_height] == 160
      assert result[:phg_gain] == 3
      assert result[:phg_directivity] == 0
    end

    test "handles PHG data in legacy map format" do
      attrs = %{
        sender: "W1AW",
        base_callsign: "W1AW",
        ssid: "0",
        destination: "APRS",
        data_type: "position",
        received_at: DateTime.utc_now(),
        data_extended: %{
          phg: %{
            power: 25,
            height: 160,
            gain: 3,
            directivity: 0
          }
        }
      }

      result = Packet.extract_additional_data(attrs)

      assert result[:phg_power] == 25
      assert result[:phg_height] == 160
      assert result[:phg_gain] == 3
      assert result[:phg_directivity] == 0
    end
  end

  describe "enhanced parser packet storage" do
    test "stores packet with enhanced parser fields" do
      attrs = %{
        sender: "W1AW-1",
        base_callsign: "W1AW",
        ssid: "1",
        destination: "APRS",
        data_type: "position",
        received_at: DateTime.utc_now(),
        raw_packet: "W1AW-1>APRS:!1234.56N/12345.67W-Test RNG0050",
        lat: 12.576,
        lon: -123.761,
        symbol_code: "-",
        symbol_table_id: "/",
        comment: "Test station",
        # Enhanced parser fields
        srccallsign: "W1AW-1",
        dstcallsign: "APRS",
        body: "!1234.56N/12345.67W-Test RNG0050",
        origpacket: "W1AW-1>APRS:!1234.56N/12345.67W-Test RNG0050",
        header: "W1AW-1>APRS",
        alive: 1,
        posambiguity: 0,
        symboltable: "/",
        symbolcode: "-",
        messaging: 0,
        radiorange: "0050"
      }

      processed_attrs = Packet.extract_additional_data(attrs)

      assert {:ok, packet} = Aprsme.Packets.store_packet(processed_attrs)

      # Verify enhanced fields are stored
      assert packet.srccallsign == "W1AW-1"
      assert packet.dstcallsign == "APRS"
      assert packet.body == "!1234.56N/12345.67W-Test RNG0050"
      assert packet.origpacket == "W1AW-1>APRS:!1234.56N/12345.67W-Test RNG0050"
      assert packet.header == "W1AW-1>APRS"
      assert packet.alive == 1
      assert packet.posambiguity == 0
      assert packet.symboltable == "/"
      assert packet.symbolcode == "-"
      assert packet.messaging == 0
      assert packet.radiorange == "0050"
    end
  end
end
