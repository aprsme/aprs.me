defmodule Aprsme.EnhancedParserTest do
  use Aprsme.DataCase, async: true

  alias Aprsme.Packet

  describe "enhanced parser field extraction" do
    test "dead parser compat fields are dropped (not preserved)" do
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

      # Dead fields should not be in result
      refute Map.has_key?(result, :srccallsign)
      refute Map.has_key?(result, :dstcallsign)
      refute Map.has_key?(result, :body)
      refute Map.has_key?(result, :origpacket)
      refute Map.has_key?(result, :header)
      refute Map.has_key?(result, :alive)
      refute Map.has_key?(result, :posambiguity)
      refute Map.has_key?(result, :symboltable)
      refute Map.has_key?(result, :symbolcode)
      refute Map.has_key?(result, :messaging)
    end

    test "extracts radio range into data map" do
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

      assert result[:data]["radiorange"] == "0050"
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

    test "handles PHG data in string format and moves to data map" do
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
      # PHG fields should be in the data map
      assert result[:data]["phg_power"] == 25
      assert result[:data]["phg_height"] == 160
      assert result[:data]["phg_gain"] == 3
      assert result[:data]["phg_directivity"] == 0
    end

    test "handles PHG data in legacy map format and moves to data map" do
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

      assert result[:data]["phg_power"] == 25
      assert result[:data]["phg_height"] == 160
      assert result[:data]["phg_gain"] == 3
      assert result[:data]["phg_directivity"] == 0
    end

    test "collect_into_data_map sweeps all 14 fields into data" do
      attrs = %{
        sender: "W1AW",
        base_callsign: "W1AW",
        ssid: "0",
        destination: "APRS",
        data_type: "position",
        received_at: DateTime.utc_now(),
        data_extended: %{
          phg: "5430",
          format: :uncompressed,
          posresolution: 18.52,
          position_ambiguity: 0,
          comment: "Test PHG5430"
        }
      }

      result = Packet.extract_additional_data(attrs)

      # These fields should be in the data map, not at top level
      assert is_map(result[:data])
      assert result[:data]["phg_power"] == 25
      assert result[:data]["format"] == "uncompressed"
      assert result[:data]["posresolution"] == 18.52
      assert result[:data]["position_ambiguity"] == 0

      # They should NOT be at top level
      refute Map.has_key?(result, :phg_power)
      refute Map.has_key?(result, :format)
      refute Map.has_key?(result, :posresolution)
      refute Map.has_key?(result, :position_ambiguity)
    end

    test "telemetry fields are moved into data map" do
      attrs = %{
        sender: "W1AW",
        base_callsign: "W1AW",
        ssid: "0",
        destination: "APRS",
        data_type: "telemetry",
        received_at: DateTime.utc_now(),
        data_extended: %{
          telemetry: %{
            seq: 123,
            vals: [1, 2, 3, 4, 5],
            bits: "11111111"
          }
        }
      }

      result = Packet.extract_additional_data(attrs)

      assert result[:data]["telemetry_seq"] == 123
      assert result[:data]["telemetry_vals"] == [1, 2, 3, 4, 5]
      assert result[:data]["telemetry_bits"] == "11111111"

      refute Map.has_key?(result, :telemetry_seq)
      refute Map.has_key?(result, :telemetry_vals)
      refute Map.has_key?(result, :telemetry_bits)
    end

    test "luminosity and rain_midnight are moved into data map" do
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
            luminosity: 500,
            rain_midnight: 0.5
          }
        }
      }

      result = Packet.extract_additional_data(attrs)

      # luminosity and rain_midnight should be in data map
      assert result[:data]["luminosity"] == 500
      assert result[:data]["rain_midnight"] == 0.5

      # temperature stays at top level
      assert result[:temperature] == 72.5
    end
  end

  describe "enhanced parser packet storage" do
    test "stores packet with data map fields" do
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
        radiorange: "0050",
        information_field: "!1234.56N/12345.67W-Test"
      }

      processed_attrs = Packet.extract_additional_data(attrs)

      assert {:ok, packet} = Aprsme.Packets.store_packet(processed_attrs)

      # radiorange and information_field should be in data map
      assert packet.data["radiorange"] == "0050"
      assert packet.data["information_field"] == "!1234.56N/12345.67W-Test"
    end
  end
end
