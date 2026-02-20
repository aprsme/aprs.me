defmodule Aprsme.PacketTest do
  use Aprsme.DataCase, async: true

  alias Aprsme.Packet

  describe "extract_additional_data/2 with telemetry data" do
    test "converts string telemetry_vals to integers" do
      raw_packet = "W5ISP>APRS,TCPIP*:T#005,12.80,0.00,0.00,0.00,0.00,00000000"

      attrs = %{
        sender: "W5ISP",
        data_type: "telemetry",
        telemetry: %{
          seq: "005",
          vals: ["12.80", "0.00", "0.00", "0.00", "0.00"],
          bits: "00000000"
        }
      }

      result = Packet.extract_additional_data(attrs, raw_packet)

      assert result.data["telemetry_seq"] == 5
      # 12.80 rounds to 13
      assert result.data["telemetry_vals"] == [13, 0, 0, 0, 0]
      assert result.data["telemetry_bits"] == "00000000"
    end

    test "handles mixed integer and string telemetry_vals" do
      attrs = %{
        sender: "TEST",
        data_type: "telemetry",
        telemetry: %{
          seq: 10,
          vals: [180, "37.50", "0.00", 88, "164.75"],
          bits: "10101010"
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert result.data["telemetry_seq"] == 10
      # Mixed types converted properly
      assert result.data["telemetry_vals"] == [180, 38, 0, 88, 165]
      assert result.data["telemetry_bits"] == "10101010"
    end

    test "handles float telemetry_vals" do
      attrs = %{
        sender: "TEST",
        data_type: "telemetry",
        telemetry: %{
          seq: "001",
          vals: [12.8, 37.2, 0.1, 88.9, 164.5]
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert result.data["telemetry_seq"] == 1
      # Floats rounded to integers
      assert result.data["telemetry_vals"] == [13, 37, 0, 89, 165]
    end

    test "handles invalid telemetry_vals gracefully" do
      attrs = %{
        sender: "TEST",
        data_type: "telemetry",
        telemetry: %{
          seq: "002",
          vals: ["invalid", nil, "", "12.5", :atom]
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert result.data["telemetry_seq"] == 2
      # Invalid values become 0
      assert result.data["telemetry_vals"] == [0, 0, 0, 13, 0]
    end

    test "handles missing telemetry data" do
      attrs = %{
        sender: "TEST",
        data_type: "position"
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert get_in(result, [:data, "telemetry_seq"]) == nil
      assert get_in(result, [:data, "telemetry_vals"]) == nil
      assert get_in(result, [:data, "telemetry_bits"]) == nil
    end
  end

  describe "comment cleaning" do
    test "strips weather data from weather packet comments" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: "007/000g000t054r000p001P001b10052h90eMB61",
          data_type: :weather,
          wx: %{temperature: 54, humidity: 90, wind_speed: 0},
          latitude: 36.124,
          longitude: -75.723
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "eMB61"
    end

    test "strips weather data with luminosity and preserves trailing comment" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: "225/004g009t075r000p000h61b10206Plano, TX weather",
          data_type: :weather,
          wx: %{temperature: 75, humidity: 61, wind_speed: 4},
          latitude: 32.0,
          longitude: -96.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "Plano, TX weather"
    end

    test "strips weather data with snow and luminosity fields" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: "082/002g005t074r000p000P000b10099h58L708eMB63",
          data_type: :weather,
          wx: %{temperature: 74, humidity: 58, wind_speed: 2},
          latitude: 30.0,
          longitude: -90.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "eMB63"
    end

    test "handles weather comment that is all weather data" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: "_180/010g015t072r000p000h45b10132s005",
          data_type: :weather,
          wx: %{temperature: 72, humidity: 45, wind_speed: 10},
          latitude: 32.0,
          longitude: -96.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] in [nil, ""]
    end

    test "strips positionless weather format (cXXXsXXX)" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: "c011s000g000t077r000P000L000h95b10080GW2000C MaxGust:1.1mph",
          data_type: :weather,
          wx: %{temperature: 77, humidity: 95},
          latitude: 6.0,
          longitude: 79.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "GW2000C MaxGust:1.1mph"
    end

    test "strips RNG data from comments" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: "RNG0050 2m Voice 145.57500MHz",
          latitude: 61.0,
          longitude: 14.0,
          radiorange: "0050"
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "2m Voice 145.57500MHz"
    end

    test "does not modify non-weather, non-RNG comments" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: "70cm Voice (D-Star) 447.12500MHz -3.0000MHz",
          latitude: 46.0,
          longitude: -94.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "70cm Voice (D-Star) 447.12500MHz -3.0000MHz"
    end

    test "handles weather data with spaces for missing values" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: "215/004g012t046r   p   P000h  b10052KU2k",
          data_type: :weather,
          wx: %{temperature: 46, wind_speed: 4},
          latitude: 39.0,
          longitude: -84.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "KU2k"
    end

    test "handles weather data with dots for missing values" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: ".../...g...t...r...p...P...h..b.....My Station",
          data_type: :weather,
          wx: %{},
          latitude: 43.0,
          longitude: -79.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "My Station"
    end

    test "handles negative temperature in weather data" do
      attrs = %{
        sender: "TEST",
        data_type: "weather",
        data_extended: %{
          comment: "000/000g005t-12r000p000h95b10200Cold Weather Station",
          data_type: :weather,
          wx: %{temperature: -12, humidity: 95, wind_speed: 0},
          latitude: 60.0,
          longitude: 25.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "Cold Weather Station"
    end

    test "strips negative altitude from comments" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: "290/054/146.520MHz/A=-00043 https://aprsdroid.org/",
          latitude: 29.0,
          longitude: -83.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "290/054/146.520MHz https://aprsdroid.org/"
    end

    test "strips combined PHG, altitude, and RNG from comment" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: "RNG0062/A=004756 2m Voice (D-Star) 145.68750MHz",
          latitude: 43.0,
          longitude: 11.0,
          radiorange: "0062"
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "2m Voice (D-Star) 145.68750MHz"
    end

    test "does not false-match comment starting with P as weather field" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: "Please QSL via bureau",
          latitude: 40.0,
          longitude: -74.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "Please QSL via bureau"
    end

    test "does not false-match comment starting with h as weather field" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: "https://aprs.fi",
          latitude: 40.0,
          longitude: -74.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert result[:comment] == "https://aprs.fi"
    end

    test "returns nil for nil comment" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: nil,
          latitude: 40.0,
          longitude: -74.0
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert is_nil(result[:comment])
    end

    test "returns nil for comment that becomes empty after stripping" do
      attrs = %{
        sender: "TEST",
        data_type: "position",
        data_extended: %{
          comment: "PHG5530/A=000680",
          latitude: 33.0,
          longitude: -96.0,
          phg: %{power: 25, height: 320, gain: 3, directivity: 0}
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")
      assert is_nil(result[:comment])
    end
  end
end
