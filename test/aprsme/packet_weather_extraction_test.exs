defmodule Aprsme.PacketWeatherExtractionTest do
  use Aprsme.DataCase

  alias Aprsme.Packet

  describe "extract_additional_data/2 with weather data" do
    test "extracts weather data from parsed APRS weather packet" do
      # Simulate a real weather packet
      raw_packet = "AE5PL-13>API510,DSTAR*:!3240.46N/09657.49W_225/004g009t075r000p000h61b10206Plano, TX weather"

      # Parse the packet using the real APRS parser
      {:ok, parsed_packet} = Aprs.parse(raw_packet)

      # Create packet data as it would come from the parser
      attrs = %{
        base_callsign: "AE5PL",
        ssid: "13",
        sender: "AE5PL-13",
        destination: "API510",
        data_type: to_string(parsed_packet[:data_type]),
        path: "DSTAR*",
        information_field: "!3240.46N/09657.49W_225/004g009t075r000p000h61b10206Plano, TX weather",
        data_extended: parsed_packet
      }

      # Extract additional data
      result = Packet.extract_additional_data(attrs, raw_packet)

      # Verify weather data was extracted
      assert result.temperature == 75
      assert result.humidity == 61
      assert result.wind_direction == 225
      assert result.wind_speed == 4
      assert result.wind_gust == 9
      assert result.pressure == 1020.6
      assert result.rain_1h == 0
    end

    test "extracts weather data from pure weather packet type" do
      # Simulate a pure weather packet (starts with _)
      raw_packet = "AE5PL-WX>APRS:_07121545c220s004g009t075r000p000h61b10206"

      # Parse the packet using the real APRS parser
      {:ok, parsed_packet} = Aprs.parse(raw_packet)

      # Create packet data as it would come from the parser
      attrs = %{
        base_callsign: "AE5PL",
        ssid: "WX",
        sender: "AE5PL-WX",
        destination: "APRS",
        data_type: to_string(parsed_packet[:data_type]),
        path: nil,
        information_field: "_07121545c220s004g009t075r000p000h61b10206",
        data_extended: parsed_packet
      }

      # Extract additional data
      result = Packet.extract_additional_data(attrs, raw_packet)

      # Verify weather data was extracted
      assert result.temperature == 75
      assert result.humidity == 61
      assert result.wind_direction == 220
      assert result.wind_speed == 4
      assert result.wind_gust == 9
      assert result.pressure == 1020.6
      assert result.rain_1h == 0
    end
  end
end
