defmodule Aprsme.PacketsWeatherTest do
  use Aprsme.DataCase

  alias Aprsme.Packets

  describe "store_packet/1 with weather data" do
    test "removes raw_weather_data from packet attributes before insertion and parses weather string" do
      # Simulate a real APRS weather packet with correct format
      raw_packet = "TEST-1>APRS,WIDE1-1,WIDE2-2:!3216.46N/09647.82W_180/010g015t072r000p000h45b10132"

      # Parse the packet using the real APRS parser
      {:ok, parsed_packet} = Aprs.parse(raw_packet)

      # Create packet data as it would come from the parser
      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        sender: "TEST-1",
        destination: "APRS",
        data_type: "position",
        path: "WIDE1-1,WIDE2-2",
        information_field: "!3216.46N/09647.82W_180/010g015t072r000p000h45b10132",
        raw_packet: raw_packet,
        data_extended: parsed_packet
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.sender == "TEST-1"
      # The parser determines data_type, not the storage layer
      assert stored_packet.data_type == "position"
      refute Map.has_key?(stored_packet, :raw_weather_data)
      refute Map.has_key?(stored_packet, "raw_weather_data")
      assert stored_packet.temperature == 72
      assert stored_packet.humidity == 45
      assert stored_packet.wind_direction == 180
      assert stored_packet.wind_speed == 10
      assert stored_packet.wind_gust == 15
      assert stored_packet.pressure == 1013.2
      assert stored_packet.rain_1h == 0
    end

    test "removes raw_weather_data from weather packets with string weather data" do
      # Simulate a real APRS weather packet with correct format
      raw_packet = "WEATHER-1>APRS,WIDE1-1:!3216.46N/09647.82W_270/012g018t085r000p000h60b10150"

      # Parse the packet using the real APRS parser
      {:ok, parsed_packet} = Aprs.parse(raw_packet)

      packet_data = %{
        base_callsign: "WEATHER",
        ssid: "1",
        sender: "WEATHER-1",
        destination: "APRS",
        data_type: "position",
        path: "WIDE1-1",
        information_field: "!3216.46N/09647.82W_270/012g018t085r000p000h60b10150",
        raw_packet: raw_packet,
        data_extended: parsed_packet
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      # The parser determines data_type, not the storage layer
      assert stored_packet.data_type == "position"
      refute Map.has_key?(stored_packet, :raw_weather_data)
      refute Map.has_key?(stored_packet, "raw_weather_data")
      assert stored_packet.temperature == 85
      assert stored_packet.humidity == 60
      assert stored_packet.wind_direction == 270
      assert stored_packet.wind_speed == 12
      assert stored_packet.wind_gust == 18
      assert stored_packet.pressure == 1015.0
    end

    test "removes raw_weather_data from comment-based weather parsing" do
      # Simulate a real APRS packet with weather data in comment
      raw_packet = "COMMENT-1>APRS,WIDE1-1:!3216.46N/09647.82W>090/008g012t095r000p000h70b10200"

      # Parse the packet using the real APRS parser
      {:ok, parsed_packet} = Aprs.parse(raw_packet)

      packet_data = %{
        base_callsign: "COMMENT",
        ssid: "1",
        sender: "COMMENT-1",
        destination: "APRS",
        data_type: "position",
        path: "WIDE1-1",
        information_field: "!3216.46N/09647.82W>090/008g012t095r000p000h70b10200",
        raw_packet: raw_packet,
        data_extended: parsed_packet
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.data_type == "position"
      refute Map.has_key?(stored_packet, :raw_weather_data)
      refute Map.has_key?(stored_packet, "raw_weather_data")
      assert stored_packet.temperature == 95
      assert stored_packet.humidity == 70
      assert stored_packet.wind_direction == 90
      assert stored_packet.wind_speed == 8
      assert stored_packet.wind_gust == 12
      assert stored_packet.pressure == 1020.0
    end

    test "handles packets without weather data correctly" do
      # Simulate a real APRS packet without weather data
      raw_packet = "REGULAR-1>APRS,WIDE1-1:!3216.46N/09647.82W>Just a regular packet"

      # Parse the packet using the real APRS parser
      {:ok, parsed_packet} = Aprs.parse(raw_packet)

      packet_data = %{
        base_callsign: "REGULAR",
        ssid: "1",
        sender: "REGULAR-1",
        destination: "APRS",
        data_type: "position",
        path: "WIDE1-1",
        information_field: "!3216.46N/09647.82W>Just a regular packet",
        raw_packet: raw_packet,
        data_extended: parsed_packet
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.data_type == "position"
      assert is_nil(stored_packet.temperature)
      assert is_nil(stored_packet.humidity)
      assert is_nil(stored_packet.wind_direction)
      assert is_nil(stored_packet.wind_speed)
      assert is_nil(stored_packet.wind_gust)
      assert is_nil(stored_packet.pressure)
      assert is_nil(stored_packet.rain_1h)
      assert is_nil(stored_packet.rain_24h)
      assert is_nil(stored_packet.rain_since_midnight)
      refute Map.has_key?(stored_packet, :raw_weather_data)
      refute Map.has_key?(stored_packet, "raw_weather_data")
    end

    test "handles batch insert operations correctly" do
      # Simulate real APRS weather packets with correct format
      raw_packet1 = "BATCH1-1>APRS,WIDE1-1:!3216.46N/09647.82W_200/010g015t070r000p000h50b10120"
      raw_packet2 = "BATCH2-1>APRS,WIDE1-1:!3216.46N/09647.82W_220/012g018t075r000p000h55b10140"

      # Parse the packets using the real APRS parser
      {:ok, parsed_packet1} = Aprs.parse(raw_packet1)
      {:ok, parsed_packet2} = Aprs.parse(raw_packet2)

      packets_data = [
        %{
          base_callsign: "BATCH1",
          ssid: "1",
          sender: "BATCH1-1",
          destination: "APRS",
          data_type: "position",
          path: "WIDE1-1",
          information_field: "!3216.46N/09647.82W_200/010g015t070r000p000h50b10120",
          raw_packet: raw_packet1,
          data_extended: parsed_packet1
        },
        %{
          base_callsign: "BATCH2",
          ssid: "1",
          sender: "BATCH2-1",
          destination: "APRS",
          data_type: "position",
          path: "WIDE1-1",
          information_field: "!3216.46N/09647.82W_220/012g018t075r000p000h55b10140",
          raw_packet: raw_packet2,
          data_extended: parsed_packet2
        }
      ]

      results = Enum.map(packets_data, &Packets.store_packet/1)
      assert Enum.all?(results, fn {status, _} -> status == :ok end)

      Enum.each(results, fn {:ok, stored_packet} ->
        refute Map.has_key?(stored_packet, :raw_weather_data)
        refute Map.has_key?(stored_packet, "raw_weather_data")
      end)

      [first_result, second_result] = results
      {:ok, first_packet} = first_result
      {:ok, second_packet} = second_result

      assert first_packet.temperature == 70
      assert first_packet.humidity == 50
      assert first_packet.wind_direction == 200
      assert second_packet.temperature == 75
      assert second_packet.humidity == 55
      assert second_packet.wind_direction == 220
    end

    test "handles snow data correctly" do
      # Simulate a real APRS weather packet with snow data
      raw_packet = "SNOW-1>APRS,WIDE1-1:!3216.46N/09647.82W_180/010g015t072r000p000h45b10132s005"

      # Parse the packet using the real APRS parser
      {:ok, parsed_packet} = Aprs.parse(raw_packet)

      packet_data = %{
        base_callsign: "SNOW",
        ssid: "1",
        sender: "SNOW-1",
        destination: "APRS",
        data_type: "position",
        path: "WIDE1-1",
        information_field: "!3216.46N/09647.82W_180/010g015t072r000p000h45b10132s005",
        raw_packet: raw_packet,
        data_extended: parsed_packet
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.data_type == "position"
      assert stored_packet.snow == 0.5
      refute Map.has_key?(stored_packet, :raw_weather_data)
      refute Map.has_key?(stored_packet, "raw_weather_data")
    end
  end
end
