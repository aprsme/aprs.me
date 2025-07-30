defmodule AprsmeWeb.MapLive.PacketUtilsTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.MapLive.PacketUtils

  describe "weather popup generation" do
    test "generates weather popup with actual weather data" do
      packet = %{
        id: 1,
        base_callsign: "TEST",
        sender: "TEST-1",
        ssid: "1",
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: "_",
        lat: 32.7767,
        lon: -96.7970,
        received_at: DateTime.utc_now(),
        temperature: 72.0,
        humidity: 45.0,
        wind_direction: 180,
        wind_speed: 10.0,
        wind_gust: 15.0,
        pressure: 1013.2,
        rain_1h: 0.0,
        rain_24h: 0.1,
        rain_since_midnight: 0.2
      }

      result = PacketUtils.build_packet_data(packet, true)

      assert result
      assert result["popup"]

      popup = result["popup"]

      # Check that it's a weather popup
      assert popup =~ "Weather Report"
      assert popup =~ "data-timestamp="

      # Check weather data is present
      assert popup =~ "Temperature: 72.0°F"
      assert popup =~ "Humidity: 45.0%"
      assert popup =~ "Wind: 180° at 10.0 mph, gusts to 15.0 mph"
      assert popup =~ "Pressure: 1013.2 hPa"
      assert popup =~ "Rain (1h): 0.0 in"
      assert popup =~ "Rain (24h): 0.1 in"
      assert popup =~ "Rain (since midnight): 0.2 in"

      # Check callsign and links
      assert popup =~ "TEST-1"
      assert popup =~ "/TEST-1"
      assert popup =~ "/info/TEST-1"
      assert popup =~ "/weather/TEST-1"
      assert popup =~ "weather charts"
    end

    test "generates weather popup with N/A for missing weather data" do
      packet = %{
        id: 1,
        base_callsign: "TEST",
        sender: "TEST-1",
        ssid: "1",
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: "_",
        lat: 32.7767,
        lon: -96.7970,
        received_at: DateTime.utc_now(),
        # Add at least one non-nil weather field to make it a weather packet
        temperature: 75,
        humidity: nil,
        pressure: nil,
        wind_speed: nil,
        wind_direction: nil,
        wind_gust: nil,
        rain_1h: nil,
        rain_24h: nil,
        rain_since_midnight: nil
      }

      result = PacketUtils.build_packet_data(packet, true)

      assert result
      assert result["popup"]

      popup = result["popup"]

      # Check that it's a weather popup
      assert popup =~ "Weather Report"
      assert popup =~ "data-timestamp="

      # Check temperature is shown, others show N/A
      assert popup =~ "Temperature: 75°F"
      assert popup =~ "Humidity: N/A%"
      assert popup =~ "Wind: N/A° at N/A mph, gusts to N/A mph"
      assert popup =~ "Pressure: N/A hPa"
      assert popup =~ "Rain (1h): N/A in"
      assert popup =~ "Rain (24h): N/A in"
      assert popup =~ "Rain (since midnight): N/A in"
    end

    test "generates weather popup with partial weather data" do
      packet = %{
        id: 1,
        base_callsign: "TEST",
        sender: "TEST-1",
        ssid: "1",
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: "_",
        lat: 32.7767,
        lon: -96.7970,
        received_at: DateTime.utc_now(),
        temperature: 85.0,
        humidity: 60.0
        # Missing wind, pressure, rain data
      }

      result = PacketUtils.build_packet_data(packet, true)

      assert result
      assert result["popup"]

      popup = result["popup"]

      # Check that it's a weather popup
      assert popup =~ "Weather Report"
      assert popup =~ "data-timestamp="

      # Check available weather data
      assert popup =~ "Temperature: 85.0°F"
      assert popup =~ "Humidity: 60.0%"

      # Check missing data shows N/A
      assert popup =~ "Wind: N/A° at N/A mph, gusts to N/A mph"
      assert popup =~ "Pressure: N/A hPa"
      assert popup =~ "Rain (1h): N/A in"
      assert popup =~ "Rain (24h): N/A in"
      assert popup =~ "Rain (since midnight): N/A in"
    end

    test "generates standard popup for non-weather packets" do
      packet = %{
        id: 1,
        base_callsign: "TEST",
        sender: "TEST-1",
        ssid: "1",
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: ">",
        lat: 32.7767,
        lon: -96.7970,
        received_at: DateTime.utc_now(),
        comment: "Test comment"
      }

      result = PacketUtils.build_packet_data(packet, true)

      assert result
      assert result["popup"]

      popup = result["popup"]

      # Check that it's NOT a weather popup
      refute popup =~ "Weather Report"
      # All popups now have data-timestamp for cache busting
      assert popup =~ "data-timestamp="

      # Check standard popup content
      assert popup =~ "TEST-1"
      assert popup =~ "/TEST-1"
      assert popup =~ "/info/TEST-1"
      assert popup =~ "Test comment"
    end

    test "generates weather popup with timestamp" do
      received_at = DateTime.utc_now()

      packet = %{
        id: 1,
        base_callsign: "TEST",
        sender: "TEST-1",
        ssid: "1",
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: "_",
        lat: 32.7767,
        lon: -96.7970,
        received_at: received_at,
        temperature: 72.0,
        humidity: 45.0
      }

      result = PacketUtils.build_packet_data(packet, true)

      assert result
      assert result["popup"]

      popup = result["popup"]

      # Check timestamp is included
      assert popup =~ "aprs-timestamp"
      assert popup =~ Calendar.strftime(received_at, "%Y-%m-%d %H:%M:%S UTC")
    end

    test "handles weather data from data_extended field" do
      packet = %{
        id: 1,
        base_callsign: "TEST",
        sender: "TEST-1",
        ssid: "1",
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: "_",
        lat: 32.7767,
        lon: -96.7970,
        received_at: DateTime.utc_now(),
        data_extended: %{
          temperature: 75.0,
          humidity: 50.0,
          wind_direction: 270,
          wind_speed: 12.0,
          pressure: 1015.0
        }
      }

      result = PacketUtils.build_packet_data(packet, true)

      assert result
      assert result["popup"]

      popup = result["popup"]

      # Check weather data from data_extended
      assert popup =~ "Temperature: 75.0°F"
      assert popup =~ "Humidity: 50.0%"
      assert popup =~ "Wind: 270° at 12.0 mph"
      assert popup =~ "Pressure: 1015.0 hPa"
    end

    test "handles weather data with string keys in data_extended" do
      packet = %{
        id: 1,
        base_callsign: "TEST",
        sender: "TEST-1",
        ssid: "1",
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: "_",
        lat: 32.7767,
        lon: -96.7970,
        received_at: DateTime.utc_now(),
        data_extended: %{
          "temperature" => 80.0,
          "humidity" => 55.0,
          "wind_direction" => 90,
          "wind_speed" => 8.0
        }
      }

      result = PacketUtils.build_packet_data(packet, true)

      assert result
      assert result["popup"]

      popup = result["popup"]

      # Check weather data from data_extended with string keys
      assert popup =~ "Temperature: 80.0°F"
      assert popup =~ "Humidity: 55.0%"
      assert popup =~ "Wind: 90° at 8.0 mph"
    end
  end

  describe "weather packet detection" do
    test "identifies weather packets by weather data fields" do
      weather_packet = %{
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: "_",
        temperature: 75
      }

      assert PacketUtils.weather_packet?(weather_packet)
    end

    test "identifies weather packets with humidity" do
      weather_packet = %{
        data_type: "weather",
        symbol_table_id: "/",
        symbol_code: ">",
        humidity: 80
      }

      assert PacketUtils.weather_packet?(weather_packet)
    end

    test "does not identify non-weather packets" do
      regular_packet = %{
        data_type: "position",
        symbol_table_id: "/",
        symbol_code: ">"
      }

      refute PacketUtils.weather_packet?(regular_packet)
    end

    test "has_weather_packets? returns false for non-existent callsign" do
      # This should return false since the database call will fail in test context
      refute PacketUtils.has_weather_packets?("NONEXISTENT")
    end

    test "has_weather_packets? handles invalid input gracefully" do
      refute PacketUtils.has_weather_packets?(nil)
      refute PacketUtils.has_weather_packets?("")
      refute PacketUtils.has_weather_packets?(123)
    end
  end

  describe "weather field extraction" do
    test "extracts weather fields from packet" do
      packet = %{
        temperature: 72.0,
        humidity: 45.0,
        wind_direction: 180,
        wind_speed: 10.0
      }

      assert PacketUtils.get_weather_field(packet, :temperature) == 72.0
      assert PacketUtils.get_weather_field(packet, :humidity) == 45.0
      assert PacketUtils.get_weather_field(packet, :wind_direction) == 180
      assert PacketUtils.get_weather_field(packet, :wind_speed) == 10.0
    end

    test "extracts weather fields from data_extended" do
      packet = %{
        data_extended: %{
          temperature: 75.0,
          humidity: 50.0
        }
      }

      assert PacketUtils.get_weather_field(packet, :temperature) == 75.0
      assert PacketUtils.get_weather_field(packet, :humidity) == 50.0
    end

    test "returns N/A for missing weather fields" do
      packet = %{}

      assert PacketUtils.get_weather_field(packet, :temperature) == "N/A"
      assert PacketUtils.get_weather_field(packet, :humidity) == "N/A"
      assert PacketUtils.get_weather_field(packet, :wind_direction) == "N/A"
    end

    test "handles string keys in data_extended" do
      packet = %{
        data_extended: %{
          "temperature" => 80.0,
          "humidity" => 55.0
        }
      }

      assert PacketUtils.get_weather_field(packet, :temperature) == 80.0
      assert PacketUtils.get_weather_field(packet, :humidity) == 55.0
    end

    test "handles string keys in packet" do
      packet = %{
        "temperature" => 85.0,
        "humidity" => 60.0
      }

      assert PacketUtils.get_weather_field(packet, :temperature) == 85.0
      assert PacketUtils.get_weather_field(packet, :humidity) == 60.0
    end
  end

  describe "callsign generation" do
    test "generates callsign with SSID" do
      packet = %{
        base_callsign: "TEST",
        ssid: "1"
      }

      assert PacketUtils.generate_callsign(packet) == "TEST-1"
    end

    test "generates callsign without SSID" do
      packet = %{
        base_callsign: "TEST",
        ssid: nil
      }

      assert PacketUtils.generate_callsign(packet) == "TEST"
    end

    test "generates callsign with empty SSID" do
      packet = %{
        base_callsign: "TEST",
        ssid: ""
      }

      assert PacketUtils.generate_callsign(packet) == "TEST"
    end
  end
end
