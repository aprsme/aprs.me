defmodule Aprsme.Packets.PreparedQueriesTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.Packet
  alias Aprsme.Packets.PreparedQueries
  alias Aprsme.Repo

  defp create_positioned_packet(attrs) do
    defaults = %{
      sender: "TEST-1",
      base_callsign: "TEST",
      ssid: "1",
      destination: "APRS",
      data_type: "position",
      lat: Decimal.new("33.0000"),
      lon: Decimal.new("-96.0000"),
      received_at: DateTime.truncate(DateTime.utc_now(), :second)
    }

    merged = Map.merge(defaults, attrs)

    %Packet{}
    |> Packet.changeset(Map.from_struct(Map.merge(%Packet{}, merged)))
    |> Repo.insert()
  end

  describe "get_latest_packets_for_callsigns/1" do
    test "returns full packet structs for multiple callsigns" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          comment: "Digi TX"
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "N5TXZ-10",
          base_callsign: "N5TXZ",
          ssid: "10",
          lat: Decimal.new("33.2000"),
          lon: Decimal.new("-96.5000"),
          comment: "iGate"
        })

      result = PreparedQueries.get_latest_packets_for_callsigns(["K5GVL-10", "N5TXZ-10"])

      assert length(result) == 2
      assert Enum.all?(result, &is_struct(&1, Packet))

      senders = Enum.map(result, & &1.sender)
      assert "K5GVL-10" in senders
      assert "N5TXZ-10" in senders

      k5gvl = Enum.find(result, &(&1.sender == "K5GVL-10"))
      assert_in_delta k5gvl.lat, 33.1, 0.01
      assert_in_delta k5gvl.lon, -96.6, 0.01
    end

    test "returns empty list for empty input" do
      assert PreparedQueries.get_latest_packets_for_callsigns([]) == []
    end

    test "returns empty list for nonexistent callsigns" do
      result = PreparedQueries.get_latest_packets_for_callsigns(["NONEXIST-1", "FAKE-2"])
      assert result == []
    end

    test "returns latest packet when multiple exist for a callsign" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second)
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("34.0000"),
          lon: Decimal.new("-97.0000"),
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      result = PreparedQueries.get_latest_packets_for_callsigns(["K5GVL-10"])

      assert length(result) == 1
      packet = hd(result)
      assert is_struct(packet, Packet)
      assert_in_delta packet.lat, 34.0, 0.01
      assert_in_delta packet.lon, -97.0, 0.01
    end
  end

  describe "get_latest_positions_for_callsigns/1" do
    test "returns positions for multiple callsigns" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000")
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "N5TXZ-10",
          base_callsign: "N5TXZ",
          ssid: "10",
          lat: Decimal.new("33.2000"),
          lon: Decimal.new("-96.5000")
        })

      result = PreparedQueries.get_latest_positions_for_callsigns(["K5GVL-10", "N5TXZ-10"])

      assert length(result) == 2
      callsigns = Enum.map(result, & &1.callsign)
      assert "K5GVL-10" in callsigns
      assert "N5TXZ-10" in callsigns

      k5gvl = Enum.find(result, &(&1.callsign == "K5GVL-10"))
      assert_in_delta k5gvl.lat, 33.1, 0.01
      assert_in_delta k5gvl.lng, -96.6, 0.01
    end

    test "returns empty list for empty input" do
      assert PreparedQueries.get_latest_positions_for_callsigns([]) == []
    end

    test "returns empty list for nonexistent callsigns" do
      result = PreparedQueries.get_latest_positions_for_callsigns(["NONEXIST-1", "FAKE-2"])
      assert result == []
    end

    test "is case-insensitive" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000")
        })

      result = PreparedQueries.get_latest_positions_for_callsigns(["k5gvl-10"])

      assert length(result) == 1
      assert hd(result).callsign == "K5GVL-10"
    end

    test "returns latest position when multiple packets exist for a callsign" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second)
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("34.0000"),
          lon: Decimal.new("-97.0000"),
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      result = PreparedQueries.get_latest_positions_for_callsigns(["K5GVL-10"])

      assert length(result) == 1
      position = hd(result)
      assert_in_delta position.lat, 34.0, 0.01
      assert_in_delta position.lng, -97.0, 0.01
    end
  end

  describe "get_nearby_weather_stations/4" do
    setup do
      # San Francisco center: 37.7749, -122.4194
      center_lat = 37.7749
      center_lon = -122.4194

      # Station A: ~0.5 miles north, weather 1h ago (should be included)
      {:ok, station_a} =
        create_positioned_packet(%{
          sender: "WX-A",
          base_callsign: "WX-A",
          ssid: "0",
          lat: Decimal.new("37.7822"),
          lon: Decimal.new("-122.4194"),
          temperature: 72.5,
          humidity: 65.0,
          pressure: 1013.25,
          wind_speed: 5.0,
          wind_direction: 180,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "Weather Station A",
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second)
        })

      # Station B: ~10 miles east, weather 2h ago (should be included)
      {:ok, station_b} =
        create_positioned_packet(%{
          sender: "WX-B-1",
          base_callsign: "WX-B",
          ssid: "1",
          lat: Decimal.new("37.7749"),
          lon: Decimal.new("-122.2700"),
          temperature: 68.0,
          humidity: 70.0,
          wind_gust: 12.0,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "Weather Station B",
          received_at: DateTime.add(DateTime.utc_now(), -7200, :second)
        })

      # Station C: ~50 miles south, weather 1h ago (should be excluded - outside radius)
      {:ok, _station_c} =
        create_positioned_packet(%{
          sender: "WX-C",
          base_callsign: "WX-C",
          ssid: "0",
          lat: Decimal.new("37.0500"),
          lon: Decimal.new("-122.4194"),
          temperature: 75.0,
          humidity: 60.0,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "Weather Station C",
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second)
        })

      # Station D: ~5 miles west, weather 8h ago (should be excluded - outside time window)
      {:ok, _station_d} =
        create_positioned_packet(%{
          sender: "WX-D",
          base_callsign: "WX-D",
          ssid: "0",
          lat: Decimal.new("37.7749"),
          lon: Decimal.new("-122.4900"),
          temperature: 70.0,
          humidity: 68.0,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "Weather Station D",
          received_at: DateTime.add(DateTime.utc_now(), -28_800, :second)
        })

      # Station E: ~5 miles southeast, no weather data (should be excluded)
      {:ok, _station_e} =
        create_positioned_packet(%{
          sender: "WX-E",
          base_callsign: "WX-E",
          ssid: "0",
          lat: Decimal.new("37.7400"),
          lon: Decimal.new("-122.3800"),
          symbol_table_id: "/",
          symbol_code: "-",
          comment: "Regular Station",
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second)
        })

      # Duplicate SSID for Station B (should be deduplicated)
      {:ok, _station_b2} =
        create_positioned_packet(%{
          sender: "WX-B-2",
          base_callsign: "WX-B",
          ssid: "2",
          lat: Decimal.new("37.7749"),
          lon: Decimal.new("-122.2700"),
          temperature: 69.0,
          humidity: 72.0,
          symbol_table_id: "/",
          symbol_code: "_",
          comment: "Weather Station B SSID 2",
          received_at: DateTime.add(DateTime.utc_now(), -1800, :second)
        })

      %{
        center_lat: center_lat,
        center_lon: center_lon,
        station_a: station_a,
        station_b: station_b
      }
    end

    test "returns nearby weather stations within radius and time window", %{
      center_lat: lat,
      center_lon: lon
    } do
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0)

      assert length(result) == 2
      callsigns = Enum.map(result, & &1.callsign)
      assert "WX-A" in callsigns
      assert "WX-B-2" in callsigns
    end

    test "returns results ordered by distance (closest first)", %{center_lat: lat, center_lon: lon} do
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0)

      assert length(result) == 2
      # Station A (~0.5 miles) should be first
      assert hd(result).callsign == "WX-A"
      assert hd(result).distance_miles < 1.0

      # Station B (~8-10 miles) should be second
      second = Enum.at(result, 1)
      assert second.callsign == "WX-B-2"
      assert second.distance_miles > 7.0
      assert second.distance_miles < 10.0
    end

    test "excludes stations outside radius", %{center_lat: lat, center_lon: lon} do
      # Use 5 mile radius - should only get Station A
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 5.0)

      assert length(result) == 1
      assert hd(result).callsign == "WX-A"
    end

    test "excludes stations outside time window", %{center_lat: lat, center_lon: lon} do
      # Use 1 hour window - should get both A (1h ago) and B-2 (30min ago)
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0, hours: 1)

      assert length(result) == 2
      callsigns = Enum.map(result, & &1.callsign)
      assert "WX-A" in callsigns
      assert "WX-B-2" in callsigns
    end

    test "excludes stations without weather data", %{center_lat: lat, center_lon: lon} do
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0)

      callsigns = Enum.map(result, & &1.callsign)
      refute "WX-E" in callsigns
    end

    test "deduplicates by base_callsign and returns most recent", %{
      center_lat: lat,
      center_lon: lon
    } do
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0)

      base_callsigns = Enum.map(result, & &1.base_callsign)
      assert length(base_callsigns) == length(Enum.uniq(base_callsigns))

      # Should get WX-B-2 (most recent) not WX-B-1
      wx_b = Enum.find(result, &(&1.base_callsign == "WX-B"))
      assert wx_b.callsign == "WX-B-2"
    end

    test "respects limit option", %{center_lat: lat, center_lon: lon} do
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0, limit: 1)

      assert length(result) == 1
      assert hd(result).callsign == "WX-A"
    end

    test "returns all required fields", %{center_lat: lat, center_lon: lon} do
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0)

      station = hd(result)
      assert is_binary(station.callsign)
      assert is_binary(station.base_callsign)
      assert is_float(station.lat)
      assert is_float(station.lon)
      assert is_float(station.distance_miles)
      assert is_binary(station.symbol_table_id)
      assert is_binary(station.symbol_code)
      assert is_binary(station.comment)
      assert %DateTime{} = station.received_at

      # Weather fields (may be nil)
      assert is_nil(station.temperature) or is_float(station.temperature)
      assert is_nil(station.humidity) or is_float(station.humidity)
      assert is_nil(station.pressure) or is_float(station.pressure)
      assert is_nil(station.wind_speed) or is_float(station.wind_speed)
      assert is_nil(station.wind_direction) or is_integer(station.wind_direction)
      assert is_nil(station.wind_gust) or is_float(station.wind_gust)
      assert is_nil(station.rain_1h) or is_float(station.rain_1h)
      assert is_nil(station.rain_24h) or is_float(station.rain_24h)
      assert is_nil(station.rain_since_midnight) or is_float(station.rain_since_midnight)
    end

    test "returns empty list when no stations in radius", %{center_lat: _lat, center_lon: _lon} do
      # Use coordinates far from any station
      result = PreparedQueries.get_nearby_weather_stations(40.0, -100.0, 5.0)

      assert result == []
    end

    test "handles custom hours option", %{center_lat: lat, center_lon: lon} do
      # Use 3 hour window - should get both A and B
      result = PreparedQueries.get_nearby_weather_stations(lat, lon, 15.0, hours: 3)

      assert length(result) == 2
    end
  end
end
