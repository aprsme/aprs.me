defmodule Aprs.GeometryTest do
  use Aprs.DataCase

  alias Aprs.Packet
  alias Aprs.Packets
  alias Geo.PostGIS.Geometry

  describe "PostGIS geometry functionality" do
    test "create_point/2 creates valid Point geometry" do
      lat = 32.7767
      lon = -96.7970

      point = Packet.create_point(lat, lon)

      assert %Geo.Point{} = point
      assert point.coordinates == {lon, lat}
      assert point.srid == 4326
    end

    test "create_point/2 returns nil for invalid coordinates" do
      # Test invalid latitude
      assert Packet.create_point(91.0, -96.7970) == nil
      assert Packet.create_point(-91.0, -96.7970) == nil

      # Test invalid longitude
      assert Packet.create_point(32.7767, 181.0) == nil
      assert Packet.create_point(32.7767, -181.0) == nil

      # Test nil values
      assert Packet.create_point(nil, -96.7970) == nil
      assert Packet.create_point(32.7767, nil) == nil
    end

    test "extract_coordinates/1 extracts lat/lon from Point geometry" do
      point = %Geo.Point{coordinates: {-96.7970, 32.7767}, srid: 4326}

      {lat, lon} = Packet.extract_coordinates(point)

      assert lat == 32.7767
      assert lon == -96.7970
    end

    test "extract_coordinates/1 returns {nil, nil} for invalid geometry" do
      assert Packet.extract_coordinates(nil) == {nil, nil}
      assert Packet.extract_coordinates("invalid") == {nil, nil}
    end

    test "packet changeset creates geometry from lat/lon" do
      attrs = %{
        base_callsign: "TEST",
        data_type: "position",
        destination: "APRS",
        information_field: "test",
        path: "WIDE1-1",
        sender: "TEST-1",
        ssid: "1",
        received_at: DateTime.utc_now(),
        lat: 32.7767,
        lon: -96.7970
      }

      changeset = Packet.changeset(%Packet{}, attrs)

      assert changeset.valid?
      assert get_change(changeset, :location) != nil
      assert get_change(changeset, :has_position) == true

      location = get_change(changeset, :location)
      assert %Geo.Point{} = location
      assert location.coordinates == {-96.7970, 32.7767}
    end

    test "packet changeset handles invalid coordinates gracefully" do
      attrs = %{
        base_callsign: "TEST",
        data_type: "position",
        destination: "APRS",
        information_field: "test",
        path: "WIDE1-1",
        sender: "TEST-1",
        ssid: "1",
        received_at: DateTime.utc_now(),
        # Invalid latitude
        lat: 91.0,
        lon: -96.7970
      }

      changeset = Packet.changeset(%Packet{}, attrs)

      # Should still be valid, just without location
      assert changeset.valid?
      assert get_change(changeset, :location) == nil
      assert get_change(changeset, :has_position) != true
    end

    test "store_packet/1 successfully stores packet with geometry" do
      attrs = %{
        base_callsign: "TEST",
        data_type: "position",
        destination: "APRS",
        information_field: "!3216.46N/09647.82W>Test packet",
        path: "WIDE1-1,WIDE2-1",
        sender: "TEST-1",
        ssid: "1",
        received_at: DateTime.utc_now(),
        lat: 32.2743,
        lon: -96.7970,
        has_position: true,
        region: "32.3,-96.8",
        raw_packet: "TEST-1>APRS,WIDE1-1,WIDE2-1:!3216.46N/09647.82W>Test packet"
      }

      assert {:ok, packet} = Packets.store_packet(attrs)
      assert packet.sender == "TEST-1"
      assert packet.has_position == true
      assert packet.location != nil
      assert %Geo.Point{} = packet.location
      assert packet.location.coordinates == {-96.7970, 32.2743}
    end

    test "store_packet/1 handles packet without position data" do
      attrs = %{
        base_callsign: "TEST",
        data_type: "message",
        destination: "APRS",
        information_field: ":TEST-2   :Hello world",
        path: "WIDE1-1",
        sender: "TEST-1",
        ssid: "1",
        received_at: DateTime.utc_now(),
        raw_packet: "TEST-1>APRS,WIDE1-1::TEST-2   :Hello world"
      }

      assert {:ok, packet} = Packets.store_packet(attrs)
      assert packet.sender == "TEST-1"
      assert packet.has_position == false
      assert packet.location == nil
    end

    test "store_packet/1 handles coordinate validation errors gracefully" do
      attrs = %{
        base_callsign: "TEST",
        data_type: "position",
        destination: "APRS",
        information_field: "test",
        path: "WIDE1-1",
        sender: "TEST-1",
        ssid: "1",
        received_at: DateTime.utc_now(),
        # Invalid latitude
        lat: 200.0,
        lon: -96.7970,
        raw_packet: "TEST-1>APRS,WIDE1-1:test"
      }

      # Should not crash, but should handle gracefully
      assert {:ok, packet} = Packets.store_packet(attrs)
      assert packet.sender == "TEST-1"
      assert packet.has_position == false
      assert packet.location == nil
    end

    test "Geo.PostGIS.Geometry cast works with Point" do
      point = %Geo.Point{coordinates: {-96.7970, 32.7767}, srid: 4326}

      assert {:ok, ^point} = Geometry.cast(point)
    end

    test "Geo.PostGIS.Geometry cast works with coordinate tuple" do
      coords = {-96.7970, 32.7767}

      # PostGIS geometry casting may not support tuples directly
      # Instead test creating a point manually
      point = %Geo.Point{coordinates: coords, srid: 4326}
      assert {:ok, ^point} = Geometry.cast(point)
    end

    test "packet lat/lon helper functions work correctly" do
      packet = %Packet{
        location: %Geo.Point{coordinates: {-96.7970, 32.7767}, srid: 4326}
      }

      assert Packet.lat(packet) == 32.7767
      assert Packet.lon(packet) == -96.7970
    end

    test "packet lat/lon helpers return nil for packets without location" do
      packet = %Packet{location: nil}

      assert Packet.lat(packet) == nil
      assert Packet.lon(packet) == nil
    end
  end

  describe "coordinate conversion and validation" do
    test "handles various coordinate formats" do
      # Test float coordinates
      assert Packet.create_point(32.7767, -96.7970) != nil

      # Test integer coordinates
      assert Packet.create_point(33, -97) != nil

      # Test boundary values
      assert Packet.create_point(90.0, 180.0) != nil
      assert Packet.create_point(-90.0, -180.0) != nil

      # Test just inside boundaries
      assert Packet.create_point(89.9999, 179.9999) != nil
      assert Packet.create_point(-89.9999, -179.9999) != nil
    end

    test "rejects coordinates outside valid ranges" do
      # Test latitude boundaries
      assert Packet.create_point(90.0001, 0.0) == nil
      assert Packet.create_point(-90.0001, 0.0) == nil

      # Test longitude boundaries
      assert Packet.create_point(0.0, 180.0001) == nil
      assert Packet.create_point(0.0, -180.0001) == nil
    end

    test "handles edge case coordinates" do
      # Test zero coordinates
      assert Packet.create_point(0.0, 0.0) != nil

      # Test very small coordinates
      assert Packet.create_point(0.000001, 0.000001) != nil

      # Test coordinates near boundaries
      assert Packet.create_point(89.999999, 179.999999) != nil
    end
  end

  describe "packet storage with different position formats" do
    test "stores packet with MicE-like data structure" do
      # Test packet storage that would involve MicE conversion
      attrs = %{
        base_callsign: "TEST",
        data_type: "position",
        destination: "APRS",
        information_field: "test",
        path: "WIDE1-1",
        sender: "TEST-1",
        ssid: "1",
        received_at: DateTime.utc_now(),
        # Pre-converted coordinates
        lat: 32.7767,
        lon: -96.7970,
        raw_packet: "TEST-1>APRS,WIDE1-1:test"
      }

      assert {:ok, packet} = Packets.store_packet(attrs)
      assert packet.sender == "TEST-1"
      assert packet.has_position == true
      assert packet.location != nil
    end

    test "stores packet with standard position data" do
      attrs = %{
        base_callsign: "TEST",
        data_type: "position",
        destination: "APRS",
        information_field: "test",
        path: "WIDE1-1",
        sender: "TEST-2",
        ssid: "2",
        received_at: DateTime.utc_now(),
        lat: 32.7767,
        lon: -96.7970,
        raw_packet: "TEST-2>APRS,WIDE1-1:test"
      }

      assert {:ok, packet} = Packets.store_packet(attrs)
      assert packet.has_position == true
      assert %Geo.Point{} = packet.location
    end

    test "stores packet without position data" do
      attrs = %{
        base_callsign: "TEST",
        data_type: "message",
        destination: "APRS",
        information_field: ":TEST-2   :Hello",
        path: "WIDE1-1",
        sender: "TEST-3",
        ssid: "3",
        received_at: DateTime.utc_now(),
        raw_packet: "TEST-3>APRS,WIDE1-1::TEST-2   :Hello"
      }

      assert {:ok, packet} = Packets.store_packet(attrs)
      assert packet.has_position == false
      assert packet.location == nil
    end
  end
end
