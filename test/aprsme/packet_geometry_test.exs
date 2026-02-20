defmodule Aprsme.PacketGeometryTest do
  use Aprsme.DataCase, async: true

  alias Aprsme.Packet

  describe "create_point/2" do
    test "creates point from valid float coordinates" do
      point = Packet.create_point(40.7128, -74.006)
      assert %Geo.Point{coordinates: {-74.006, 40.7128}, srid: 4326} = point
    end

    test "creates point from Decimal coordinates" do
      lat = Decimal.new("40.7128")
      lon = Decimal.new("-74.006")
      point = Packet.create_point(lat, lon)
      assert %Geo.Point{srid: 4326} = point
    end

    test "creates point from integer coordinates" do
      point = Packet.create_point(40, -74)
      assert %Geo.Point{coordinates: {-74, 40}, srid: 4326} = point
    end

    test "returns nil for invalid latitude" do
      assert is_nil(Packet.create_point(91.0, 0.0))
      assert is_nil(Packet.create_point(-91.0, 0.0))
    end

    test "returns nil for invalid longitude" do
      assert is_nil(Packet.create_point(0.0, 181.0))
      assert is_nil(Packet.create_point(0.0, -181.0))
    end

    test "returns nil for nil coordinates" do
      assert is_nil(Packet.create_point(nil, nil))
      assert is_nil(Packet.create_point(40.0, nil))
      assert is_nil(Packet.create_point(nil, -74.0))
    end

    test "handles boundary coordinates" do
      # North pole
      assert %Geo.Point{} = Packet.create_point(90.0, 0.0)
      # South pole
      assert %Geo.Point{} = Packet.create_point(-90.0, 0.0)
      # Date line
      assert %Geo.Point{} = Packet.create_point(0.0, 180.0)
      assert %Geo.Point{} = Packet.create_point(0.0, -180.0)
      # Equator/prime meridian
      point = Packet.create_point(0.0, 0.0)
      assert %Geo.Point{srid: 4326} = point
      assert point.coordinates == {0.0, 0.0}
    end

    test "returns nil for non-numeric types" do
      assert is_nil(Packet.create_point("40.0", "-74.0"))
      assert is_nil(Packet.create_point(:lat, :lon))
    end
  end

  describe "extract_coordinates/1" do
    test "extracts lat/lon from Geo.Point" do
      point = %Geo.Point{coordinates: {-74.006, 40.7128}, srid: 4326}
      assert {40.7128, -74.006} = Packet.extract_coordinates(point)
    end

    test "returns nil tuple for nil" do
      assert {nil, nil} = Packet.extract_coordinates(nil)
    end

    test "returns nil tuple for non-point input" do
      assert {nil, nil} = Packet.extract_coordinates(%{lat: 40.0, lon: -74.0})
      assert {nil, nil} = Packet.extract_coordinates("not a point")
    end
  end

  describe "lat/1" do
    test "returns latitude from packet with location" do
      packet = %Packet{location: %Geo.Point{coordinates: {-74.006, 40.7128}, srid: 4326}}
      assert Packet.lat(packet) == 40.7128
    end

    test "returns nil for packet without location" do
      packet = %Packet{location: nil}
      assert is_nil(Packet.lat(packet))
    end

    test "returns nil for non-packet" do
      assert is_nil(Packet.lat(nil))
      assert is_nil(Packet.lat(%{}))
    end
  end

  describe "lon/1" do
    test "returns longitude from packet with location" do
      packet = %Packet{location: %Geo.Point{coordinates: {-74.006, 40.7128}, srid: 4326}}
      assert Packet.lon(packet) == -74.006
    end

    test "returns nil for packet without location" do
      packet = %Packet{location: nil}
      assert is_nil(Packet.lon(packet))
    end

    test "returns nil for non-packet" do
      assert is_nil(Packet.lon(nil))
      assert is_nil(Packet.lon(%{}))
    end
  end
end
