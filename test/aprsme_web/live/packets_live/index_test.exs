defmodule AprsmeWeb.PacketsLive.IndexTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.PacketsLive.Index

  describe "extract_coordinate/2" do
    test "extracts lat from direct :lat key" do
      packet = %{lat: 35.123456, lon: -75.654321}
      assert Index.extract_coordinate(packet, :lat) == 35.123456
    end

    test "extracts lon from direct :lon key" do
      packet = %{lat: 35.0, lon: -75.654321}
      assert Index.extract_coordinate(packet, :lon) == -75.654321
    end

    test "extracts lat from Packet struct with location" do
      packet = %Aprsme.Packet{location: %Geo.Point{coordinates: {-75.0, 35.0}, srid: 4326}}
      assert Index.extract_coordinate(packet, :lat) == 35.0
    end

    test "extracts lon from Packet struct with location" do
      packet = %Aprsme.Packet{location: %Geo.Point{coordinates: {-75.0, 35.0}, srid: 4326}}
      assert Index.extract_coordinate(packet, :lon) == -75.0
    end

    test "extracts lat from data_extended latitude" do
      packet = %{data_extended: %{latitude: 35.5, longitude: -75.5}}
      assert Index.extract_coordinate(packet, :lat) == 35.5
    end

    test "extracts lon from data_extended longitude" do
      packet = %{data_extended: %{latitude: 35.5, longitude: -75.5}}
      assert Index.extract_coordinate(packet, :lon) == -75.5
    end

    test "returns nil for missing data" do
      assert Index.extract_coordinate(%{}, :lat) == nil
      assert Index.extract_coordinate(%{}, :lon) == nil
    end

    test "prefers direct keys over data_extended" do
      packet = %{lat: 10.0, lon: 20.0, data_extended: %{latitude: 30.0, longitude: 40.0}}
      assert Index.extract_coordinate(packet, :lat) == 10.0
      assert Index.extract_coordinate(packet, :lon) == 20.0
    end
  end

  describe "format_coordinate/1" do
    test "formats float to 6 decimal places" do
      assert Index.format_coordinate(35.123456789) == "35.123457"
    end

    test "formats integer" do
      assert Index.format_coordinate(35) == "35"
    end

    test "truncates long binary coordinate" do
      assert Index.format_coordinate("35.12345678901") == "35.123456"
    end

    test "passes through short binary" do
      assert Index.format_coordinate("35.12") == "35.12"
    end

    test "returns empty string for nil" do
      assert Index.format_coordinate(nil) == ""
    end
  end
end
