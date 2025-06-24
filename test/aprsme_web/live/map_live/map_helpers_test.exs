defmodule AprsmeWeb.MapLive.MapHelpersTest do
  use ExUnit.Case, async: true

  alias Aprs.Types.MicE
  alias AprsmeWeb.MapLive.MapHelpers

  describe "get_coordinates/1" do
    test "returns lat/lon/data_extended for map with lat/lon" do
      packet = %{lat: 10.0, lon: 20.0, data_extended: %{foo: :bar}}
      assert MapHelpers.get_coordinates(packet) == {10.0, 20.0, %{foo: :bar}}
    end

    test "returns lat/lon/data_extended for map with latitude/longitude in data_extended" do
      packet = %{data_extended: %{latitude: 11.1, longitude: 22.2}}

      assert MapHelpers.get_coordinates(packet) ==
               {11.1, 22.2, %{latitude: 11.1, longitude: 22.2}}
    end

    test "returns lat/lon/mic_e for MicE struct" do
      mic_e = %MicE{
        lat_degrees: 12,
        lat_minutes: 34,
        lat_fractional: 0,
        lat_direction: :north,
        lon_degrees: 56,
        lon_minutes: 78,
        lon_fractional: 0,
        lon_direction: :east
      }

      packet = %{data_extended: mic_e}
      {lat, lon, ext} = MapHelpers.get_coordinates(packet)
      assert is_number(lat) and is_number(lon)
      assert ext == mic_e
    end

    test "returns {nil, nil, nil} for missing data" do
      assert MapHelpers.get_coordinates(%{}) == {nil, nil, nil}
    end
  end

  describe "get_coordinates_from_mic_e/1" do
    test "returns correct lat/lon for valid MicE" do
      mic_e = %MicE{
        lat_degrees: 10,
        lat_minutes: 30,
        lat_fractional: 0,
        lat_direction: :north,
        lon_degrees: 20,
        lon_minutes: 40,
        lon_fractional: 0,
        lon_direction: :east
      }

      {lat, lon} = MapHelpers.get_coordinates_from_mic_e(mic_e)
      assert_in_delta lat, 10.5, 0.0001
      assert_in_delta lon, 20.6667, 0.0001
    end

    test "returns {nil, nil} for out-of-bounds" do
      mic_e = %MicE{
        lat_degrees: 100,
        lat_minutes: 0,
        lat_fractional: 0,
        lat_direction: :north,
        lon_degrees: 200,
        lon_minutes: 0,
        lon_fractional: 0,
        lon_direction: :east
      }

      assert MapHelpers.get_coordinates_from_mic_e(mic_e) == {nil, nil}
    end
  end

  describe "has_position_data?/1" do
    test "true for MicE in data_extended" do
      mic_e = %MicE{
        lat_degrees: 1,
        lat_minutes: 1,
        lat_fractional: 0,
        lat_direction: :north,
        lon_degrees: 1,
        lon_minutes: 1,
        lon_fractional: 0,
        lon_direction: :east
      }

      assert MapHelpers.has_position_data?(%{data_extended: mic_e})
    end

    test "true for latitude/longitude in data_extended" do
      assert MapHelpers.has_position_data?(%{data_extended: %{latitude: 1, longitude: 2}})
    end

    test "true for lat/lon at top level" do
      assert MapHelpers.has_position_data?(%{lat: 1, lon: 2})
    end

    test "false for missing position" do
      refute MapHelpers.has_position_data?(%{})
    end
  end

  describe "within_bounds?/2" do
    test "true for point in bounds" do
      bounds = %{north: 10, south: 0, east: 10, west: 0}
      assert MapHelpers.within_bounds?(%{lat: 5, lon: 5}, bounds)
    end

    test "false for point out of bounds" do
      bounds = %{north: 10, south: 0, east: 10, west: 0}
      refute MapHelpers.within_bounds?(%{lat: 15, lon: 5}, bounds)
    end

    test "true for tuple input" do
      bounds = %{north: 10, south: 0, east: 10, west: 0}
      assert MapHelpers.within_bounds?({5, 5}, bounds)
    end

    test "false for nil input" do
      bounds = %{north: 10, south: 0, east: 10, west: 0}
      refute MapHelpers.within_bounds?(%{}, bounds)
    end
  end
end
