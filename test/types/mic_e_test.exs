defmodule Parser.Types.MicETest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Types.MicE

  describe "Access behavior" do
    test "fetch/2 returns calculated latitude and longitude" do
      mic_e = %MicE{
        lat_degrees: 40,
        lat_minutes: 30,
        lat_direction: :north,
        lon_degrees: 74,
        lon_minutes: 15,
        lon_direction: :west
      }

      assert {:ok, 40.5} = MicE.fetch(mic_e, :latitude)
      assert {:ok, -74.25} = MicE.fetch(mic_e, :longitude)
    end

    test "fetch/2 returns :error for missing components" do
      mic_e = %MicE{
        lat_degrees: nil,
        lat_minutes: 30,
        lat_direction: :north,
        lon_degrees: nil,
        lon_minutes: 15,
        lon_direction: :west
      }

      assert :error = MicE.fetch(mic_e, :latitude)
      assert :error = MicE.fetch(mic_e, :longitude)
    end

    test "fetch/2 falls back to struct field" do
      mic_e = %MicE{message_code: "M01"}
      assert {:ok, "M01"} = MicE.fetch(mic_e, :message_code)
    end

    test "fetch/2 handles string keys" do
      mic_e = %MicE{message_code: "M01"}
      assert {:ok, "M01"} = MicE.fetch(mic_e, "message_code")
      assert :error = MicE.fetch(mic_e, "not_a_field")
    end
  end

  describe "get_and_update/3 and pop/2" do
    test "get_and_update/3 updates a field" do
      mic_e = %MicE{message: "foo"}
      {old, updated} = MicE.get_and_update(mic_e, :message, fn val -> {val, "bar"} end)
      assert old == "foo"
      assert updated.message == "bar"
    end

    test "get_and_update/3 with :pop" do
      mic_e = %MicE{message: "foo"}
      {old, updated} = MicE.get_and_update(mic_e, :message, fn _val -> :pop end)
      assert old == "foo"
      assert updated.message == nil
    end

    test "pop/2 removes a field" do
      mic_e = %MicE{message: "foo"}
      {val, updated} = MicE.pop(mic_e, :message)
      assert val == "foo"
      assert updated.message == nil
    end

    test "pop/2 with string key" do
      mic_e = %MicE{message: "foo"}
      {val, updated} = MicE.pop(mic_e, "message")
      assert val == "foo"
      assert updated.message == nil
    end

    test "pop/2 with non-existent string key" do
      mic_e = %MicE{message: "foo"}
      {val, updated} = MicE.pop(mic_e, "not_a_field")
      assert val == nil
      assert updated == mic_e
    end
  end

  property "fetch/2 for latitude/longitude returns correct sign for direction" do
    check all deg <- StreamData.integer(0..90),
              min <- StreamData.integer(0..59),
              lat_dir <- StreamData.member_of([:north, :south]),
              lon_dir <- StreamData.member_of([:east, :west]),
              lon_deg <- StreamData.integer(0..180),
              lon_min <- StreamData.integer(0..59) do
      mic_e = %MicE{
        lat_degrees: abs(deg),
        lat_minutes: abs(min),
        lat_direction: lat_dir,
        lon_degrees: abs(lon_deg),
        lon_minutes: abs(lon_min),
        lon_direction: lon_dir
      }

      {:ok, lat} = MicE.fetch(mic_e, :latitude)
      {:ok, lon} = MicE.fetch(mic_e, :longitude)

      if lat_dir == :south do
        assert lat <= 0,
               "lat_dir: #{inspect(lat_dir)}, lat: #{inspect(lat)}, mic_e: #{inspect(mic_e)}"
      else
        assert lat >= 0,
               "lat_dir: #{inspect(lat_dir)}, lat: #{inspect(lat)}, mic_e: #{inspect(mic_e)}"
      end

      if lon_dir == :west do
        assert lon <= 0,
               "lon_dir: #{inspect(lon_dir)}, lon: #{inspect(lon)}, mic_e: #{inspect(mic_e)}"
      else
        assert lon >= 0,
               "lon_dir: #{inspect(lon_dir)}, lon: #{inspect(lon)}, mic_e: #{inspect(mic_e)}"
      end
    end
  end
end
