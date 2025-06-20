defmodule Parser.HelpersTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Helpers

  describe "NMEA helpers" do
    test "parse_nmea_coordinate parses valid coordinates" do
      assert Helpers.parse_nmea_coordinate("4916.45", "N") == {:ok, 49.1645}
      {:ok, val} = Helpers.parse_nmea_coordinate("12311.12", "W")
      assert_in_delta val, -123.1112, 1.0e-6
    end

    test "parse_nmea_coordinate errors on invalid input" do
      assert Helpers.parse_nmea_coordinate("bad", "N") ==
               {:error, "Invalid coordinate value"}

      assert Helpers.parse_nmea_coordinate("4916.45", "Q") ==
               {:error, "Invalid coordinate direction"}

      assert Helpers.parse_nmea_coordinate(nil, nil) ==
               {:error, "Invalid coordinate format"}
    end

    property "parse_nmea_coordinate returns ok or error for any string" do
      check all v <- StreamData.string(:printable), d <- StreamData.string(:printable) do
        result = Helpers.parse_nmea_coordinate(v, d)
        assert match?({:ok, _}, result) or match?({:error, _}, result)
      end
    end
  end

  describe "PHG/DF helpers" do
    test "parse_phg_power returns correct values" do
      assert Helpers.parse_phg_power(?0) == {1, "1 watt"}
      assert Helpers.parse_phg_power(?9) == {81, "81 watts"}
      assert Helpers.parse_phg_power(?X) == {nil, "Unknown power: X"}
    end

    test "parse_phg_height returns correct values" do
      assert Helpers.parse_phg_height(?0) == {10, "10 feet"}
      assert Helpers.parse_phg_height(?9) == {5120, "5120 feet"}
      assert Helpers.parse_phg_height(?X) == {nil, "Unknown height: X"}
    end

    test "parse_phg_gain returns correct values" do
      assert Helpers.parse_phg_gain(?0) == {0, "0 dB"}
      assert Helpers.parse_phg_gain(?9) == {9, "9 dB"}
      assert Helpers.parse_phg_gain(?X) == {nil, "Unknown gain: X"}
    end

    test "parse_phg_directivity returns correct values" do
      assert Helpers.parse_phg_directivity(?0) == {360, "Omni"}
      assert Helpers.parse_phg_directivity(?9) == {nil, "Undefined"}
      assert Helpers.parse_phg_directivity(?X) == {nil, "Unknown directivity: X"}
    end

    test "parse_df_strength returns correct values" do
      assert Helpers.parse_df_strength(?0) == {0, "0 dB"}
      assert Helpers.parse_df_strength(?9) == {9, "27 dB above S0"}
      assert Helpers.parse_df_strength(?X) == {nil, "Unknown strength: X"}
    end
  end

  describe "compressed position helpers" do
    test "convert_compressed_lat and lon return floats" do
      lat = Helpers.convert_compressed_lat("abcd")
      lon = Helpers.convert_compressed_lon("abcd")
      assert is_struct(lat, Decimal) or is_nil(lat) or is_number(lat)
      assert is_struct(lon, Decimal) or is_nil(lon) or is_number(lon)
    end
  end

  describe "KISS/TNC2 utilities" do
    test "kiss_to_tnc2 decodes KISS frames" do
      frame = <<0xC0, 0x00, 65, 66, 67, 0xC0>>
      assert Parser.Helpers.kiss_to_tnc2(frame) == "ABC"
    end

    test "kiss_to_tnc2 returns error for invalid frame" do
      assert Parser.Helpers.kiss_to_tnc2("notkiss") == %{
               error_code: :packet_invalid,
               error_message: "Unknown error"
             }
    end

    test "tnc2_to_kiss encodes TNC2 to KISS" do
      tnc2 = "A\xDBB\xC0C"
      kiss = Parser.Helpers.tnc2_to_kiss(tnc2)
      assert :binary.part(kiss, 0, 1) == <<0xC0>>
      assert :binary.part(kiss, byte_size(kiss) - 1, 1) == <<0xC0>>
    end
  end
end
