defmodule ParserUnitTest do
  use ExUnit.Case, async: true

  describe "parse_aprs_position/2 (via parse_position_without_timestamp)" do
    test "parses valid APRS lat/lon" do
      # 4903.50N/12311.12W>
      result = Parser.parse_position_without_timestamp("4903.50N/12311.12W>comment")
      assert Decimal.equal?(Decimal.round(result.latitude, 4), Decimal.new("49.0583"))
      assert Decimal.equal?(Decimal.round(result.longitude, 4), Decimal.new("-123.1853"))
    end

    test "returns nils for invalid lat/lon" do
      result = Parser.parse_position_without_timestamp("bad/12311.12W>comment")
      assert result.latitude == nil
      result = Parser.parse_position_without_timestamp("4903.50N/bad>comment")
      assert result.longitude == nil
    end
  end

  describe "decode_compressed_position/1 and convert_to_base91/1" do
    test "decodes compressed position and base91" do
      # 4 chars, all '!' (ASCII 33) should decode to 33
      assert Parser.convert_to_base91("!!!!") == 33
      # Use a valid compressed string: 1 + 4 + 4 + 1 + 2 + 2 + 1 = 15 bytes
      # Format: "/" <> lat(4) <> lon(4) <> sym(1) <> cs(2) <> comp(2) <> rest(1)
      bin = "/!!!!!!!!>abccx"
      pos = Parser.decode_compressed_position(bin)
      assert pos.latitude == 33
      assert pos.longitude == 33
      assert pos.symbol_code == ">"
    end
  end

  describe "parse_position_with_datetime_and_weather/7" do
    test "returns map with lat/lon and weather" do
      result =
        Parser.parse_position_with_datetime_and_weather(
          false,
          "201750z",
          "4916.45N",
          "/",
          "12311.12W",
          ">",
          "_12345678c000s000"
        )

      assert is_map(result)
      assert Decimal.equal?(Decimal.round(result.latitude, 4), Decimal.new("49.2742"))
      assert Decimal.equal?(Decimal.round(result.longitude, 4), Decimal.new("-123.1853"))
      assert result.weather != nil
      assert result.timestamp == "201750z"
    end
  end

  describe "parse_data/3 fallback branches" do
    test "raw_gps_ultimeter returns error map" do
      result =
        Parser.parse_data(:raw_gps_ultimeter, "", "$GPRMC,123456,A,4903.50,N,07201.75,W*6A")

      assert result.data_type == :raw_gps_ultimeter
      assert result.error != nil
    end

    test "df_report fallback" do
      result = Parser.parse_data(:df_report, "", "notdfsdata")
      assert result.data_type == :df_report
      assert result.df_data == "notdfsdata"
    end

    test "phg_data fallback" do
      result = Parser.parse_data(:phg_data, "", "notphgdata")
      assert is_map(result)
    end
  end

  describe "parse_position_without_timestamp/1 fallback" do
    test "malformed input returns malformed_position" do
      result = Parser.parse_position_without_timestamp("badinput")
      assert result.data_type == :malformed_position
      assert result.latitude == nil
      assert result.longitude == nil
    end
  end

  describe "parse_position_with_timestamp/3 fallback" do
    test "malformed input returns error map" do
      result = Parser.parse_position_with_timestamp(false, "badinput", :timestamped_position)
      assert result.data_type == :timestamped_position_error
      assert result.error =~ "Invalid timestamped position format"
    end
  end
end
