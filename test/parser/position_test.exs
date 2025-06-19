defmodule Parser.PositionTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Position
  alias Parser.Types.Position, as: PositionStruct

  describe "parse/1" do
    test "returns a Position struct for valid input" do
      result = Position.parse("4903.50N/07201.75W>Test position")
      assert %PositionStruct{} = result
      assert result.symbol_table_id == "/"
      assert result.symbol_code == ">"
      assert result.comment == "Test position"
    end

    test "returns nil or struct with nil lat/lon for invalid input" do
      for input <- ["", "invalidstring", "12345678N/123456789W"] do
        result = Position.parse(input)
        assert result == nil or match?(%PositionStruct{latitude: nil, longitude: nil}, result)
      end
    end
  end

  property "returns nil or struct with nil lat/lon for random invalid strings" do
    check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30),
              not String.match?(s, ~r/^\d{4}\.\d{2}[NS][\/\\]\d{5}\.\d{2}[EW].+/) do
      result = Position.parse(s)
      assert result == nil or match?(%PositionStruct{latitude: nil, longitude: nil}, result)
    end
  end

  describe "parse_aprs_position/2" do
    test "parses valid APRS lat/lon strings" do
      result = Position.parse_aprs_position("4903.50N", "07201.75W")

      if result.latitude == nil or result.longitude == nil do
        flunk("parse_aprs_position/2 returned nil for latitude or longitude")
      end

      assert_in_delta result.latitude, 49.05833333333333, 1.0e-10
      assert_in_delta result.longitude, -72.02916666666667, 1.0e-10
    end

    test "returns nils for invalid strings" do
      assert %{latitude: nil, longitude: nil} = Position.parse_aprs_position("bad", "data")
    end
  end

  describe "calculate_position_ambiguity/2" do
    test "returns correct ambiguity for no spaces" do
      assert Position.calculate_position_ambiguity("4903.50N", "07201.75W") == 0
    end

    test "returns correct ambiguity for one space in each string" do
      assert Position.calculate_position_ambiguity("49 3.50N", "07201.7 W") == 1
    end

    test "returns correct ambiguity for two spaces in each string" do
      assert Position.calculate_position_ambiguity("4  3.50N", "0720  .7W") == 2
    end
  end

  describe "count_spaces/1" do
    property "counts spaces correctly" do
      check all s <- StreamData.string(:ascii, min_length: 0, max_length: 20) do
        assert Position.count_spaces(s) == s |> String.graphemes() |> Enum.count(&(&1 == " "))
      end
    end
  end

  describe "parse_dao_extension/1" do
    test "parses valid DAO extension" do
      assert %{lat_dao: "A", lon_dao: "B", datum: "WGS84"} = Position.parse_dao_extension("!ABZ!")
    end

    test "returns nil for no DAO extension" do
      assert Position.parse_dao_extension("no dao here") == nil
    end
  end
end
