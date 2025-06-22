defmodule Parser.ObjectTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Object

  describe "parse/1" do
    test "returns a map with :data_type => :object for valid input" do
      result = Object.parse(";OBJECT*111111z4903.50N/07201.75W>Test object")
      assert is_map(result)
      assert result[:data_type] == :object
    end

    property "always returns a map with :data_type == :object for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = Object.parse(s)
        assert is_map(result)
        assert result[:data_type] == :object
      end
    end

    test "parses uncompressed object position" do
      # 9-char name, 1 live/killed, 7 timestamp, 8 lat, 1 sym_table, 9 lon, 1 sym_code, comment
      data = ";OBJECTNAM*1234567" <> "4903.50N/" <> "07201.75W" <> ">Test object"
      result = Object.parse(data)
      assert is_map(result)
      assert result[:data_type] == :object
      assert result[:position_format] == :uncompressed
      assert result[:latitude] != nil
      assert result[:longitude] != nil
    end

    test "parses compressed object position" do
      # 9-char name, 1 live/killed, 7 timestamp, compressed position
      data = ";OBJECTNAM*1234567/abcdabcd>12!cTest compressed"
      result = Object.parse(data)
      assert is_map(result)
      assert result[:data_type] == :object
      assert result[:position_format] == :compressed
    end

    test "parses unknown/fallback object position" do
      data = ";OBJECTNAM*1234567unknownformat"
      result = Object.parse(data)
      assert is_map(result)
      assert result[:data_type] == :object
      assert result[:position_format] == :unknown
    end

    test "parses fallback/other data" do
      data = "not an object"
      result = Object.parse(data)
      assert is_map(result)
      assert result[:data_type] == :object
      assert result[:raw_data] == data
    end
  end
end
