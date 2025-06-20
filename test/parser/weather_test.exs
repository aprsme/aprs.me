defmodule Parser.WeatherTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Weather

  describe "parse/1" do
    test "returns a map with :data_type => :weather for valid input" do
      result = Weather.parse("_12345678c000s000g000t000r000p000P000h00b00000")
      assert is_map(result)
      assert result[:data_type] == :weather
    end

    property "always returns a map with :data_type == :weather for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = Weather.parse(s)
        assert is_map(result)
        assert result[:data_type] == :weather
      end
    end
  end
end
