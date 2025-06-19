defmodule Parser.WeatherTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Weather

  describe "parse/1" do
    test "returns nil for now" do
      assert Weather.parse("_12345678c000s000g000t000r000p000P000h00b00000") == nil
    end

    property "always returns nil for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        assert Weather.parse(s) == nil
      end
    end
  end
end
