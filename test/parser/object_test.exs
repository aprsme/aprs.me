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
  end
end
