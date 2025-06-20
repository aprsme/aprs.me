defmodule Parser.ItemTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Item

  describe "parse/1" do
    test "returns a map with :data_type => :item for valid input" do
      result = Item.parse(")ITEM!4903.50N/07201.75W>Test item")
      assert is_map(result)
      assert result[:data_type] == :item
    end

    property "always returns a map with :data_type == :item for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = Item.parse(s)
        assert is_map(result)
        assert result[:data_type] == :item
      end
    end
  end
end
