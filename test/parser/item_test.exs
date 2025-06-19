defmodule Parser.ItemTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Item

  describe "parse/1" do
    test "returns nil for now" do
      assert Item.parse(")ITEM!4903.50N/07201.75W>Test item") == nil
    end

    property "always returns nil for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        assert Item.parse(s) == nil
      end
    end
  end
end
