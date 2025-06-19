defmodule Parser.ObjectTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Object

  describe "parse/1" do
    test "returns nil for now" do
      assert Object.parse(";OBJECT*111111z4903.50N/07201.75W>Test object") == nil
    end

    property "always returns nil for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        assert Object.parse(s) == nil
      end
    end
  end
end
