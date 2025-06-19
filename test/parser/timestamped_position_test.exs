defmodule Parser.TimestampedPositionTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.TimestampedPosition

  describe "parse/1" do
    test "returns nil for now" do
      assert TimestampedPosition.parse("/123456h4903.50N/07201.75W>Test timestamped position") ==
               nil
    end

    property "always returns nil for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        assert TimestampedPosition.parse(s) == nil
      end
    end
  end
end
