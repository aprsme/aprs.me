defmodule Parser.StatusTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Status

  describe "parse/1" do
    test "returns nil for now" do
      assert Status.parse(">Test status message") == nil
    end

    property "always returns nil for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        assert Status.parse(s) == nil
      end
    end
  end
end
