defmodule Parser.MessageTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Message

  describe "parse/1" do
    test "returns nil for now" do
      assert Message.parse(":CALLSIGN:Hello World") == nil
    end

    property "returns nil for any string input (stub)" do
      check all s <- StreamData.string(:printable, min_length: 1, max_length: 30) do
        assert Message.parse(s) == nil
      end
    end
  end
end
