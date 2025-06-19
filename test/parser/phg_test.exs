defmodule Parser.PHGTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.PHG
  alias Parser.Types.ParseError

  describe "parse/1" do
    test "returns not_implemented error for now" do
      assert %ParseError{error_code: :not_implemented} = PHG.parse("#PHG2360")
    end

    property "always returns not_implemented error for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = PHG.parse(s)
        assert %ParseError{error_code: :not_implemented} = result
      end
    end
  end
end
