defmodule Parser.CompressedTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Compressed
  alias Parser.Types.ParseError

  describe "parse/1" do
    test "returns not_implemented error for now" do
      assert %ParseError{error_code: :not_implemented} = Compressed.parse("/5L!!<*e7>7P[")
    end

    property "always returns not_implemented error for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = Compressed.parse(s)
        assert %ParseError{error_code: :not_implemented} = result
      end
    end
  end
end
