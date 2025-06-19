defmodule Parser.CompressedTest do
  use ExUnit.Case, async: true

  alias Parser.Compressed
  alias Parser.Types.ParseError

  describe "parse/1" do
    test "returns not_implemented error for now" do
      assert %ParseError{error_code: :not_implemented} = Compressed.parse("/5L!!<*e7>7P[")
    end
  end
end
