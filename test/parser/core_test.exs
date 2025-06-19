defmodule Parser.CoreTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Core
  alias Parser.Types.ParseError

  describe "parse/1" do
    test "returns not_implemented error for now" do
      assert {:error, %ParseError{error_code: :not_implemented}} =
               Core.parse("SOME_PACKET_STRING")
    end

    property "always returns not_implemented error for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = Core.parse(s)
        assert {:error, %ParseError{error_code: :not_implemented}} = result
      end
    end
  end
end
