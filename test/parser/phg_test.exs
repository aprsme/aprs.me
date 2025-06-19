defmodule Parser.PHGTest do
  use ExUnit.Case, async: true

  alias Parser.PHG
  alias Parser.Types.ParseError

  describe "parse/1" do
    test "returns not_implemented error for now" do
      assert %ParseError{error_code: :not_implemented} = PHG.parse("#PHG2360")
    end
  end
end
