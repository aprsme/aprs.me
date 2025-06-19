defmodule Parser.CoreTest do
  use ExUnit.Case, async: true

  alias Parser.Core
  alias Parser.Types.ParseError

  describe "parse/1" do
    test "returns not_implemented error for now" do
      assert {:error, %ParseError{error_code: :not_implemented}} = Core.parse("test packet")
    end
  end
end
