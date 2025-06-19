defmodule Parser.NMEATest do
  use ExUnit.Case, async: true

  alias Parser.NMEA
  alias Parser.Types.ParseError

  describe "parse/1" do
    test "returns not_implemented error for now" do
      assert %ParseError{error_code: :not_implemented} =
               NMEA.parse("$GPRMC,123456,A,4903.50,N,07201.75,W*6A")
    end
  end
end
