defmodule Parser.AX25Test do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.AX25

  describe "parse_callsign/1" do
    test "parses callsign with dash" do
      assert {:ok, {"CALL", "1"}} = AX25.parse_callsign("CALL-1")
    end

    test "parses callsign with multiple dashes" do
      assert {:ok, {"CALL-1-EXTRA", "0"}} = AX25.parse_callsign("CALL-1-EXTRA")
    end

    test "parses callsign without dash" do
      assert {:ok, {"CALL", "0"}} = AX25.parse_callsign("CALL")
    end

    test "returns error for empty string" do
      assert {:error, _} = AX25.parse_callsign("")
    end

    test "returns error for non-binary input" do
      assert {:error, _} = AX25.parse_callsign(123)
    end

    property "parses any ASCII string as callsign or returns error" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 10) do
        result = AX25.parse_callsign(s)
        assert match?({:ok, {_base, _ssid}}, result) or match?({:error, _}, result)
      end
    end
  end

  describe "parse_path/1" do
    test "returns error for any input (stub)" do
      assert {:error, _} = AX25.parse_path("WIDE1-1,WIDE2-2")
      assert {:error, _} = AX25.parse_path("")
    end

    property "always returns error for any string (stub)" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 20) do
        assert match?({:error, _}, AX25.parse_path(s))
      end
    end
  end
end
