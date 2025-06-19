defmodule Parser.TelemetryTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Telemetry

  describe "parse/1" do
    test "returns nil for now" do
      assert Telemetry.parse("T#123,456,789,012,345,678,901,234") == nil
    end

    property "always returns nil for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        assert Telemetry.parse(s) == nil
      end
    end
  end
end
