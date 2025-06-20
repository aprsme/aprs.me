defmodule Parser.TelemetryTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Telemetry

  describe "parse/1" do
    test "returns a map with :data_type for valid input" do
      result = Telemetry.parse("T#123,456,789,012,345,678,901,234")
      assert is_map(result)
      assert Map.has_key?(result, :data_type)
    end

    property "always returns a map with :data_type for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = Telemetry.parse(s)
        assert is_map(result)
        assert Map.has_key?(result, :data_type)
      end
    end
  end
end
