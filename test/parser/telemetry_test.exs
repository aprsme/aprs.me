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

    test "parses T# telemetry string with analog and digital values" do
      result = Telemetry.parse("T#123,1,2,3,4,5,1,0,1,0,1,0,1")
      assert is_map(result)
      assert result[:data_type] == :telemetry
      assert result[:sequence_number] == 123
      assert is_list(result[:analog_values])
      assert is_list(result[:digital_values])
    end

    test "parses :PARM. telemetry parameter names" do
      result = Telemetry.parse(":PARM.A,B,C,D,E")
      assert result[:data_type] == :telemetry_parameters
      assert result[:parameter_names] == ["A", "B", "C", "D", "E"]
    end

    test "parses :EQNS. telemetry equations" do
      result = Telemetry.parse(":EQNS.1,2,3,4,5,6,7,8,9")
      assert result[:data_type] == :telemetry_equations
      assert is_list(result[:equations])
      assert Enum.all?(result[:equations], &is_map/1)
    end

    test "parses :UNIT. telemetry units" do
      result = Telemetry.parse(":UNIT.V,A,degC")
      assert result[:data_type] == :telemetry_units
      assert result[:units] == ["V", "A", "degC"]
    end

    test "parses :BITS. telemetry bits sense and project names" do
      result = Telemetry.parse(":BITS.101010,foo,bar")
      assert result[:data_type] == :telemetry_bits
      assert is_list(result[:bits_sense])
      assert result[:project_names] == ["foo", "bar"]
    end

    test "parses :BITS. with no project names" do
      result = Telemetry.parse(":BITS.101010")
      assert result[:data_type] == :telemetry_bits
      assert is_list(result[:bits_sense])
      assert result[:project_names] == []
    end

    test "parses unknown/fallback data" do
      result = Telemetry.parse("random data")
      assert result[:data_type] == :telemetry
      assert result[:raw_data] == "random data"
    end
  end
end
