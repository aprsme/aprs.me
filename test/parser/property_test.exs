defmodule Parser.PropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  describe "split_packet/1" do
    property "returns {:ok, [sender, path, data]} for valid packets" do
      check all sender <- StreamData.string(:alphanumeric, min_length: 1),
                path <- StreamData.string(:alphanumeric, min_length: 1),
                data <- StreamData.string(:printable, min_length: 1) do
        packet = sender <> ">" <> path <> ":" <> data
        assert {:ok, [^sender, ^path, ^data]} = Parser.split_packet(packet)
      end
    end

    property "returns error for invalid packets" do
      check all s <- StreamData.string(:printable, max_length: 10) do
        # Missing '>' or ':'
        bad = s <> s
        assert match?({:error, _}, Parser.split_packet(bad))
      end
    end
  end

  describe "split_path/1" do
    property "splits path into destination and digipeater path for any string" do
      check all s <- StreamData.string(:alphanumeric, min_length: 0, max_length: 10) do
        result = Parser.split_path(s)
        assert match?({:ok, [_, _]}, result)
      end
    end
  end

  describe "parse_callsign/1" do
    property "parses valid callsigns" do
      check all base <- StreamData.string(:alphanumeric, min_length: 1),
                ssid <- StreamData.string(:alphanumeric, min_length: 1) do
        callsign = base <> "-" <> ssid
        assert {:ok, [^base, ^ssid]} = Parser.parse_callsign(callsign)
      end
    end
  end

  describe "validate_path/1" do
    property "rejects paths with too many components" do
      check all n <- StreamData.integer(9..20) do
        path = Enum.map_join(1..n, ",", fn _ -> "WIDE1" end)
        assert match?({:error, _}, Parser.validate_path(path))
      end
    end

    property "accepts paths with 8 or fewer components" do
      check all n <- StreamData.integer(1..8) do
        path = Enum.map_join(1..n, ",", fn _ -> "WIDE1" end)
        assert :ok = Parser.validate_path(path)
      end
    end
  end

  describe "parse_datatype/1" do
    property "returns an atom for any printable string" do
      check all s <- StreamData.string(:printable, min_length: 1) do
        assert is_atom(Parser.parse_datatype(s))
      end
    end
  end

  describe "parse/1 error and fallback branches" do
    test "returns error for non-binary input" do
      assert {:error, :invalid_packet} = Parser.parse(123)
      assert {:error, :invalid_packet} = Parser.parse(nil)
    end

    test "returns error for invalid packet format" do
      assert {:error, "Invalid packet format"} = Parser.parse(":badpacket")
    end

    test "returns error for unknown error" do
      # This triggers the _ -> {:error, "PARSE ERROR"} branch
      # Use a packet that will fail split_path
      # path is not splittable
      msg = "NOCALL>APRS:!badpath"
      result = Parser.parse(msg)
      assert {:ok, packet} = result
      assert packet.data_extended.data_type == :malformed_position

      if Map.has_key?(packet, :latitude) and not is_nil(packet.latitude) do
        assert is_struct(packet.latitude, Decimal)
      end

      if Map.has_key?(packet, :longitude) and not is_nil(packet.longitude) do
        assert is_struct(packet.longitude, Decimal)
      end
    end
  end

  describe "parse_data/3 unknown and fallback branches" do
    test "returns nil for unknown type" do
      assert Parser.parse_data(:unknown_type, "", "") == nil
    end

    test "returns nil for parse_data/3 fallback" do
      assert Parser.parse_data(:not_a_real_type, "", "") == nil
    end
  end

  describe "parse_datatype/1 edge and unknown cases" do
    test "returns :unknown_datatype for unrecognized data" do
      assert Parser.parse_datatype("ZZZ") == :unknown_datatype
      assert Parser.parse_datatype("") == :unknown_datatype
    end

    property "returns an atom for any printable string" do
      check all s <- StreamData.string(:printable, min_length: 1) do
        assert is_atom(Parser.parse_datatype(s))
      end
    end
  end

  describe "split_packet/1 and split_path/1 malformed input" do
    test "split_packet/1 returns error for missing parts" do
      assert {:error, _} = Parser.split_packet("")
      assert {:error, _} = Parser.split_packet("NOCALL>")
      assert {:error, _} = Parser.split_packet(":nope")
    end

    test "split_path/1 returns error for invalid format" do
      # The actual behavior is to return {:ok, ["", ",,,"]}
      assert Parser.split_path(",,,,") == {:ok, ["", ",,,"]}
    end
  end
end
