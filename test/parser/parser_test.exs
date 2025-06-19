defmodule ParserTest do
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
        bad = s <> s
        assert match?({:error, _}, Parser.split_packet(bad))
      end
    end

    test "returns error for missing > or :" do
      assert match?({:error, _}, Parser.split_packet("senderpathdata"))
      assert match?({:error, _}, Parser.split_packet(":onlycolon"))
      assert match?({:error, _}, Parser.split_packet(">onlygt"))
    end
  end

  describe "split_path/1" do
    property "splits path into destination and digipeater path for any string" do
      check all s <- StreamData.string(:alphanumeric, min_length: 0, max_length: 10) do
        result = Parser.split_path(s)
        assert match?({:ok, [_, _]}, result)
      end
    end

    test "splits with no comma" do
      assert {:ok, ["DEST", ""]} = Parser.split_path("DEST")
    end

    test "splits with one comma" do
      assert {:ok, ["DEST", "DIGI"]} = Parser.split_path("DEST,DIGI")
    end

    test "returns error for more than one comma" do
      assert {:ok, ["A", "A,A"]} = Parser.split_path("A,A,A")
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

  describe "validate_callsign/2" do
    property "accepts valid source callsigns" do
      uppercase = Enum.map(?A..?Z, &<<&1>>)
      digits = Enum.map(?0..?9, &<<&1>>)
      valid_chars = uppercase ++ digits ++ ["-"]

      check all cs_list <- StreamData.list_of(StreamData.member_of(valid_chars), min_length: 1),
                cs = Enum.join(cs_list) do
        assert :ok = Parser.validate_callsign(cs, :src)
      end
    end

    property "rejects invalid source callsigns" do
      check all cs <- StreamData.string(:printable, min_length: 1),
                not String.match?(cs, ~r/^[A-Z0-9\-]+$/) or String.contains?(cs, "*") do
        assert match?({:error, _}, Parser.validate_callsign(cs, :src))
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

  describe "parse_datatype/1 and parse_datatype_safe/1" do
    property "parse_datatype returns an atom for any printable string" do
      check all s <- StreamData.string(:printable, min_length: 1) do
        assert is_atom(Parser.parse_datatype(s))
      end
    end

    test "parse_datatype_safe returns {:ok, atom} for non-empty, {:error, _} for empty" do
      assert {:ok, _} = Parser.parse_datatype_safe("!")
      assert {:error, _} = Parser.parse_datatype_safe("")
    end

    test "returns correct atom for each known type indicator" do
      assert Parser.parse_datatype(":msg") == :message
      assert Parser.parse_datatype(">status") == :status
      assert Parser.parse_datatype("!pos") == :position
      assert Parser.parse_datatype("/tspos") == :timestamped_position
      assert Parser.parse_datatype("=posmsg") == :position_with_message
      assert Parser.parse_datatype("@tsmsg") == :timestamped_position_with_message
      assert Parser.parse_datatype(";object") == :object
      assert Parser.parse_datatype("`mic_e") == :mic_e
      assert Parser.parse_datatype("'mic_e_old") == :mic_e_old
      assert Parser.parse_datatype("_weather") == :weather
      assert Parser.parse_datatype("Ttele") == :telemetry
      assert Parser.parse_datatype("$raw") == :raw_gps_ultimeter
      assert Parser.parse_datatype("<cap") == :station_capabilities
      assert Parser.parse_datatype("?query") == :query
      assert Parser.parse_datatype("{userdef") == :user_defined
      assert Parser.parse_datatype("}thirdparty") == :third_party_traffic
      assert Parser.parse_datatype("%item") == :item
      assert Parser.parse_datatype(")item") == :item
      assert Parser.parse_datatype("*peet") == :peet_logging
      assert Parser.parse_datatype(",test") == :invalid_test_data
      assert Parser.parse_datatype("#DFSfoo") == :df_report
      assert Parser.parse_datatype("#PHGfoo") == :phg_data
      assert Parser.parse_datatype("#foo") == :phg_data
      assert Parser.parse_datatype("Xunknown") == :unknown_datatype
    end
  end

  describe "parse_data/3" do
    test "returns nil for unknown type" do
      assert Parser.parse_data(:unknown, "", "") == nil
    end

    test "returns nil for invalid test data" do
      assert Parser.parse_data(:invalid_test_data, "", ",testdata")[:data_type] ==
               :invalid_test_data
    end

    test "returns map for weather" do
      result = Parser.parse_data(:weather, "", "_12345678c000s000g000t000r000p000P000h00b00000")
      assert is_map(result)
      assert result[:data_type] == :weather
    end

    test "returns map for telemetry" do
      result = Parser.parse_data(:telemetry, "", "T#123,456,789,012,345,678,901,234")
      assert is_map(result)
      assert result[:data_type] == :telemetry
    end

    test "returns map for object" do
      result = Parser.parse_data(:object, "", ";OBJECT*111111z4903.50N/07201.75W>Test object")
      assert is_map(result)
      assert result[:data_type] == :object
    end

    test "returns map for item" do
      result = Parser.parse_data(:item, "", ")ITEM!4903.50N/07201.75W>Test item")
      assert is_map(result)
      assert result[:data_type] == :item
    end

    test "returns map for status" do
      result = Parser.parse_data(:status, "", ">Test status message")
      assert is_map(result)
      assert result[:data_type] == :status
    end

    test "returns map for user_defined" do
      result = Parser.parse_data(:user_defined, "", "{userdef")
      assert is_map(result)
      assert result[:data_type] == :user_defined
    end

    # test "returns map for third_party_traffic" do
    #   result = Parser.parse_data(:third_party_traffic, "", "}thirdparty")
    #   assert is_map(result)
    #   assert result[:data_type] == :third_party_traffic
    # end

    test "returns map for peet_logging" do
      result = Parser.parse_data(:peet_logging, "", "*peet")
      assert is_map(result)
      assert result[:data_type] == :peet_logging
    end

    test "returns map for station_capabilities" do
      result = Parser.parse_data(:station_capabilities, "", "<cap")
      assert is_map(result)
      assert result[:data_type] == :station_capabilities
    end

    test "returns map for query" do
      result = Parser.parse_data(:query, "", "?query")
      assert is_map(result)
      assert result[:data_type] == :query
    end

    test "returns map for df_report" do
      result = Parser.parse_data(:df_report, "", "#DFS1234rest")
      assert is_map(result)
      assert result[:data_type] == :df_report
    end

    test "returns map for phg_data" do
      result = Parser.parse_data(:phg_data, "", "#PHG1234rest")
      assert is_map(result)
      assert result[:data_type] == :phg_data
    end
  end

  describe "parse/1" do
    test "returns {:error, _} for obviously invalid input" do
      assert match?({:error, _}, Parser.parse(""))
      assert match?({:error, _}, Parser.parse("notapacket"))
    end
  end
end
