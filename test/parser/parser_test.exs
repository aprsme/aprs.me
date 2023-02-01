defmodule Parser.ParserTest do
  use ExUnit.Case

  test "timestamped position" do
    aprs_message =
      "KE7XXX>APRS,TCPIP*,qAC,NINTH:@211743z4444.67N/11111.68W_061/005g012t048r000p000P000h77b10015.DsVP\r\n"

    {:ok, packet} = Parser.parse(aprs_message)
    assert packet.data_type == :timestamped_position_with_message
  end

  test "mic_e convert digits" do
    assert Parser.parse_mic_e_digit("0") == [0, 0, nil]
  end

  test "mic_e convert destination field" do
    assert Parser.parse_mic_e_destination("T7SYWP") == %{
             lat_degrees: 47,
             lat_minutes: 39,
             lat_fractional: 70,
             lat_direction: :north,
             lon_direction: :west,
             longitude_offset: 100,
             message_code: "M02",
             message_description: "In Service"
           }
  end

  test "mic_e convert information field" do
    information_field = ~s(`\(_fn"Oj/]TEST=)
    sut = Parser.parse_mic_e_information(information_field, 100)

    assert sut ==
             %{
               dti: "`",
               heading: 251,
               lon_degrees: 112,
               lon_fractional: 74,
               lon_minutes: 7,
               speed: 20,
               symbol: "j",
               table: "/",
               message: "]TEST=",
               manufacturer: "Kenwood DM-710"
             }
  end

  test "mic_e" do
    sut = Parser.parse_mic_e("T7SYWP", ~s(`\(_fn"Oj/))
    assert %Parser.Types.MicE{} = sut
  end

  test "weird format" do
    aprs_message =
      ~s(ON4AVM-11>APDI23,WIDE1-1,WIDE2-2,qAR,ON0LB-10:=S4`k!OZ,C# sT/A=000049DIXPRS 2.3.0b\n)

    Parser.parse(aprs_message)
  end

  describe "parse/1" do
    test "with invalid packet" do
      assert {:error, :invalid_packet} = Parser.parse("invalid packet")
    end
  end

  describe "parse_callsign/1" do
    test "callsign with ssid" do
      assert Parser.parse_callsign("W5ISP-1") == ["W5ISP", "1"]
    end

    test "callsign without ssid" do
      assert Parser.parse_callsign("W5ISP") == ["W5ISP", nil]
    end
  end

  describe "parse_datatype/1" do
    test "position" do
      %{
        ":" => :message,
        ">" => :status,
        "!" => :position,
        "/" => :timestamped_position,
        "=" => :position_with_message,
        "@" => :timestamped_position_with_message,
        ";" => :object,
        "`" => :mic_e,
        "'" => :mic_e_old,
        "_" => :weather,
        "T" => :telemetry,
        "$" => :raw_gps_ultimeter,
        "<" => :station_capabilities,
        "?" => :query,
        "{" => :user_defined,
        "}" => :third_party_traffic,
        "" => :unknown_datatype
      }
      |> Enum.each(fn {key, value} ->
        assert Parser.parse_datatype(key) == value
      end)
    end
  end

  describe "parse_manufacturer/3" do
    test "with any manufacturer" do
      [
        %{matcher: [" ", nil, nil], result: "Original MIC-E"},
        %{matcher: [">", nil, "="], result: "Kenwood TH-D72"},
        %{matcher: [">", nil, "^"], result: "Kenwood TH-D74"},
        %{matcher: [">", nil, nil], result: "Kenwood TH-D74A"},
        %{matcher: ["]", nil, "="], result: "Kenwood DM-710"},
        %{matcher: ["]", nil, nil], result: "Kenwood DM-700"},
        %{matcher: ["`", "_", " "], result: "Yaesu VX-8"},
        %{matcher: ["`", "_", "\""], result: "Yaesu FTM-350"},
        %{matcher: ["`", "_", "#"], result: "Yaesu VX-8G"},
        %{matcher: ["`", "_", "$"], result: "Yaesu FT1D"},
        %{matcher: ["`", "_", "%"], result: "Yaesu FTM-400DR"},
        %{matcher: ["`", "_", ")"], result: "Yaesu FTM-100D"},
        %{matcher: ["`", "_", "("], result: "Yaesu FT2D"},
        %{matcher: ["`", " ", "X"], result: "AP510"},
        %{matcher: ["`", nil, nil], result: "Mic-Emsg"},
        %{matcher: ["'", "|", "3"], result: "Byonics TinyTrack3"},
        %{matcher: ["'", "|", "4"], result: "Byonics TinyTrack4"},
        %{matcher: ["'", ":", "4"], result: "SCS GmbH & Co. P4dragon DR-7400 modems"},
        %{matcher: ["'", ":", "8"], result: "SCS GmbH & Co. P4dragon DR-7800 modems"},
        %{matcher: ["'", nil, nil], result: "McTrackr"},
        %{matcher: [nil, "\"", nil], result: "Hamhud ?"},
        %{matcher: [nil, "/", nil], result: "Argent ?"},
        %{matcher: [nil, "^", nil], result: "HinzTec anyfrog"},
        %{matcher: [nil, "*", nil], result: "APOZxx www.KissOZ.dk Tracker. OZ1EKD and OZ7HVO"},
        %{matcher: [nil, "~", nil], result: "Other"},
        %{matcher: [nil, nil, nil], result: :unknown_manufacturer}
      ]
      |> Enum.each(fn %{matcher: [s1, s2, s3], result: result} ->
        assert Parser.parse_manufacturer(s1, s2, s3) == result
      end)
    end
  end

  describe "parse_mic_e/2" do
    test "with valid mic-e" do
      # `|J!l4^\k/]"6?}=
      mic_e_position = <<96, 124, 74, 33, 108, 52, 94, 107, 47, 93, 34, 54, 63, 125, 61>>
      assert %Parser.Types.MicE{} = Parser.parse_mic_e("SS0Y1S", mic_e_position)
    end
  end

  describe "parse_mic_e_destination/1" do
    test "1" do
      assert Parser.parse_mic_e_destination("SS0Y1S") == %{
               lat_degrees: 33,
               lat_direction: :north,
               lat_fractional: 13,
               lat_minutes: 9,
               lon_direction: :west,
               longitude_offset: 0,
               message_code: "M01",
               message_description: "En Route"
             }
    end

    test "2" do
      assert Parser.parse_mic_e_destination("SS08LL") == %{
               lat_degrees: 33,
               lat_direction: :south,
               lat_fractional: 0,
               lat_minutes: 8,
               lon_direction: :east,
               longitude_offset: 0,
               message_code: "M01",
               message_description: "En Route"
             }
    end

    test "3" do
      assert Parser.parse_mic_e_destination("SS0L0A") == %{
               lat_degrees: 33,
               lat_direction: :south,
               lat_fractional: 0,
               lat_minutes: 0,
               lon_direction: :unknown,
               longitude_offset: 0,
               message_code: "M01",
               message_description: "En Route"
             }
    end

    test "4" do
      assert Parser.parse_mic_e_destination("SS0AA3") == %{
               lat_degrees: 33,
               lat_direction: :unknown,
               lat_fractional: 3,
               lat_minutes: 0,
               lon_direction: :east,
               longitude_offset: :unknown,
               message_code: "M01",
               message_description: "En Route"
             }
    end
  end

  describe "parse_mic_e_information/2" do
    test "1" do
      info = <<96, 40, 95, 102, 110, 34, 79, 106, 47, 93, 84, 69, 83, 84, 61>>

      assert Parser.parse_mic_e_information(info, 100) == %{
               dti: "`",
               heading: 251,
               lon_degrees: 112,
               lon_fractional: 74,
               lon_minutes: 7,
               manufacturer: "Kenwood DM-710",
               message: "]TEST=",
               speed: 20,
               symbol: "j",
               table: "/"
             }
    end

    test "2" do
      info = <<96, 40, 95, 102, 50, 34, 79, 106, 47, 93, 61>>

      assert Parser.parse_mic_e_information(info, 100) == %{
               dti: "`",
               heading: 251,
               lon_degrees: 112,
               lon_fractional: 74,
               lon_minutes: 7,
               manufacturer: "Kenwood DM-710",
               message: "]=",
               speed: 220,
               symbol: "j",
               table: "/"
             }
    end
  end

  describe "convert_compressed_cs/1" do
    # !!
    # I!
    # Y"
    test "1" do
      assert Parser.convert_compressed_cs("Y$") == %{course: 12, speed: 0.3}
    end
  end
end
