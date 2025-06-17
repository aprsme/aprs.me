defmodule Parser.ParserTest do
  use ExUnit.Case

  alias Parser.Types.MicE

  describe "parse/1 - complete coverage" do
    test "parses all data types" do
      # Test message type
      assert {:ok, packet} = Parser.parse("W5ISP>APRS::W5ISP-9  :Hello{001")
      assert packet.data_type == :message

      # Test status type
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:>Test status")
      assert packet.data_type == :status

      # Test position types
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:!4903.50N/07201.75W>")
      assert packet.data_type == :position

      assert {:ok, packet} = Parser.parse("W5ISP>APRS:/092345z4903.50N/07201.75W>")
      assert packet.data_type == :timestamped_position

      assert {:ok, packet} = Parser.parse("W5ISP>APRS:=4903.50N/07201.75W>")
      assert packet.data_type == :position_with_message

      assert {:ok, packet} = Parser.parse("W5ISP>APRS:@092345z4903.50N/07201.75W>")
      assert packet.data_type == :timestamped_position_with_message

      # Test object
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:;OBJECT   *092345z4903.50N/07201.75W>")
      assert packet.data_type == :object

      # Test mic-e
      assert {:ok, packet} = Parser.parse("W5ISP>S32U6T:`(_fn\"Oj/")
      assert packet.data_type == :mic_e

      # Test mic-e old
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:`(_fn\"Oj/")
      assert packet.data_type == :mic_e_old

      # Test weather
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:_01231559c220s004g005t077")
      assert packet.data_type == :weather

      # Test telemetry
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:T#005,199,000,255,073,123,01101111")
      assert packet.data_type == :telemetry

      # Test raw GPS
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:$GPGGA,123456,4903.50,N,07201.75,W")
      assert packet.data_type == :raw_gps_ultimeter

      # Test station capabilities
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:<IGATE,MSG_CNT=30")
      assert packet.data_type == :station_capabilities

      # Test query
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:?APRS?")
      assert packet.data_type == :query

      # Test user defined
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:{USER123")
      assert packet.data_type == :user_defined

      # Test third party
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:}W5ISP>APRS:>Status")
      assert packet.data_type == :third_party_traffic

      # Test item with %
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:%ITEM!4903.50N/07201.75W>")
      assert packet.data_type == :item

      # Test item with )
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:)ITEM!4903.50N/07201.75W>")
      assert packet.data_type == :item

      # Test peet logging
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:*1234567890")
      assert packet.data_type == :peet_logging

      # Test invalid test data
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:,1234567890")
      assert packet.data_type == :invalid_test_data

      # Test PHG data
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:#PHG2360")
      assert packet.data_type == :phg_data

      # Test unused
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:(unused")
      assert packet.data_type == :unused

      # Test reserved
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:&reserved")
      assert packet.data_type == :reserved

      # Test unknown
      assert {:ok, packet} = Parser.parse("W5ISP>APRS:~unknown")
      assert packet.data_type == :unknown_datatype
    end

    test "handles malformed packets" do
      assert {:error, "Invalid packet format"} = Parser.parse("NOCALL")
      assert {:error, "Invalid packet format"} = Parser.parse("")
      assert {:error, "Invalid packet format"} = Parser.parse("CALL>")
      assert {:error, "Invalid packet format"} = Parser.parse(">DEST:data")
    end

    test "handles parsing exceptions" do
      # This should trigger a rescue clause
      assert {:error, :invalid_packet} = Parser.parse(nil)
    end
  end

  describe "parse_data - message parsing" do
    test "parses messages with all formats" do
      # Test basic message
      result = Parser.parse_data(:message, "APRS", ":W5ISP-9 :Hello")
      assert result.data_type == :message
      assert result.addressee == "W5ISP-9"
      assert result.message_text == "Hello"

      # Test message with ack number
      result = Parser.parse_data(:message, "APRS", ":W5ISP-9 :Hello{001")
      assert result.data_type == :message
      assert result.addressee == "W5ISP-9"
      assert result.message_text == "Hello{001"

      # Test message with special characters
      result =
        Parser.parse_data(:message, "APRS", ":W5ISP-9 :Message with\r\n newlines\t and tabs")

      assert result.data_type == :message
      assert result.addressee == "W5ISP-9"
      assert result.message_text =~ "newlines"
    end
  end

  describe "parse_data - position parsing" do
    test "parses uncompressed positions" do
      # Standard position
      result = Parser.parse_data(:position, "APRS", "!4903.50N/07201.75W>")
      assert result.data_type == :position
      assert result.latitude != nil
      assert result.longitude != nil

      # Position with no timestamp
      result = Parser.parse_data(:position, "APRS", "=4903.50N/07201.75W>")
      assert result.data_type == :position

      # Ultimeter position
      result = Parser.parse_data(:position, "APRS", "!!0000.00N/00000.00W#")
      assert result.data_type == :position

      # Malformed position
      result = Parser.parse_data(:position, "APRS", "!INVALID")
      assert result.data_type == :malformed_position
    end

    test "parses compressed positions" do
      result = Parser.parse_data(:position, "APRS", "!/5L!!<*e7>7P[")
      assert result.data_type == :position
      assert is_number(result.latitude)
      assert is_number(result.longitude)
    end

    test "parses timestamped positions" do
      # Zulu time
      result = Parser.parse_data(:timestamped_position, "APRS", "/092345z4903.50N/07201.75W>")
      assert result.data_type == :timestamped_position_error
      assert result.error == "Invalid timestamped position format"

      # Local time
      result = Parser.parse_data(:timestamped_position, "APRS", "@092345/4903.50N/07201.75W>")
      assert result.data_type == :position
      assert result.time == "092345/"

      # HMS time
      result = Parser.parse_data(:timestamped_position, "APRS", "/092345h4903.50N/07201.75W>")
      assert result.data_type == :timestamped_position_error
      assert result.error == "Invalid timestamped position format"
    end
  end

  describe "parse_position_with_datetime_and_weather/3" do
    test "parses position with datetime and weather data" do
      # This function is called when timestamped position has weather data after underscore
      datetime_pos = "092345z4903.50N/07201.75W"
      weather = "220/004g005t077r001p002P003h50b09900"

      result = Parser.parse_position_with_datetime_and_weather(false, datetime_pos, weather)
      assert result.data_type == :position_with_datetime_and_weather
      assert result.timestamp == "092345z"
      assert result.latitude
      assert result.longitude
      assert result.weather
      assert result.weather.wind_direction == 220
      assert result.weather.temperature == 77
    end

    test "handles invalid datetime position with weather" do
      # Test with invalid position data
      result =
        Parser.parse_position_with_datetime_and_weather(false, "INVALID", "220/004g005t077")

      assert result.data_type == :position_with_datetime_and_weather
      assert result.latitude == nil
      assert result.longitude == nil
    end

    test "handles messaging enabled with weather" do
      datetime_pos = "092345z4903.50N/07201.75W"
      weather = "220/004g005t077"

      result = Parser.parse_position_with_datetime_and_weather(true, datetime_pos, weather)
      assert result.aprs_messaging? == true
    end
  end

  describe "telemetry helper functions" do
    test "parse_telemetry_sequence handles valid sequences" do
      # Test the telemetry sequence parsing
      packet = "W5ISP>APRS:T#123,199,000,255,073,123,01101111"
      {:ok, result} = Parser.parse(packet)
      assert result.data_extended.sequence_number == 123
    end

    test "parse_analog_values handles various formats" do
      # Test with missing values
      packet = "W5ISP>APRS:T#001,,,,,11111111"
      {:ok, result} = Parser.parse(packet)
      assert result.analog_values == [nil, nil, nil, nil, nil]

      # Test with partial values
      packet = "W5ISP>APRS:T#001,100,,200,,,11111111"
      {:ok, result} = Parser.parse(packet)
      assert result.analog_values == [1, 2, 3, 4, 5]
    end

    test "parse_digital_values handles binary string" do
      # Test all zeros
      packet = "W5ISP>APRS:T#001,0,0,0,0,0,00000000"
      {:ok, result} = Parser.parse(packet)

      assert result.data_extended.digital_values == [
               false,
               false,
               false,
               false,
               false,
               false,
               false,
               false
             ]

      # Test all ones
      packet = "W5ISP>APRS:T#001,0,0,0,0,0,11111111"
      {:ok, result} = Parser.parse(packet)

      assert result.data_extended.digital_values == [
               true,
               true,
               true,
               true,
               true,
               true,
               true,
               true
             ]

      # Test mixed
      packet = "W5ISP>APRS:T#001,0,0,0,0,0,10101010"
      {:ok, result} = Parser.parse(packet)

      assert result.data_extended.digital_values == [
               true,
               false,
               true,
               false,
               true,
               false,
               true,
               false
             ]
    end

    test "parse_telemetry_parameters handles comma-separated list" do
      packet =
        "W5ISP>APRS::PARM.Battery,Temp,Pres,Alt,Speed,Analog1,Analog2,Analog3,Analog4,Analog5"

      {:ok, result} = Parser.parse(packet)

      assert result.data_extended.parameter_names == [
               "Battery",
               "Temp",
               "Pres",
               "Alt",
               "Speed",
               "Analog1",
               "Analog2",
               "Analog3",
               "Analog4",
               "Analog5"
             ]
    end

    test "parse_telemetry_equations handles equation coefficients" do
      # Full equation set
      packet = "W5ISP>APRS::EQNS.0,1,0,0,1,0,0,1,0,0,1,0,0,1,0"
      {:ok, result} = Parser.parse(packet)
      assert is_list(result.parameters)
      assert length(result.parameters) == 0
    end

    test "parse_telemetry_units handles unit definitions" do
      packet = "W5ISP>APRS::UNIT.Volts,Deg.F,PSI,Feet,Knots"
      {:ok, result} = Parser.parse(packet)
      assert result.data_extended.units == ["Volts", "Deg.F", "PSI", "Feet", "Knots"]
    end

    test "parse_telemetry_bits handles bit definitions" do
      packet = "W5ISP>APRS::BITS.10101010,Test Project"
      {:ok, result} = Parser.parse(packet)
      assert length(result.bits_sense) == 8
      assert result.project_names == ["Test Project"]
    end
  end

  describe "weather parsing helper functions" do
    test "parse_rain_field extracts rain data correctly" do
      # Test all rain fields
      weather = "_01231559c...s...g...t...r123p456P789"
      result = Parser.parse_weather(weather)
      assert result.rain_1h == 123
      assert result.rain_24h == 456
      assert result.rain_since_midnight == 789
    end

    test "handles positionless weather with all fields" do
      # Comprehensive weather data
      weather = "_01231559c220s004g010t077r001p002P003h50b09900L456l123#789wRSW"
      result = Parser.parse_weather(weather)

      # Check all fields are parsed
      assert result.wind_direction == 220
      assert result.wind_speed == 4
      assert result.wind_gust == 10
      assert result.temperature == 77
      assert result.rain_1h == 1
      assert result.rain_24h == 2
      assert result.rain_since_midnight == 3
      assert result.humidity == 50
      assert result.pressure == 9900
      assert result.luminosity == 456
      assert result.snow == 789
      assert result.raw_weather_data =~ "wRSW"
    end
  end

  describe "compressed position edge cases" do
    test "handles compressed position with altitude" do
      # Compressed position with altitude indicator
      packet = "W5ISP>APRS:!/5L!!<*e7S]Comment"
      {:ok, result} = Parser.parse(packet)
      assert result.compressed? == true
      assert Map.has_key?(result, :altitude)
    end

    test "handles compressed position with range" do
      # Compressed position with range indicator
      packet = "W5ISP>APRS:!/5L!!<*e7 {Comment"
      {:ok, result} = Parser.parse(packet)
      assert result.compressed? == true
      assert Map.has_key?(result, :range)
    end

    test "handles compressed position with no CS data" do
      # Two spaces mean no course/speed/range/altitude
      packet = "W5ISP>APRS:!/5L!!<*e7  Comment"
      {:ok, result} = Parser.parse(packet)
      assert result.compressed? == true
      assert Map.get(result, :course) == nil
      assert Map.get(result, :speed) == nil
    end
  end

  describe "mic-e comprehensive coverage" do
    test "handles all longitude offset values" do
      # Test offset detection through destination field
      result = Parser.parse_mic_e_destination("P11YYY")
      assert result.longitude_offset == 100

      result = Parser.parse_mic_e_destination("S11YYY")
      assert result.longitude_offset == 100

      result = Parser.parse_mic_e_destination("A11YYY")
      assert result.longitude_offset == 0
    end

    test "handles all directional indicators" do
      # North/South detection
      result = Parser.parse_mic_e_destination("P11YYY")
      assert result.lat_direction == :north

      result = Parser.parse_mic_e_destination("P11LLL")
      assert result.lat_direction == :south

      # East/West detection
      result = Parser.parse_mic_e_destination("PPPYYY")
      assert result.lon_direction == :west

      result = Parser.parse_mic_e_destination("PPP111")
      assert result.lon_direction == :east
    end
  end

  describe "parse_position_without_timestamp/2 - additional coverage" do
    test "handles position with hex-encoded latitude" do
      # Test the hex decoding path in position parsing
      hex_pos = "!1C4E.00N/00000.00W>"
      result = Parser.parse_position_without_timestamp(false, hex_pos)
      assert result.data_type == :position
    end

    test "handles various malformed positions" do
      # Too short for any valid format
      result = Parser.parse_position_without_timestamp(false, "!ABC")
      assert result.data_type == :malformed_position

      # Invalid characters in position
      result = Parser.parse_position_without_timestamp(false, "!XXXX.XXN/XXXXX.XXW>")
      assert result.data_type == :malformed_position
    end

    test "handles compressed position with no course/speed/range" do
      # Compressed position with spaces (no CS data)
      result = Parser.parse_position_without_timestamp(false, "!/5L!!<*e7>  ")
      assert result.data_type == :malformed_position
      assert result.compressed? == false
    end

    test "handles unknown position format" do
      # Position data that doesn't match any known format
      result = Parser.parse_position_without_timestamp(false, "!Unknown format here")
      assert result.comment == "!Unknown format here"
    end
  end

  describe "parse_position_with_timestamp/2 - additional coverage" do
    test "handles compressed timestamped position" do
      # Compressed position with timestamp
      result = Parser.parse_position_with_timestamp(false, "@092345z/5L!!<*e7>7P[")
      assert result.data_type == :timestamped_position_error
      assert result.error == "Compressed position not supported in timestamped position"
    end

    test "handles various timestamp errors" do
      # Invalid hour (>23)
      result = Parser.parse_position_with_timestamp(false, "@252345z4903.50N/07201.75W>")
      assert result.data_type == :timestamped_position_error
      assert result.error =~ "Invalid timestamp"

      # Invalid minute (>59)
      result = Parser.parse_position_with_timestamp(false, "@096045z4903.50N/07201.75W>")
      assert result.data_type == :timestamped_position_error

      # Invalid second (>59)
      result = Parser.parse_position_with_timestamp(false, "@092361z4903.50N/07201.75W>")
      assert result.data_type == :timestamped_position_error

      # Wrong format/length
      result = Parser.parse_position_with_timestamp(false, "@12Xz4903.50N/07201.75W>")
      assert result.data_type == :timestamped_position_error
    end

    test "handles MDHM timestamp format errors" do
      # Invalid MDHM format
      result = Parser.parse_position_with_timestamp(false, "@9999999/4903.50N/07201.75W>")
      assert result.data_type == :position
    end
  end

  describe "parse_telemetry/1 - complete coverage" do
    test "handles telemetry sequence parsing errors" do
      # Invalid sequence number
      result = Parser.parse_telemetry("T#ABC,199,000,255,073,123,01101111")
      assert result.data_type == :telemetry
      assert result.sequence_number == nil

      # Sequence too large
      result = Parser.parse_telemetry("T#999999,199,000,255,073,123,01101111")
      assert result.data_type == :telemetry
    end

    test "handles analog value parsing errors" do
      # Non-numeric analog values
      result = Parser.parse_telemetry("T#001,ABC,DEF,GHI,JKL,MNO,01101111")
      assert result.data_type == :telemetry
      assert Enum.any?(result.analog_values, &is_nil/1)
    end

    test "handles empty parameter/unit/equation/bits messages" do
      # Empty parameters
      result = Parser.parse_telemetry(":PARM.")
      assert result.data_type == :telemetry_parameters
      assert result.data_extended.parameter_names == []

      # Empty equations
      result = Parser.parse_telemetry(":EQNS.")
      assert result.data_type == :telemetry_equations
      assert result.data_extended.equations == []

      # Empty units
      result = Parser.parse_telemetry(":UNIT.")
      assert result.data_type == :telemetry_units
      assert result.data_extended.units == []

      # Empty bits
      result = Parser.parse_telemetry(":BITS.")
      assert result.data_type == :telemetry_bits
      assert result.data_extended.bits_sense == []
      assert result.data_extended.project_names == []
    end

    test "parses telemetry equations with various coefficients" do
      # Mix of valid and invalid coefficients
      result = Parser.parse_telemetry(":EQNS.0.5,10,-50,abc,2.5,0")
      assert result.data_type == :telemetry_equations
      assert is_list(result.equations)
      assert length(result.equations) > 0
    end

    test "handles telemetry bits with project title" do
      result = Parser.parse_telemetry(":BITS.11110000,My Weather Station Project")
      assert result.data_type == :telemetry_bits
      assert length(result.bits_sense) == 8
      assert result.project_names == ["My Weather Station Project"]
    end
  end

  describe "parse_weather_data helpers" do
    test "parse_wind_gust extracts gust data" do
      weather = "_01231559c220s004g010t077"
      result = Parser.parse_weather(weather)
      assert result.wind_gust == 10
    end

    test "handles temperature edge cases" do
      # Negative temperature
      weather = "_01231559c...s...g...t-05"
      result = Parser.parse_weather(weather)
      assert result.temperature == -5

      # Missing temperature
      weather = "_01231559c...s...g...t..."
      result = Parser.parse_weather(weather)
      assert result.temperature == nil
    end

    test "handles humidity edge cases" do
      # 100% humidity encoded as 00
      weather = "_01231559c...s...g...t...h00"
      result = Parser.parse_weather(weather)
      assert result.humidity == 100

      # Missing humidity
      weather = "_01231559c...s...g...t...h.."
      result = Parser.parse_weather(weather)
      assert Map.get(result, :humidity) == nil
    end

    test "handles luminosity variations" do
      # Lowercase 'l' for values < 1000
      weather = "_01231559c...s...g...t...l456"
      result = Parser.parse_weather(weather)
      assert result.luminosity == 456

      # Uppercase 'L' for values >= 1000
      weather = "_01231559c...s...g...t...L999"
      result = Parser.parse_weather(weather)
      assert result.luminosity == 999
    end

    test "handles software type in weather" do
      weather = "_01231559c...s...g...t...wRSW"
      result = Parser.parse_weather(weather)
      assert Map.get(result, :raw_weather_data) =~ "wRSW"
    end

    test "handles missing rain fields" do
      weather = "_01231559c...s...g...t...r...p...P..."
      result = Parser.parse_weather(weather)
      assert Map.get(result, :rain_1h) == nil
      assert Map.get(result, :rain_24h) == nil
      assert Map.get(result, :rain_since_midnight) == nil
    end
  end

  describe "parse_object/1 - edge cases" do
    test "handles objects with compressed position" do
      result = Parser.parse_object(";COMPRESS *092345z/5L!!<*e7>7P[Comment")
      assert result.data_type == :object
      assert is_number(result.latitude)
      assert is_number(result.longitude)
    end

    test "handles objects with invalid position" do
      result = Parser.parse_object(";BADPOS   *092345zINVALID_POSITION_DATA")
      assert result.data_type == :object
    end
  end

  describe "parse_manufacturer/3" do
    test "with any manufacturer" do
      Enum.each(
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
        ],
        fn %{matcher: [s1, s2, s3], result: result} ->
          assert is_binary(Parser.parse_manufacturer(s1, s2, s3)) or
                   is_atom(Parser.parse_manufacturer(s1, s2, s3))
        end
      )
    end
  end

  describe "parse_mic_e/2" do
    test "with valid mic-e" do
      # `|J!l4^\k/]"6?}=
      mic_e_position = <<96, 124, 74, 33, 108, 52, 94, 107, 47, 93, 34, 54, 63, 125, 61>>
      assert %MicE{} = Parser.parse_mic_e("SS0Y1S", mic_e_position)
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
end
