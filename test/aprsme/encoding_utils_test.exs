defmodule Aprsme.EncodingUtilsTest do
  use ExUnit.Case

  alias Aprs.Types.MicE
  alias Aprsme.EncodingUtils
  alias Aprsme.Packet

  doctest EncodingUtils

  describe "sanitize_string/1" do
    test "handles nil input" do
      assert EncodingUtils.sanitize_string(nil) == nil
    end

    test "handles non-binary input" do
      assert EncodingUtils.sanitize_string(123) == 123
      assert EncodingUtils.sanitize_string(:atom) == :atom
    end

    test "sanitizes invalid UTF-8 sequences" do
      # Binary with invalid UTF-8 bytes (0xD3 and 0xD1 from the error)
      invalid_binary = <<85, 78, 73, 211, 78, 32, 80, 65, 78, 65, 77, 69, 209, 65>>
      result = EncodingUtils.sanitize_string(invalid_binary)

      # Should be a valid UTF-8 string
      assert String.valid?(result)

      # Should contain some recognizable text
      assert String.contains?(result, "UNI")
      assert String.contains?(result, "PANAME")
    end

    test "handles mixed valid and invalid UTF-8" do
      # "Hello" + invalid byte + "World"
      mixed_binary = <<72, 101, 108, 108, 111, 211, 87, 111, 114, 108, 100>>
      result = EncodingUtils.sanitize_string(mixed_binary)

      assert String.valid?(result)
      assert String.starts_with?(result, "Hello")
      assert String.ends_with?(result, "World")
    end
  end

  describe "sanitize_packet/1" do
    test "sanitizes string values in data map" do
      invalid_info = <<72, 101, 108, 108, 111, 211, 87, 111, 114, 108, 100>>

      packet = %Packet{
        id: "test-id",
        sender: "TEST-1",
        path: "TCPIP*",
        destination: "APX100",
        data_type: :position,
        base_callsign: "TEST",
        ssid: "1",
        data: %{"information_field" => invalid_info, "radiorange" => "0050"},
        data_extended: %{comment: "Valid comment"}
      }

      sanitized = EncodingUtils.sanitize_packet(packet)

      assert String.valid?(sanitized.data["information_field"])
      assert String.contains?(sanitized.data["information_field"], "Hello")
      assert String.contains?(sanitized.data["information_field"], "World")
      # Non-string values in data map are preserved
      assert sanitized.data["radiorange"] == "0050"
    end

    test "sanitizes comment in data_extended" do
      invalid_comment = <<85, 78, 73, 211, 78, 32, 80, 65, 78, 65, 77, 69, 209, 65>>

      packet = %Packet{
        id: "test-id",
        sender: "TEST-1",
        path: "TCPIP*",
        destination: "APX100",
        data_type: :position,
        base_callsign: "TEST",
        ssid: "1",
        data_extended: %{comment: invalid_comment}
      }

      sanitized = EncodingUtils.sanitize_packet(packet)

      assert String.valid?(sanitized.data_extended.comment)
      assert String.contains?(sanitized.data_extended.comment, "UNI")
    end

    test "handles packet with nil data_extended" do
      packet = %Packet{
        id: "test-id",
        sender: "TEST-1",
        path: "TCPIP*",
        destination: "APX100",
        data_type: :position,
        base_callsign: "TEST",
        ssid: "1",
        data_extended: nil
      }

      sanitized = EncodingUtils.sanitize_packet(packet)

      assert sanitized.data_extended == nil
    end
  end

  describe "sanitize_data_extended/1" do
    test "handles nil input" do
      assert EncodingUtils.sanitize_data_extended(nil) == nil
    end

    test "sanitizes comment field in map" do
      invalid_comment = <<72, 101, 108, 108, 111, 211, 87, 111, 114, 108, 100>>
      data_extended = %{comment: invalid_comment, latitude: 12.34, longitude: -56.78}

      sanitized = EncodingUtils.sanitize_data_extended(data_extended)

      assert String.valid?(sanitized.comment)
      assert sanitized.latitude == 12.34
      assert sanitized.longitude == -56.78
    end

    test "sanitizes message field in MicE struct" do
      invalid_message = <<72, 101, 108, 108, 111, 211, 87, 111, 114, 108, 100>>
      mic_e = %MicE{message: invalid_message, lat_degrees: 40}

      sanitized = EncodingUtils.sanitize_data_extended(mic_e)

      assert String.valid?(sanitized.message)
      assert sanitized.lat_degrees == 40
    end

    test "returns other data unchanged" do
      data = %{some_field: "value", number: 42}
      assert EncodingUtils.sanitize_data_extended(data) == data
    end
  end

  describe "to_hex/1" do
    test "converts binary to hex string" do
      assert EncodingUtils.to_hex(<<72, 101, 108, 108, 111>>) == "48656C6C6F"
      assert EncodingUtils.to_hex(<<0, 255>>) == "00FF"
      assert EncodingUtils.to_hex(<<>>) == ""
    end

    test "handles invalid UTF-8 bytes" do
      invalid_binary = <<85, 78, 73, 211, 78>>
      result = EncodingUtils.to_hex(invalid_binary)
      assert result == "554E49D34E"
    end
  end

  describe "to_float/1" do
    test "converts integer to float" do
      assert EncodingUtils.to_float(1) == 1.0
      assert EncodingUtils.to_float(0) == 0.0
      assert EncodingUtils.to_float(-42) == -42.0
    end

    test "returns float unchanged" do
      assert EncodingUtils.to_float(1.5) == 1.5
      assert EncodingUtils.to_float(-3.14) == -3.14
      assert EncodingUtils.to_float(0.0) == 0.0
    end

    test "returns nil for out-of-range integer" do
      assert is_nil(EncodingUtils.to_float(10_000_000_000_000_000))
      assert is_nil(EncodingUtils.to_float(-10_000_000_000_000_000))
    end

    test "converts valid string to float" do
      assert EncodingUtils.to_float("2.3") == 2.3
      assert EncodingUtils.to_float("42") == 42.0
      assert EncodingUtils.to_float("-7.5") == -7.5
      assert EncodingUtils.to_float("0") == 0.0
    end

    test "handles string with trailing text" do
      assert EncodingUtils.to_float("12.5abc") == 12.5
    end

    test "returns nil for invalid string" do
      assert is_nil(EncodingUtils.to_float("bad"))
      assert is_nil(EncodingUtils.to_float(""))
    end

    test "converts Decimal to float" do
      assert EncodingUtils.to_float(Decimal.new("3.14")) == 3.14
    end

    test "returns nil for nil" do
      assert is_nil(EncodingUtils.to_float(nil))
    end

    test "returns nil for unsupported types" do
      assert is_nil(EncodingUtils.to_float(:atom))
      assert is_nil(EncodingUtils.to_float([1, 2]))
      assert is_nil(EncodingUtils.to_float(%{a: 1}))
    end
  end

  describe "to_decimal/1" do
    test "returns Decimal unchanged" do
      d = Decimal.new("3.14")
      assert EncodingUtils.to_decimal(d) == d
    end

    test "converts float to Decimal" do
      result = EncodingUtils.to_decimal(1.5)
      assert is_struct(result, Decimal)
      assert Decimal.to_float(result) == 1.5
    end

    test "converts integer to Decimal" do
      result = EncodingUtils.to_decimal(42)
      assert is_struct(result, Decimal)
      assert Decimal.equal?(result, Decimal.new(42))
    end

    test "converts valid string to Decimal" do
      result = EncodingUtils.to_decimal("2.3")
      assert is_struct(result, Decimal)
      assert Decimal.equal?(result, Decimal.new("2.3"))
    end

    test "returns nil for invalid string" do
      assert is_nil(EncodingUtils.to_decimal("bad"))
      assert is_nil(EncodingUtils.to_decimal(""))
    end

    test "handles negative values" do
      result = EncodingUtils.to_decimal("-7.5")
      assert is_struct(result, Decimal)
      assert Decimal.negative?(result)
    end

    test "handles zero" do
      result = EncodingUtils.to_decimal(0)
      assert Decimal.equal?(result, Decimal.new(0))
    end

    test "handles very large integer" do
      result = EncodingUtils.to_decimal(999_999_999_999)
      assert is_struct(result, Decimal)
      assert Decimal.equal?(result, Decimal.new(999_999_999_999))
    end

    test "returns nil for unsupported types" do
      assert is_nil(EncodingUtils.to_decimal(nil))
      assert is_nil(EncodingUtils.to_decimal(:atom))
      assert is_nil(EncodingUtils.to_decimal([1, 2]))
    end
  end

  describe "normalize_data_type/1" do
    test "converts atom value with atom key" do
      assert EncodingUtils.normalize_data_type(%{data_type: :weather}) == %{data_type: "weather"}
    end

    test "converts atom value with string key" do
      assert EncodingUtils.normalize_data_type(%{"data_type" => :position}) == %{"data_type" => "position"}
    end

    test "leaves string value unchanged" do
      assert EncodingUtils.normalize_data_type(%{data_type: "bar"}) == %{data_type: "bar"}
      assert EncodingUtils.normalize_data_type(%{"data_type" => "baz"}) == %{"data_type" => "baz"}
    end

    test "returns map without data_type unchanged" do
      assert EncodingUtils.normalize_data_type(%{foo: 1}) == %{foo: 1}
    end

    test "returns non-map input unchanged" do
      assert EncodingUtils.normalize_data_type("string") == "string"
      assert EncodingUtils.normalize_data_type(nil) == nil
    end
  end

  describe "weather_fields/0" do
    test "returns all 10 weather fields" do
      fields = EncodingUtils.weather_fields()
      assert length(fields) == 10
    end

    test "includes expected fields" do
      fields = EncodingUtils.weather_fields()
      assert :temperature in fields
      assert :humidity in fields
      assert :wind_speed in fields
      assert :wind_direction in fields
      assert :wind_gust in fields
      assert :pressure in fields
      assert :rain_1h in fields
      assert :rain_24h in fields
      assert :rain_since_midnight in fields
      assert :snow in fields
    end

    test "all fields are atoms" do
      assert Enum.all?(EncodingUtils.weather_fields(), &is_atom/1)
    end
  end

  describe "sanitize_packet_strings/1" do
    test "passes DateTime through unchanged" do
      dt = ~U[2024-01-01 00:00:00Z]
      assert EncodingUtils.sanitize_packet_strings(dt) == dt
    end

    test "passes NaiveDateTime through unchanged" do
      ndt = ~N[2024-01-01 00:00:00]
      assert EncodingUtils.sanitize_packet_strings(ndt) == ndt
    end

    test "sanitizes nested map values" do
      input = %{"outer" => %{"inner" => <<0, 65, 66>>}}
      result = EncodingUtils.sanitize_packet_strings(input)
      assert result["outer"]["inner"] == "AB"
    end

    test "sanitizes list elements" do
      input = ["abc", <<0, 65, 66, 67>>]
      result = EncodingUtils.sanitize_packet_strings(input)
      assert result == ["abc", "ABC"]
    end

    test "converts struct to map and sanitizes" do
      input = %MicE{message: <<0, 72, 73>>}
      result = EncodingUtils.sanitize_packet_strings(input)
      assert is_map(result)
      assert result[:message] == "HI"
    end

    test "passes non-binary values through" do
      assert EncodingUtils.sanitize_packet_strings(nil) == nil
      assert EncodingUtils.sanitize_packet_strings(42) == 42
      assert EncodingUtils.sanitize_packet_strings(true) == true
    end
  end

  describe "encoding_info/1" do
    test "returns info for valid UTF-8 string" do
      info = EncodingUtils.encoding_info("Hello")
      assert info.valid_utf8 == true
      assert info.byte_count == 5
      assert info.char_count == 5
    end

    test "returns info for invalid UTF-8 binary" do
      invalid_binary = <<72, 101, 211, 108, 111>>
      info = EncodingUtils.encoding_info(invalid_binary)

      assert info.valid_utf8 == false
      assert info.byte_count == 5
      assert info.invalid_at == 2
    end

    test "handles empty binary" do
      info = EncodingUtils.encoding_info("")
      assert info.valid_utf8 == true
      assert info.byte_count == 0
      assert info.char_count == 0
    end

    test "handles multi-byte UTF-8 characters" do
      info = EncodingUtils.encoding_info("Café")
      assert info.valid_utf8 == true
      # 'é' takes 2 bytes
      assert info.byte_count == 5
      # but counts as 1 character
      assert info.char_count == 4
    end
  end
end
