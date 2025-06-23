defmodule Aprs.EncodingUtilsTest do
  use ExUnit.Case

  alias Aprs.EncodingUtils
  alias Aprs.Packet

  doctest Aprs.EncodingUtils

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
    test "sanitizes information_field" do
      invalid_info = <<72, 101, 108, 108, 111, 211, 87, 111, 114, 108, 100>>

      packet = %Packet{
        id: "test-id",
        sender: "TEST-1",
        path: "TCPIP*",
        destination: "APX100",
        information_field: invalid_info,
        data_type: :position,
        base_callsign: "TEST",
        ssid: "1",
        data_extended: %{comment: "Valid comment"}
      }

      sanitized = EncodingUtils.sanitize_packet(packet)

      assert String.valid?(sanitized.information_field)
      assert String.contains?(sanitized.information_field, "Hello")
      assert String.contains?(sanitized.information_field, "World")
    end

    test "sanitizes comment in data_extended" do
      invalid_comment = <<85, 78, 73, 211, 78, 32, 80, 65, 78, 65, 77, 69, 209, 65>>

      packet = %Packet{
        id: "test-id",
        sender: "TEST-1",
        path: "TCPIP*",
        destination: "APX100",
        information_field: "Valid info",
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
        information_field: "Valid info",
        data_type: :position,
        base_callsign: "TEST",
        ssid: "1",
        data_extended: nil
      }

      sanitized = EncodingUtils.sanitize_packet(packet)

      assert sanitized.data_extended == nil
      assert sanitized.information_field == "Valid info"
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
      mic_e = %Parser.Types.MicE{message: invalid_message, lat_degrees: 40}

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
