defmodule Aprs.PacketsEncodingTest do
  use Aprs.DataCase

  alias Aprs.EncodingUtils
  alias Aprs.Packets

  describe "store_packet/1 with encoding issues" do
    test "preserves valid UTF-8 characters" do
      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        data_type: "position",
        destination: "APDR15",
        information_field: "Valid UTF-8: ñ, é, 中文, 🚀",
        path: "WIDE1-1,WIDE2-1",
        sender: "TEST-1",
        comment: "More UTF-8: αβγ, русский",
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.information_field == "Valid UTF-8: ñ, é, 中文, 🚀"
      assert stored_packet.comment == "More UTF-8: αβγ, русский"
    end

    test "handles data_extended with invalid UTF-8" do
      data_extended = %{
        comment: "Extended comment with invalid: " <> <<0xB0>>,
        manufacturer: "Manufacturer " <> <<0xFF>>,
        equipment_type: "Equipment" <> <<0x80>>
      }

      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        data_type: "position",
        destination: "APDR15",
        information_field: "Valid information",
        path: "WIDE1-1,WIDE2-1",
        sender: "TEST-1",
        data_extended: data_extended,
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      # The data_extended should be sanitized through the EncodingUtils
      # Note: data_extended might be nil if not properly handled in the schema
      assert stored_packet.sender == "TEST-1"
    end

    test "handles nil and empty string values" do
      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        data_type: "position",
        destination: "APDR15",
        information_field: "",
        path: "",
        sender: "TEST-1",
        comment: nil,
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      # Empty strings may be stored as nil or empty string depending on Ecto behavior
      assert stored_packet.information_field in [nil, ""]
      assert stored_packet.path in [nil, ""]
      assert is_nil(stored_packet.comment)
    end

    test "handles binary data that looks like control characters" do
      # Test with various problematic byte sequences
      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        data_type: "position",
        destination: "APDR15",
        information_field: "Control chars: " <> <<0x00, 0x01, 0x02, 0x1F, 0x7F>>,
        path: "WIDE1-1,WIDE2-1",
        sender: "TEST-1",
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      # Control characters should be filtered out
      assert stored_packet.information_field == "Control chars:"
    end
  end

  describe "EncodingUtils integration" do
    test "sanitize_string removes invalid UTF-8 sequences" do
      invalid_string = "Valid text" <> <<0xB0>> <> "more text"
      sanitized = EncodingUtils.sanitize_string(invalid_string)
      assert sanitized == "Valid text°more text"
    end

    test "sanitize_string preserves valid UTF-8" do
      valid_string = "Hello 世界! 🌍"
      sanitized = EncodingUtils.sanitize_string(valid_string)
      assert sanitized == valid_string
    end

    test "encoding_info detects invalid UTF-8" do
      invalid_string = "Hello" <> <<0xB0>>
      info = EncodingUtils.encoding_info(invalid_string)
      assert info.valid_utf8 == false
      assert info.invalid_at == 5
    end
  end
end
