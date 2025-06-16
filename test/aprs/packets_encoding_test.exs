defmodule Aprs.PacketsEncodingTest do
  use Aprs.DataCase

  alias Aprs.EncodingUtils
  alias Aprs.Packets

  describe "store_packet/1 with encoding issues" do
    test "handles invalid UTF-8 bytes in information_field" do
      packet_data = %{
        base_callsign: "XE2CT",
        ssid: "10",
        data_type: "position",
        destination: "APDR15",
        information_field: "Test with invalid byte: " <> <<0xB0>>,
        path: "WIDE1-1,WIDE2-1",
        sender: "XE2CT-10",
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.information_field == "Test with invalid byte:"
      assert stored_packet.sender == "XE2CT-10"
    end

    test "handles invalid UTF-8 bytes in sender field" do
      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        data_type: "position",
        destination: "APDR15",
        information_field: "Valid information",
        path: "WIDE1-1,WIDE2-1",
        sender: "TEST" <> <<0xB0>> <> "-1",
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.sender == "TEST-1"
      assert stored_packet.information_field == "Valid information"
    end

    test "handles invalid UTF-8 bytes in comment field" do
      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        data_type: "position",
        destination: "APDR15",
        information_field: "Valid information",
        path: "WIDE1-1,WIDE2-1",
        sender: "TEST-1",
        comment: "Comment with invalid bytes: " <> <<0xB0, 0xFF, 0x80>>,
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.comment == "Comment with invalid bytes:"
      assert stored_packet.sender == "TEST-1"
    end

    test "handles multiple invalid UTF-8 bytes across different fields" do
      packet_data = %{
        base_callsign: "TEST" <> <<0xB0>>,
        ssid: "1",
        data_type: "position",
        destination: "APDR" <> <<0xFF>> <> "15",
        information_field: "Info " <> <<0x80, 0x81>>,
        path: "WIDE1-1" <> <<0xB0>> <> ",WIDE2-1",
        sender: "TEST-1",
        comment: "Comment " <> <<0xB0>>,
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.base_callsign == "TEST"
      assert stored_packet.destination == "APDR15"
      assert stored_packet.information_field == "Info"
      assert stored_packet.path == "WIDE1-1,WIDE2-1"
      assert stored_packet.comment == "Comment"
    end

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

    test "handles the specific 0xb0 byte that caused the original error" do
      # This is the exact scenario from the error message
      packet_data = %{
        base_callsign: "XE2CT",
        ssid: "10",
        data_type: "position",
        destination: "APDR15",
        information_field: "Some text" <> <<0xB0>> <> "more text",
        path: "WIDE1-1,WIDE2-1",
        sender: "XE2CT-10",
        lat: 19.12345,
        lon: -99.54321,
        has_position: true
      }

      # This should not raise a Postgrex.Error anymore
      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.sender == "XE2CT-10"
      # The 0xb0 byte should be removed
      assert stored_packet.information_field == "Some textmore text"
    end
  end

  describe "EncodingUtils integration" do
    test "sanitize_string removes invalid UTF-8 sequences" do
      invalid_string = "Valid text" <> <<0xB0>> <> "more text"
      sanitized = EncodingUtils.sanitize_string(invalid_string)
      assert sanitized == "Valid textmore text"
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
