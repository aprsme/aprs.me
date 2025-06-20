defmodule Aprs.PacketsMicETest do
  use Aprs.DataCase

  alias Aprs.Packets
  alias Parser.Types.MicE

  describe "store_packet/1 with MicE data" do
    test "stores packet with MicE data_extended successfully" do
      # This test reproduces the exact error scenario from the bug report
      mic_e_data = %MicE{
        lat_degrees: 49,
        lat_minutes: 14,
        lat_fractional: 72,
        lat_direction: :north,
        lon_direction: :east,
        longitude_offset: 100,
        message_code: "M02",
        message_description: "In Service",
        dti: "`",
        heading: 0,
        lon_degrees: 12,
        lon_minutes: 5,
        lon_fractional: 75,
        speed: 0,
        manufacturer: "Kenwood TH-D74A",
        message: ">Harald QRV R1298,625"
      }

      packet_data = %{
        base_callsign: "DG1ID",
        ssid: "9",
        sender: "DG1ID-9",
        destination: "APRS",
        data_type: "mic_e",
        path: "TCPIP*",
        information_field: "MicE packet data",
        data_extended: mic_e_data
      }

      # This should not raise a KeyError
      assert {:ok, stored_packet} = Packets.store_packet(packet_data)

      # Verify the packet was stored with correct position data
      assert stored_packet.sender == "DG1ID-9"
      assert stored_packet.has_position == true

      # Verify coordinates were calculated correctly from MicE components
      expected_lat = 49.0 + 14.0 / 60.0
      expected_lon = 12.0 + 5.0 / 60.0

      assert stored_packet.lat |> Decimal.to_float() |> Float.round(3) == Float.round(expected_lat, 3)
      assert stored_packet.lon |> Decimal.to_float() |> Float.round(3) == Float.round(expected_lon, 3)
    end

    test "handles MicE data with south/west coordinates" do
      mic_e_data = %MicE{
        lat_degrees: 34,
        lat_minutes: 30,
        lat_fractional: 0,
        lat_direction: :south,
        lon_direction: :west,
        longitude_offset: 100,
        message_code: "M01",
        message_description: "En Route",
        dti: "`",
        heading: 90,
        lon_degrees: 118,
        lon_minutes: 15,
        lon_fractional: 30,
        speed: 25,
        manufacturer: "Kenwood TH-D74A",
        message: "Test message"
      }

      packet_data = %{
        base_callsign: "TEST",
        ssid: "1",
        sender: "TEST-1",
        destination: "APRS",
        data_type: "mic_e",
        path: "WIDE1-1,WIDE2-2",
        information_field: "MicE test packet",
        data_extended: mic_e_data
      }

      assert {:ok, stored_packet} = Packets.store_packet(packet_data)

      # Verify south latitude is negative
      expected_lat = -(34.0 + 30.0 / 60.0)
      # Verify west longitude is negative
      expected_lon = -(118.0 + 15.0 / 60.0)

      assert stored_packet.lat |> Decimal.to_float() |> Float.round(3) == Float.round(expected_lat, 3)
      assert stored_packet.lon |> Decimal.to_float() |> Float.round(3) == Float.round(expected_lon, 3)
      assert Decimal.to_float(stored_packet.lat) < 0
      assert Decimal.to_float(stored_packet.lon) < 0
    end

    test "handles MicE data with missing position components gracefully" do
      # Test with incomplete MicE data (missing some coordinate components)
      incomplete_mic_e = %MicE{
        lat_degrees: nil,
        lat_minutes: 14,
        lat_fractional: 72,
        lat_direction: :north,
        lon_direction: :east,
        longitude_offset: 100,
        message_code: "M02",
        message_description: "In Service",
        dti: "`",
        heading: 0,
        lon_degrees: 12,
        lon_minutes: 5,
        lon_fractional: 75,
        speed: 0,
        manufacturer: "Kenwood TH-D74A",
        message: "Test incomplete"
      }

      packet_data = %{
        base_callsign: "TEST",
        ssid: "2",
        sender: "TEST-2",
        destination: "APRS",
        data_type: "mic_e",
        path: "TCPIP*",
        information_field: "Incomplete MicE",
        data_extended: incomplete_mic_e
      }

      # Should still store the packet, but without position data
      assert {:ok, stored_packet} = Packets.store_packet(packet_data)
      assert stored_packet.sender == "TEST-2"
      # Should not have position data when components are missing
      assert stored_packet.has_position != true || stored_packet.has_position == nil
    end

    test "MicE Access behavior works correctly" do
      # Test the Access behavior implementation directly
      mic_e = %MicE{
        lat_degrees: 40,
        lat_minutes: 30,
        lat_fractional: 0,
        lat_direction: :north,
        lon_degrees: 74,
        lon_minutes: 15,
        lon_fractional: 0,
        lon_direction: :west
      }

      # Test bracket notation (which triggers Access behavior)
      assert mic_e[:latitude] == 40.5
      assert mic_e[:longitude] == -74.25

      # Test that we can access both calculated and direct fields
      assert mic_e[:lat_degrees] == 40
      assert mic_e[:message_code] == nil

      # Test get_in works
      assert get_in(mic_e, [:latitude]) == 40.5
      assert get_in(mic_e, [:longitude]) == -74.25
    end

    test "exact error case from bug report - DG1ID-9" do
      # This test reproduces the EXACT error scenario from the bug report
      # Exception in store_packet for "DG1ID-9": %KeyError{key: :latitude, term: %Parser.Types.MicE{...
      mic_e_data = %MicE{
        lat_degrees: 49,
        lat_minutes: 14,
        lat_fractional: 72,
        lat_direction: :north,
        lon_direction: :east,
        longitude_offset: 100,
        message_code: "M02",
        message_description: "In Service",
        dti: "`",
        heading: 0,
        # Note: this is the exact value from the error
        lon_degrees: 198,
        lon_minutes: 5,
        lon_fractional: 75,
        speed: 0,
        manufacturer: "Kenwood TH-D74A",
        message: ">Harald QRV R1298,625"
      }

      packet_data = %{
        base_callsign: "DG1ID",
        ssid: "9",
        sender: "DG1ID-9",
        destination: "APRS",
        data_type: "mic_e",
        path: "TCPIP*",
        information_field: "MicE packet from error report",
        data_extended: mic_e_data
      }

      # This should NOT raise a KeyError{key: :latitude} anymore
      # The fix ensures we use data_extended[:latitude] instead of data_extended.latitude
      assert {:ok, stored_packet} = Packets.store_packet(packet_data)

      # Verify the packet was stored correctly
      assert stored_packet.sender == "DG1ID-9"

      # Note: longitude 198 is invalid (outside -180 to 180 range)
      # So the packet should be stored but without position data
      # This is correct behavior - the important thing is no KeyError was raised
      assert stored_packet.has_position != true || stored_packet.has_position == nil

      # The fix prevented the KeyError - that's what we're testing
      # Invalid coordinates are handled gracefully by the validation logic
    end
  end
end
