defmodule Aprsme.PacketsOldestTest do
  use Aprsme.DataCase

  alias Aprsme.Packets
  alias Aprsme.Repo

  describe "get_oldest_packet_timestamp/0" do
    test "returns nil when no packets exist" do
      assert is_nil(Packets.get_oldest_packet_timestamp())
    end

    test "returns the timestamp of the oldest packet" do
      # Create packets with different timestamps relative to now
      now = DateTime.utc_now()
      oldest_time = DateTime.add(now, -365 * 24 * 60 * 60, :second)  # 1 year ago
      middle_time = DateTime.add(now, -180 * 24 * 60 * 60, :second)  # 6 months ago
      newest_time = DateTime.add(now, -30 * 24 * 60 * 60, :second)   # 1 month ago

      # Insert packets with different timestamps
      {:ok, _} = create_test_packet("OLD-1", oldest_time)
      {:ok, _} = create_test_packet("MID-1", middle_time)
      {:ok, _} = create_test_packet("NEW-1", newest_time)

      # Should return the oldest timestamp
      result = Packets.get_oldest_packet_timestamp()
      
      # Allow for small time differences due to database precision
      assert DateTime.diff(result, oldest_time, :second) == 0
    end

    test "handles packets with microsecond precision" do
      # Create a packet with microsecond precision using current time
      now = DateTime.utc_now()
      # Truncate to microseconds to match database precision
      timestamp_with_microseconds = DateTime.truncate(now, :microsecond)
      
      {:ok, _} = create_test_packet("TEST-1", timestamp_with_microseconds)

      result = Packets.get_oldest_packet_timestamp()
      # Database might truncate microseconds, so we check the timestamp is close
      assert DateTime.diff(result, timestamp_with_microseconds, :microsecond) < 1000
    end
  end

  # Helper function to create a test packet with specific timestamp
  defp create_test_packet(callsign, received_at) do
    # Extract SSID from callsign (e.g., "TEST-1" -> ssid = "1")
    {base_callsign, ssid} =
      case String.split(callsign, "-", parts: 2) do
        [base] -> {base, "0"}
        [base, ssid_part] -> {base, ssid_part}
      end

    packet_data = %{
      sender: callsign,
      base_callsign: base_callsign,
      ssid: ssid,
      destination: "APRS",
      data_type: "position",
      path: "WIDE1-1",
      information_field: "!3216.46N/09647.82W>Test packet",
      raw_packet: "#{callsign}>APRS,WIDE1-1:!3216.46N/09647.82W>Test packet",
      lat: 32.274333,
      lon: -96.797,
      has_position: true,
      received_at: received_at
    }

    %Aprsme.Packet{}
    |> Aprsme.Packet.changeset(packet_data)
    |> Repo.insert()
  end
end
