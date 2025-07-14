defmodule Aprsme.PacketParsingTest do
  use Aprsme.DataCase

  alias Aprsme.Packet

  describe "packet parsing for altitude and PHG" do
    test "extracts altitude and PHG data from packet" do
      raw_packet = "W5MRC-15>APN391,qAO,KG5JPL-1:!3317.02NN09634.37W#PHG5530 Collin Cty Wide Digi /A=000680"

      # Parse the packet
      {:ok, parsed} = Aprs.parse(raw_packet)

      # Get the position data from data_extended
      position_data = parsed[:data_extended]

      # Verify altitude was extracted
      assert position_data[:altitude] == 680.0

      # Verify PHG data was extracted
      assert is_map(position_data[:phg])
      # PHG5 = 36W
      assert position_data[:phg][:power] == 36
      # PHG x5x = 320 ft
      assert position_data[:phg][:height] == 320
      # PHG xx3 = 3 dBi
      assert position_data[:phg][:gain] == 3
      # PHG xxx0 = omni (360°)
      assert position_data[:phg][:directivity] == 360

      # Verify comment includes PHG data (parser doesn't extract it from comment)
      assert position_data[:comment] == "PHG5530 Collin Cty Wide Digi"
    end

    test "packet changeset includes altitude and PHG fields" do
      attrs = %{
        sender: "W5MRC-15",
        base_callsign: "W5MRC",
        ssid: "15",
        data_type: "position",
        destination: "APN391",
        information_field: "!3317.02NN09634.37W#PHG5530 Collin Cty Wide Digi /A=000680",
        path: "qAO,KG5JPL-1",
        lat: Decimal.new("33.2837"),
        lon: Decimal.new("-96.5728"),
        location: %Geo.Point{coordinates: {-96.5728, 33.2837}},
        has_position: true,
        received_at: DateTime.truncate(DateTime.utc_now(), :second),
        data_extended: %{
          altitude: 680.0,
          phg: %{
            power: 25,
            height: 320,
            gain: 3,
            directivity: 0
          },
          comment: "Collin Cty Wide Digi"
        }
      }

      # Extract additional data
      attrs_with_data = Packet.extract_additional_data(attrs, attrs.information_field)

      # Create changeset
      changeset = Packet.changeset(%Packet{}, attrs_with_data)

      assert changeset.valid?

      # Check that fields were properly set
      changes = changeset.changes
      assert changes.altitude == 680.0
      assert changes.phg_power == 25
      assert changes.phg_height == 320
      assert changes.phg_gain == 3
      assert changes.phg_directivity == 0
      assert changes.comment == "Collin Cty Wide Digi"
    end
  end
end
