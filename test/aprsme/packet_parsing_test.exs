defmodule Aprsme.PacketParsingTest do
  use Aprsme.DataCase

  alias Aprsme.Packet

  describe "packet parsing for altitude and PHG" do
    test "extracts altitude and PHG data from packet" do
      raw_packet = "W5MRC-15>APN391,qAO,KG5JPL-1:!3317.02NN09634.37W#PHG5530 Collin Cty Wide Digi /A=000680"

      # Parse the packet
      {:ok, parsed} = Aprs.parse(raw_packet)

      # Create attributes for packet
      attrs = %{
        sender: parsed[:sender],
        data_extended: parsed[:data_extended]
      }

      # Extract additional data (this is where altitude and PHG are parsed from comment)
      extracted_attrs = Packet.extract_additional_data(attrs, raw_packet)

      # Verify altitude was extracted
      assert extracted_attrs[:altitude] == 680.0

      # Verify PHG data was extracted into individual fields
      # PHG5 = 25W (5^2)
      assert extracted_attrs[:phg_power] == 25
      # PHG x5x = 320 ft
      assert extracted_attrs[:phg_height] == 320
      # PHG xx3 = 3 dBi
      assert extracted_attrs[:phg_gain] == 3
      # PHG xxx0 = 0 degrees (not 360 for omni in our implementation)
      assert extracted_attrs[:phg_directivity] == 0

      # Verify comment has altitude and PHG removed
      assert extracted_attrs[:comment] == "Collin Cty Wide Digi"
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
