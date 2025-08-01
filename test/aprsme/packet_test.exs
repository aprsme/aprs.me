defmodule Aprsme.PacketTest do
  use Aprsme.DataCase, async: true
  alias Aprsme.Packet

  describe "extract_additional_data/2 with telemetry data" do
    test "converts string telemetry_vals to integers" do
      raw_packet = "W5ISP>APRS,TCPIP*:T#005,12.80,0.00,0.00,0.00,0.00,00000000"
      
      attrs = %{
        sender: "W5ISP",
        data_type: "telemetry",
        telemetry: %{
          seq: "005",
          vals: ["12.80", "0.00", "0.00", "0.00", "0.00"],
          bits: "00000000"
        }
      }

      result = Packet.extract_additional_data(attrs, raw_packet)

      assert result.telemetry_seq == 5
      assert result.telemetry_vals == [13, 0, 0, 0, 0]  # 12.80 rounds to 13
      assert result.telemetry_bits == "00000000"
    end

    test "handles mixed integer and string telemetry_vals" do
      attrs = %{
        sender: "TEST",
        data_type: "telemetry", 
        telemetry: %{
          seq: 10,
          vals: [180, "37.50", "0.00", 88, "164.75"],
          bits: "10101010"
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert result.telemetry_seq == 10
      assert result.telemetry_vals == [180, 38, 0, 88, 165]  # Mixed types converted properly
      assert result.telemetry_bits == "10101010"
    end

    test "handles float telemetry_vals" do
      attrs = %{
        sender: "TEST",
        data_type: "telemetry",
        telemetry: %{
          seq: "001", 
          vals: [12.8, 37.2, 0.1, 88.9, 164.5]
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert result.telemetry_seq == 1
      assert result.telemetry_vals == [13, 37, 0, 89, 165]  # Floats rounded to integers
    end

    test "handles invalid telemetry_vals gracefully" do
      attrs = %{
        sender: "TEST",
        data_type: "telemetry",
        telemetry: %{
          seq: "002",
          vals: ["invalid", nil, "", "12.5", :atom]
        }
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert result.telemetry_seq == 2
      assert result.telemetry_vals == [0, 0, 0, 13, 0]  # Invalid values become 0
    end

    test "handles missing telemetry data" do
      attrs = %{
        sender: "TEST",
        data_type: "position"
      }

      result = Packet.extract_additional_data(attrs, "test_packet")

      assert Map.get(result, :telemetry_seq) == nil
      assert Map.get(result, :telemetry_vals) == nil
      assert Map.get(result, :telemetry_bits) == nil
    end
  end
end