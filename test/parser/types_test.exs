defmodule Parser.TypesTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Types.Packet
  alias Parser.Types.ParseError
  alias Parser.Types.Position

  describe "Position.from_aprs/2" do
    test "parses valid APRS lat/lon strings" do
      result = Position.from_aprs("3339.13N", "11759.13W")
      assert_in_delta result.latitude, 33.65216666666667, 1.0e-10
      assert_in_delta result.longitude, -117.9855, 1.0e-10
      result2 = Position.from_aprs("1234.70S", "04540.70E")
      assert_in_delta result2.latitude, -12.345, 0.3
      assert_in_delta result2.longitude, 45.678333333333335, 1.0e-10
    end

    test "returns nils for invalid strings" do
      assert %{latitude: nil, longitude: nil} = Position.from_aprs("bad", "data")
    end
  end

  describe "Position.from_decimal/2" do
    property "returns a map with the same lat/lon as input" do
      check all lat <- StreamData.float(), lon <- StreamData.float() do
        result = Position.from_decimal(lat, lon)
        assert %{latitude: ^lat, longitude: ^lon} = result
      end
    end
  end

  describe "struct creation" do
    test "can create Packet, Position, and ParseError structs" do
      p =
        struct(Packet,
          id: "1",
          sender: "A",
          path: "B",
          destination: "C",
          information_field: "D",
          data_type: :foo,
          base_callsign: "E",
          ssid: "0",
          data_extended: nil,
          received_at: nil
        )

      assert p.id == "1"
      pos = struct(Position, latitude: 1.0, longitude: 2.0)
      assert pos.latitude == 1.0
      err = struct(ParseError, error_code: :bad, error_message: "fail", raw_data: "oops")
      assert err.error_code == :bad
    end
  end
end
