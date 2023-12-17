defmodule Parser.Types.PositionTest do
  use ExUnit.Case

  alias Parser.Types.Position

  describe "from_aprs/2" do
    test "1" do
      assert Position.from_aprs("3339.13N", "11759.13W") == %{
               latitude: 33.652166666666666,
               longitude: -117.9855
             }
    end
  end

  describe "from_decimal/2" do
    test "1" do
      assert Position.from_decimal(33.652166666666666, -117.9855) == %{
               latitude: 33.652166666666666,
               longitude: -117.9855
             }
    end
  end
end
