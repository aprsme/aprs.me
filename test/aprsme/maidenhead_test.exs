defmodule Aprsme.MaidenheadTest do
  use ExUnit.Case, async: true

  alias Aprsme.Maidenhead

  describe "encode/2" do
    test "encodes known locations to 6-character grid squares" do
      # Washington DC area
      assert Maidenhead.encode(-77.036, 38.897) == "FM18lv"
      # London
      assert Maidenhead.encode(-0.1257, 51.5085) == "IO91wm"
      # Tokyo
      assert Maidenhead.encode(139.6917, 35.6895) == "PM95uq"
      # Sydney
      assert Maidenhead.encode(151.2093, -33.8688) == "QF56od"
    end

    test "encodes edge coordinates" do
      # South pole
      assert Maidenhead.encode(0.0, -90.0) == "JA00aa"
      # North pole area
      assert Maidenhead.encode(0.0, 89.99) == "JR09ax"
    end

    test "returns nil for nil inputs" do
      assert Maidenhead.encode(nil, 0.0) == nil
      assert Maidenhead.encode(0.0, nil) == nil
      assert Maidenhead.encode(nil, nil) == nil
    end
  end
end
