defmodule Aprsme.MockHelpersTest do
  use ExUnit.Case, async: false

  import Aprsme.MockHelpers

  setup do
    Mox.set_mox_private()
    :ok
  end

  describe "stub_packets_mock/0" do
    test "stubs get_recent_packets/1 to return []" do
      stub_packets_mock()
      assert [] = Aprsme.PacketsMock.get_recent_packets(%{})
    end

    test "stubs get_nearby_stations/4 to return []" do
      stub_packets_mock()
      assert [] = Aprsme.PacketsMock.get_nearby_stations(39.0, -104.0, nil, %{})
    end
  end
end
