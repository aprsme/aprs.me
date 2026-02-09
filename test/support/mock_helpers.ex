defmodule Aprsme.MockHelpers do
  @moduledoc """
  Helper functions for setting up mocks in tests.
  """

  def stub_packets_mock do
    # Stub the packets module to prevent external calls
    Mox.stub(Aprsme.PacketsMock, :get_recent_packets, fn _opts ->
      []
    end)

    Mox.stub(Aprsme.PacketsMock, :get_nearby_stations, fn _lat, _lon, _exclude, _opts ->
      []
    end)
  end
end
