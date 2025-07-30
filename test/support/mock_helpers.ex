defmodule Aprsme.MockHelpers do
  @moduledoc """
  Helper functions for setting up mocks in tests.
  """

  def stub_packets_mock do
    # Stub the packets module to prevent external calls
    Mox.stub(Aprsme.PacketsMock, :get_packets_for_callsign, fn _callsign ->
      {:ok, []}
    end)

    Mox.stub(Aprsme.PacketsMock, :get_packets_for_callsign_with_limit, fn _callsign, _limit ->
      {:ok, []}
    end)

    Mox.stub(Aprsme.PacketsMock, :get_packets_for_callsign_with_date_range, fn _callsign, _start_date, _end_date ->
      {:ok, []}
    end)

    Mox.stub(Aprsme.PacketsMock, :get_recent_packets, fn _opts ->
      []
    end)

    Mox.stub(Aprsme.PacketsMock, :get_nearby_stations, fn _lat, _lon, _exclude, _opts ->
      []
    end)
  end

  def stub_badpackets_mock do
    Mox.stub_with(BadPacketsMock, BadPacketsStub)
  end
end
