defmodule Aprs.MockHelpers do
  @moduledoc """
  Provides mocks and stubs for tests.
  """

  alias Aprs.BadPacket
  alias Aprs.Packets

  def stub_packets_mock do
    Mox.defmock(PacketsMock, for: Aprs.PacketsBehaviour)

    Mox.stub(PacketsMock, :get_historical_packet_count, fn _opts -> 0 end)
    Mox.stub(PacketsMock, :stream_packets_for_replay, fn _opts -> [] end)
  end

  def stub_badpackets_mock do
    Mox.defmock(BadPacketsMock, for: BadPacket)

    Mox.stub_with(BadPacketsMock, BadPacketsStub)
  end
end
