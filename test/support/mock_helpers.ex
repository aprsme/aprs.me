defmodule Aprs.MockHelpers do
  @moduledoc """
  Provides mocks and stubs for tests.
  """

  alias Aprs.BadPacket

  def stub_packets_mock do
    # Only define the mock if it doesn't already exist
    if !Code.ensure_loaded?(PacketsMock) do
      Mox.defmock(PacketsMock, for: Aprs.PacketsBehaviour)
    end

    Mox.stub(PacketsMock, :get_historical_packet_count, fn _opts -> 0 end)
    Mox.stub(PacketsMock, :stream_packets_for_replay, fn _opts -> [] end)
    Mox.stub(PacketsMock, :get_packets_for_replay, fn _opts -> [] end)
    Mox.stub(PacketsMock, :get_recent_packets, fn _opts -> [] end)
    Mox.stub(PacketsMock, :clean_old_packets, fn -> {:ok, 0} end)
    Mox.stub(PacketsMock, :clean_packets_older_than, fn _days -> {:ok, 0} end)
  end

  def stub_badpackets_mock do
    if !Code.ensure_loaded?(BadPacketsMock) do
      Mox.defmock(BadPacketsMock, for: BadPacket)
    end

    Mox.stub_with(BadPacketsMock, BadPacketsStub)
  end
end
