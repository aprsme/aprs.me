defmodule Aprs.MockHelpers do
  @moduledoc """
  Provides mocks and stubs for tests.
  """

  alias Aprs.BadPacket
  alias Aprs.Packets

  def stub_packets_mock do
    Mox.defmock(PacketsMock, for: Packets)

    Mox.stub_with(PacketsMock, PacketsStub)
  end

  def stub_badpackets_mock do
    Mox.defmock(BadPacketsMock, for: BadPacket)

    Mox.stub_with(BadPacketsMock, BadPacketsStub)
  end
end
