defmodule Aprs.PacketsBehaviour do
  @moduledoc """
  Behavior definition for the Packets context.
  This allows us to mock the Packets module in tests.
  """

  @callback get_packets_for_replay(map()) :: [Aprs.Packet.t()]
  @callback get_recent_packets(map()) :: [Aprs.Packet.t()]
  @callback get_historical_packet_count(map()) :: non_neg_integer()
  @callback stream_packets_for_replay(map()) :: Enumerable.t()
end
