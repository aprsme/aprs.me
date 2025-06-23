defmodule Aprs.PacketsBehaviour do
  @moduledoc """
  Behavior definition for the Packets context.
  This allows us to mock the Packets module in tests.
  """

  @callback get_packets_for_replay(map()) :: [Aprs.Packet.t()]
  @callback get_recent_packets(map()) :: [Aprs.Packet.t()]
  @callback get_historical_packet_count(map()) :: non_neg_integer()
  @callback stream_packets_for_replay(map()) :: Enumerable.t()
  @callback clean_old_packets() :: non_neg_integer()
  @callback clean_packets_older_than(pos_integer()) :: non_neg_integer()
end
