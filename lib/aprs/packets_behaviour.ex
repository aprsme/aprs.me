defmodule Aprs.PacketsBehaviour do
  @moduledoc """
  Behaviour for Packets module to allow mocking in tests
  """

  @callback get_historical_packet_count(map()) :: non_neg_integer()
  @callback stream_packets_for_replay(map()) :: Enumerable.t()
end
