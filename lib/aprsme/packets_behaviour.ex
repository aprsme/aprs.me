defmodule Aprsme.PacketsBehaviour do
  @moduledoc """
  Behaviour for Packets module to allow mocking in tests
  """

  @callback get_historical_packet_count(map()) :: non_neg_integer()
  @callback stream_packets_for_replay(map()) :: Enumerable.t()
  @callback get_packets_for_replay(map()) :: list()
  @callback get_recent_packets(map()) :: list()
  @callback get_weather_packets(String.t(), DateTime.t(), DateTime.t(), map()) :: list()
  @callback clean_old_packets() :: {:ok, non_neg_integer()} | {:error, any()}
  @callback clean_packets_older_than(pos_integer()) :: {:ok, non_neg_integer()} | {:error, any()}
end
