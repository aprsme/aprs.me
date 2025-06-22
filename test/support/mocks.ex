# Mocks are defined in test_helper.exs
# This file can be used for mock implementations or test utilities

defmodule Aprs.MockHelpers do
  @moduledoc """
  Helper functions for setting up mocks in tests
  """

  import Mox

  @doc """
  Stubs the PacketsMock to return an empty list for get_packets_for_replay.
  Use this in tests that don't care about historical packets but use MapLive.
  """
  def stub_packets_mock do
    stub(Aprs.PacketsMock, :get_packets_for_replay, fn _opts -> [] end)
  end

  @doc """
  Sets up an expectation for PacketsMock with custom return value.
  """
  def expect_packets_for_replay(packets) do
    stub(Aprs.PacketsMock, :get_packets_for_replay, fn _opts ->
      packets
    end)
  end

  @doc """
  Sets up an expectation for PacketsMock with filtering based on bounds.
  """
  def expect_packets_for_replay_with_bounds(packets) do
    stub(Aprs.PacketsMock, :get_packets_for_replay, fn opts ->
      case opts[:bounds] do
        [west, south, east, north] ->
          Enum.filter(packets, fn packet ->
            packet.has_position &&
              packet.lat >= south && packet.lat <= north &&
              packet.lon >= west && packet.lon <= east
          end)

        _ ->
          packets
      end
    end)
  end
end
