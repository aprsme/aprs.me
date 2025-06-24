defmodule Aprsme.PacketReplayBehaviour do
  @moduledoc """
  Behavior definition for the PacketReplay module.
  This allows us to mock the PacketReplay module in tests.
  """

  @callback start_replay(pid(), list(Aprsme.Packet.t()), keyword()) :: {:ok, reference()} | {:error, term()}
  @callback stop_replay(reference()) :: :ok
  @callback pause_replay(reference()) :: :ok | {:error, :not_found}
  @callback resume_replay(reference()) :: :ok | {:error, :not_found}
  @callback get_replay_status(reference()) :: {:ok, map()} | {:error, :not_found}
  @callback adjust_replay_speed(reference(), float()) :: :ok | {:error, :not_found}
end
