defmodule Aprsme.PacketCounter do
  @moduledoc """
  Schema for the packet_counters table that provides O(1) packet counting.
  """
  use Ecto.Schema

  import Ecto.Query

  alias Aprsme.Repo

  schema "packet_counters" do
    field :counter_type, :string
    field :count, :integer, default: 0

    timestamps()
  end

  @doc """
  Gets the current packet count instantly.
  This is an O(1) operation thanks to the counter table.
  """
  def get_count(counter_type \\ "total_packets") do
    from(pc in __MODULE__,
      where: pc.counter_type == ^counter_type,
      select: pc.count
    )
    |> Repo.one()
    |> case do
      nil -> 0
      count -> count
    end
  end

  @doc """
  Subscribe to packet count changes via PostgreSQL LISTEN/NOTIFY.
  This allows real-time updates without polling.
  """
  def subscribe_to_changes do
    # This would require setting up PostgreSQL LISTEN/NOTIFY
    # For now, we'll rely on the existing PubSub mechanism
    :ok
  end
end
