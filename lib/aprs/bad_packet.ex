defmodule Aprs.BadPacket do
  @moduledoc false
  use Ecto.Schema

  import Ecto.Changeset
  import Ecto.Query

  schema "badpackets" do
    field :raw_packet, :string
    field :error_message, :string
    field :error_type, :string
    field :attempted_at, :utc_datetime_usec

    timestamps(type: :utc_datetime_usec)
  end

  @doc false
  @spec changeset(%Aprs.BadPacket{}, map()) :: Ecto.Changeset.t()
  def changeset(bad_packet, attrs) do
    bad_packet
    |> cast(attrs, [:raw_packet, :error_message, :error_type, :attempted_at])
    |> validate_required([:raw_packet])
  end

  @doc """
  Returns recent bad packets, ordered by attempted_at descending
  """
  @spec recent(Ecto.Queryable.t(), pos_integer()) :: Ecto.Query.t()
  def recent(query \\ __MODULE__, limit \\ 100) do
    from(b in query,
      order_by: [desc: b.attempted_at],
      limit: ^limit
    )
  end

  @doc """
  Returns bad packets by error type
  """
  @spec by_error_type(Ecto.Queryable.t(), String.t()) :: Ecto.Query.t()
  def by_error_type(query \\ __MODULE__, error_type) do
    from(b in query,
      where: b.error_type == ^error_type
    )
  end

  @doc """
  Returns count of bad packets in the last N hours
  """
  @spec count_recent(pos_integer()) :: Ecto.Query.t()
  def count_recent(hours \\ 24) do
    cutoff = DateTime.add(DateTime.utc_now(), -hours * 3600, :second)

    from(b in __MODULE__,
      where: b.attempted_at > ^cutoff,
      select: count(b.id)
    )
  end
end
