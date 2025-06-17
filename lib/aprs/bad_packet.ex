defmodule Aprs.BadPacket do
  @moduledoc false
  use Ecto.Schema

  import Ecto.Changeset

  schema "badpackets" do
    field :raw_packet, :string
    field :error_message, :string
    field :error_type, :string
    field :attempted_at, :utc_datetime_usec

    timestamps(type: :utc_datetime_usec)
  end

  @doc false
  def changeset(bad_packet, attrs) do
    bad_packet
    |> cast(attrs, [:raw_packet, :error_message, :error_type, :attempted_at])
    |> validate_required([:raw_packet])
  end
end
