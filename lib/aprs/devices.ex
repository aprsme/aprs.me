defmodule Aprs.Devices do
  use Ecto.Schema
  import Ecto.Changeset

  schema "devices" do
    field :identifier, :string
    field :class, :string
    field :model, :string
    field :vendor, :string
    field :os, :string
    field :contact, :string
    field :features, {:array, :string}
    timestamps(updated_at: :updated_at)
  end

  def changeset(device, attrs) do
    device
    |> cast(attrs, [:identifier, :class, :model, :vendor, :os, :contact, :features])
    |> validate_required([:identifier])
    |> unique_constraint(:identifier)
  end
end
