defmodule Aprsme.DataExtended do
  @moduledoc false
  use Ecto.Schema

  import Ecto.Changeset

  alias Aprsme.DataExtended

  embedded_schema do
    field :aprs_messaging, :boolean, default: false
    field :comment, :string
    field :data_type, :string
    field :latitude, :decimal
    field :longitude, :decimal
    field :symbol_code, :string
    field :symbol_table_id, :string
  end

  @type t :: %__MODULE__{}

  @doc false
  @spec changeset(Aprsme.DataExtended.t(), map()) :: Ecto.Changeset.t()
  def changeset(%DataExtended{} = data_extended, attrs) do
    data_extended
    |> cast(attrs, [
      :aprs_messaging,
      :comment,
      :data_type,
      :latitude,
      :longitude,
      :symbol_code,
      :symbol_table_id
    ])
    |> validate_required([
      :aprs_messaging,
      :comment,
      :data_type,
      :symbol_code,
      :symbol_table_id
    ])
    |> validate_required_if_present(:latitude)
    |> validate_required_if_present(:longitude)
  end

  defp validate_required_if_present(changeset, field) do
    if Map.has_key?(changeset.changes, field) do
      validate_required(changeset, [field])
    else
      changeset
    end
  end
end
