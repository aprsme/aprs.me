defmodule Aprs.DataExtended do
  @moduledoc false
  use Ecto.Schema

  import Ecto.Changeset

  alias Aprs.DataExtended

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
  @spec changeset(Aprs.DataExtended.t(), map()) :: Ecto.Changeset.t()
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
      :latitude,
      :longitude,
      :symbol_code,
      :symbol_table_id
    ])
  end
end
