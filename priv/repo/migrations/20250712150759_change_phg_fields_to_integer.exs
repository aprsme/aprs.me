defmodule Aprsme.Repo.Migrations.ChangePhgFieldsToInteger do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      modify :phg_power, :integer, from: :float
      modify :phg_height, :integer, from: :float
      modify :phg_gain, :integer, from: :float
    end
  end
end
