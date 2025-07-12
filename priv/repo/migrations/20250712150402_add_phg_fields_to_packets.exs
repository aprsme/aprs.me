defmodule Aprsme.Repo.Migrations.AddPhgFieldsToPackets do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      add :phg_power, :float
      add :phg_height, :float
      add :phg_gain, :float
      add :phg_directivity, :integer
    end

    # Create indexes for PHG fields to support queries by power/height/etc
    create index(:packets, [:phg_power])
    create index(:packets, [:phg_height])
  end
end
