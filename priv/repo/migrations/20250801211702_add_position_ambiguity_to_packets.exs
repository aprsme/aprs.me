defmodule Aprsme.Repo.Migrations.AddPositionAmbiguityToPackets do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      add :position_ambiguity, :integer
    end
  end
end
