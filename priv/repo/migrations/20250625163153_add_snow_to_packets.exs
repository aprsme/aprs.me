defmodule Aprsme.Repo.Migrations.AddSnowToPackets do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      add :snow, :float
    end
  end
end
