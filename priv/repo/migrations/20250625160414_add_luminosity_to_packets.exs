defmodule Aprsme.Repo.Migrations.AddLuminosityToPackets do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      add :luminosity, :integer
    end
  end
end
