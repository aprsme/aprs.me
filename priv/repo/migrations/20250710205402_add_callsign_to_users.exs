defmodule Aprsme.Repo.Migrations.AddCallsignToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :callsign, :string, null: false
    end

    create unique_index(:users, [:callsign])
  end
end
