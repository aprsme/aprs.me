defmodule Aprsme.Repo.Migrations.CreateBadpackets do
  use Ecto.Migration

  def change do
    create table(:badpackets) do
      add :raw_packet, :text, null: false
      add :error_message, :text
      add :error_type, :string
      add :attempted_at, :utc_datetime_usec

      timestamps(type: :utc_datetime_usec)
    end

    create index(:badpackets, [:attempted_at])
  end
end
