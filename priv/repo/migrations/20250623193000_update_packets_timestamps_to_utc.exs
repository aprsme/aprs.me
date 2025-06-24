defmodule Aprsme.Repo.Migrations.UpdatePacketsTimestampsToUtc do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      modify :inserted_at, :utc_datetime, null: false
      modify :updated_at, :utc_datetime, null: false
      modify :received_at, :utc_datetime, null: false
    end
  end
end
