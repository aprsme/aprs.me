defmodule Aprsme.Repo.Migrations.CreateDevices do
  use Ecto.Migration

  def change do
    create table(:devices) do
      add :identifier, :string, null: false
      add :class, :string
      add :model, :string
      add :vendor, :string
      add :os, :string
      add :contact, :string
      add :features, {:array, :string}
      timestamps(type: :utc_datetime_usec, updated_at: :updated_at)
    end

    create unique_index(:devices, [:identifier])
  end
end
