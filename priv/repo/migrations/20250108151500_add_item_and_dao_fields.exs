defmodule Aprsme.Repo.Migrations.AddItemAndDaoFields do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # For APRS items/objects
      add :item_name, :string
      add :object_name, :string
      add :is_item, :boolean, default: false
      add :is_object, :boolean, default: false

      # DAO extension data for extra position precision
      add :dao, :map
    end

    # Add indexes for querying items/objects
    create index(:packets, [:is_item])
    create index(:packets, [:is_object])
    create index(:packets, [:item_name])
    create index(:packets, [:object_name])
  end
end
