defmodule Aprsme.Repo.Migrations.OptimizeMapQueries do
  use Ecto.Migration
  @disable_ddl_transaction true

  def up do
    # Add compound index for the most common map query pattern
    create_if_not_exists index(:packets, [:has_position, :received_at])
    create_if_not_exists index(:packets, [:sender, :received_at])
    create_if_not_exists index(:packets, [:sender, :has_position])
    create_if_not_exists index(:packets, [:data_type])
    create_if_not_exists index(:packets, [:data_type, :received_at])
    create_if_not_exists index(:packets, [:symbol_table_id, :symbol_code, :received_at])
    create_if_not_exists index(:packets, [:device_identifier])
    create_if_not_exists index(:packets, [:region, :has_position, :received_at])
  end

  def down do
    drop index(:packets, [:region, :has_position, :received_at])
    drop index(:packets, [:device_identifier])
    drop index(:packets, [:symbol_table_id, :symbol_code, :received_at])
    drop index(:packets, [:data_type, :received_at])
    drop index(:packets, [:data_type])
    drop index(:packets, [:sender, :has_position])
    drop index(:packets, [:sender, :received_at])
    drop index(:packets, [:has_position, :received_at])
  end
end
