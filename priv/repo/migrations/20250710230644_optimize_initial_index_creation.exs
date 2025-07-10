defmodule Aprsme.Repo.Migrations.OptimizeInitialIndexCreation do
  use Ecto.Migration
  @disable_ddl_transaction true

  def up do
    # Drop and recreate the count optimization index more efficiently
    execute "DROP INDEX IF EXISTS packets_count_idx"

    # Use a simpler approach - just create the index normally for initial setup
    # The CONCURRENTLY option is mainly useful in production with live data
    execute """
    CREATE INDEX packets_count_idx
    ON packets (id)
    WHERE id IS NOT NULL
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_count_idx"
  end
end
