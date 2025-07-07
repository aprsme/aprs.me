defmodule Aprsme.Repo.Migrations.AddCountOptimizationIndex do
  use Ecto.Migration
  @disable_ddl_transaction true

  def up do
    # Add a partial index on id that will be used for COUNT(*) queries
    # This creates a small, fast index that PostgreSQL can use for counting
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_count_idx
    ON packets (id)
    WHERE id IS NOT NULL
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_count_idx"
  end
end
