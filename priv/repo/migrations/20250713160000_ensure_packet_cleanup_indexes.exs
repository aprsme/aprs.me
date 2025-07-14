defmodule Aprsme.Repo.Migrations.EnsurePacketCleanupIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Ensure index on received_at for efficient time-based deletes
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_received_at_idx
    ON packets (received_at)
    """

    # Ensure index on id for efficient batch deletes
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_id_idx
    ON packets (id)
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_received_at_idx"
    execute "DROP INDEX IF EXISTS packets_id_idx"
  end
end
