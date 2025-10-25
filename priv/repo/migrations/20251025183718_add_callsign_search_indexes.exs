defmodule Aprsme.Repo.Migrations.AddCallsignSearchIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Check if we're in a test environment by looking at the repo config
    # In test, the pool is Ecto.Adapters.SQL.Sandbox which doesn't support CONCURRENTLY
    pool_module = Aprsme.Repo.config()[:pool]

    if pool_module == Ecto.Adapters.SQL.Sandbox do
      # Test environment - skip index creation to avoid timeout issues
      # The test database is empty anyway
      :ok
    else
      # Production/dev environment - create indexes with CONCURRENTLY
      # Add index on sender for pattern matching searches
      # Using text_pattern_ops for LIKE/ILIKE queries via raw SQL
      execute """
      CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_pattern_idx
      ON packets (sender text_pattern_ops)
      """

      # Add index on base_callsign for pattern matching
      execute """
      CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_base_callsign_pattern_idx
      ON packets (base_callsign text_pattern_ops)
      """

      # Add composite index for sender + received_at for efficient sorting
      execute """
      CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_at_idx
      ON packets (sender, received_at)
      """
    end
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_sender_pattern_idx"
    execute "DROP INDEX IF EXISTS packets_base_callsign_pattern_idx"
    execute "DROP INDEX IF EXISTS packets_sender_received_at_idx"
  end
end
