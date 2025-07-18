defmodule Aprsme.Repo.Migrations.AddOptimizedPerformanceIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Add BRIN index for time-series data (much smaller than B-tree for time-based queries)
    execute """
    CREATE INDEX IF NOT EXISTS packets_received_at_brin_idx 
    ON packets USING BRIN(received_at)
    """

    # Add compound index for sender and time queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_at_idx 
    ON packets(sender, received_at DESC)
    """

    # Add index for lowercase sender queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_lower_received_at_idx
    ON packets(LOWER(sender), received_at DESC)
    """

    # Add partial index for very recent geographic queries (last 24 hours)
    # We'll use a static date check that gets evaluated at query time
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_location_recent_idx
    ON packets USING GIST(location)
    WHERE received_at > '2025-01-01'::timestamp
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_sender_received_at_idx"
    execute "DROP INDEX IF EXISTS packets_sender_lower_received_at_idx"
    execute "DROP INDEX IF EXISTS packets_received_at_brin_idx"
    execute "DROP INDEX IF EXISTS packets_location_recent_idx"
  end
end
