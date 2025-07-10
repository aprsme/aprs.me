defmodule Aprsme.Repo.Migrations.AddSenderLowerIndex do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Add functional index for case-insensitive sender searches
    # This helps if we ever need to do ILIKE queries
    execute """
            CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_lower_idx 
            ON packets (LOWER(sender))
            WHERE sender IS NOT NULL;
            """,
            ""

    # Also add a trigram index for more flexible pattern matching if needed
    # Note: This requires pg_trgm extension
    execute "CREATE EXTENSION IF NOT EXISTS pg_trgm;", ""

    execute """
            CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_trgm_idx
            ON packets USING gin (sender gin_trgm_ops)
            WHERE sender IS NOT NULL;
            """,
            ""
  end

  def down do
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_sender_lower_idx;", ""
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_sender_trgm_idx;", ""
  end
end
