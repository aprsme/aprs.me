defmodule Aprsme.Repo.Migrations.OptimizeStationsHeardByQuery do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Enable the pg_trgm extension for trigram indexing
    execute "CREATE EXTENSION IF NOT EXISTS pg_trgm;", ""

    # Add index for path pattern matching to optimize the regex operations
    execute """
            CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_path_pattern_idx
            ON packets USING GIN (path gin_trgm_ops)
            WHERE path IS NOT NULL
              AND path != ''
              AND path !~ '^TCPIP'
              AND path !~ ',TCPIP';
            """,
            ""

    # Add composite index for the main filtering conditions
    execute """
            CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_stations_heard_idx
            ON packets (received_at DESC, sender, lat, lon)
            WHERE path IS NOT NULL
              AND path != ''
              AND path !~ '^TCPIP'
              AND path !~ ',TCPIP'
              AND lat IS NOT NULL
              AND lon IS NOT NULL;
            """,
            ""

    # Add index for digipeater location lookups
    execute """
            CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_digipeater_location_idx
            ON packets (sender, received_at DESC)
            WHERE location IS NOT NULL;
            """,
            ""

    # Analyze tables to update statistics for query planner
    execute "ANALYZE packets;", ""
  end

  def down do
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_path_pattern_idx;", ""
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_stations_heard_idx;", ""
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_digipeater_location_idx;", ""
    # Note: We don't drop the pg_trgm extension as it might be used by other parts of the system
  end
end
