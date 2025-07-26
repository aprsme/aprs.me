defmodule Aprsme.Repo.Migrations.AddOptimizedIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # 1. Functional index for case-insensitive callsign searches
    # This replaces the need for upper() in queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_sender_upper 
    ON packets (upper(sender))
    """

    # 2. Composite index for position packets with time filtering
    # Optimizes common map queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_position_time 
    ON packets (has_position, received_at DESC)
    WHERE has_position = true
    """

    # 3. Spatial index for geography-based distance queries
    # Needed for ST_DWithin queries with geography cast
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_location_geography 
    ON packets USING GIST ((location::geography))
    """

    # 4. Base callsign with time for deduplication queries
    # Optimizes DISTINCT ON queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_base_callsign_time 
    ON packets (base_callsign, received_at DESC)
    """

    # 5. Partial index for weather packets
    # Significantly speeds up weather data queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_weather 
    ON packets (sender, received_at DESC)
    WHERE temperature IS NOT NULL 
       OR humidity IS NOT NULL 
       OR pressure IS NOT NULL 
       OR wind_speed IS NOT NULL 
       OR wind_direction IS NOT NULL 
       OR rain_1h IS NOT NULL
    """

    # 6. Composite index for packets with positions (for path queries)
    # Optimizes RF path visualization
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_sender_position 
    ON packets (sender, received_at DESC)
    WHERE lat IS NOT NULL AND lon IS NOT NULL
    """

    # 7. Add has_weather computed column for better query performance
    execute """
    ALTER TABLE packets 
    ADD COLUMN IF NOT EXISTS has_weather boolean 
    GENERATED ALWAYS AS (
      temperature IS NOT NULL 
      OR humidity IS NOT NULL 
      OR pressure IS NOT NULL 
      OR wind_speed IS NOT NULL 
      OR wind_direction IS NOT NULL 
      OR rain_1h IS NOT NULL
    ) STORED
    """

    # 8. Index on the new has_weather column
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_has_weather 
    ON packets (has_weather, sender, received_at DESC)
    WHERE has_weather = true
    """

    # 9. Composite index for region-based queries with position
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_region_position_time 
    ON packets (region, has_position, received_at DESC)
    WHERE has_position = true AND region IS NOT NULL
    """

    # 10. Index for data_type with sender and time
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_datatype_sender_time 
    ON packets (data_type, sender, received_at DESC)
    """

    # Analyze the table to update statistics after adding indexes
    execute "ANALYZE packets;"
  end

  def down do
    # Drop indexes in reverse order
    execute "DROP INDEX IF EXISTS idx_packets_datatype_sender_time;"
    execute "DROP INDEX IF EXISTS idx_packets_region_position_time;"
    execute "DROP INDEX IF EXISTS idx_packets_has_weather;"
    execute "ALTER TABLE packets DROP COLUMN IF EXISTS has_weather;"
    execute "DROP INDEX IF EXISTS idx_packets_sender_position;"
    execute "DROP INDEX IF EXISTS idx_packets_weather;"
    execute "DROP INDEX IF EXISTS idx_packets_base_callsign_time;"
    execute "DROP INDEX IF EXISTS idx_packets_location_geography;"
    execute "DROP INDEX IF EXISTS idx_packets_position_time;"
    execute "DROP INDEX IF EXISTS idx_packets_sender_upper;"
  end
end
