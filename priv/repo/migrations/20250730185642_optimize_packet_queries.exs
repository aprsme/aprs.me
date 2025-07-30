defmodule Aprsme.Repo.Migrations.OptimizePacketQueries do
  use Ecto.Migration

  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Index for path pattern matching (used in heard_by queries)
    # Using btree with text_pattern_ops for LIKE queries since trigram might not be available
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_path_pattern 
    ON packets(path text_pattern_ops)
    WHERE path IS NOT NULL AND path != ''
    """

    # Composite index for base_callsign lookups with time
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_base_callsign_time 
    ON packets(base_callsign, received_at DESC)
    WHERE base_callsign IS NOT NULL
    """

    # Index for device_identifier lookups
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_device_identifier 
    ON packets(device_identifier)
    WHERE device_identifier IS NOT NULL
    """

    # Partial index for recent packets (last 7 days) - very useful for most queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_recent_only 
    ON packets(received_at DESC)
    WHERE received_at > NOW() - INTERVAL '7 days'
    """

    # Index for weather packet lookups by callsign
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_weather_by_callsign
    ON packets(sender, received_at DESC)
    WHERE (temperature IS NOT NULL 
       OR humidity IS NOT NULL 
       OR pressure IS NOT NULL 
       OR wind_speed IS NOT NULL 
       OR wind_direction IS NOT NULL 
       OR rain_1h IS NOT NULL)
    """

    # Optimize existing indexes by updating statistics
    execute "ANALYZE packets"

    # Increase statistics target for frequently queried columns
    execute "ALTER TABLE packets ALTER COLUMN sender SET STATISTICS 1000"
    execute "ALTER TABLE packets ALTER COLUMN received_at SET STATISTICS 1000"
    execute "ALTER TABLE packets ALTER COLUMN base_callsign SET STATISTICS 500"

    # Create a compound index for the common "latest packet per callsign" query pattern
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_sender_id_desc
    ON packets(sender, id DESC)
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS idx_packets_path_pattern"
    execute "DROP INDEX IF EXISTS idx_packets_base_callsign_time"
    execute "DROP INDEX IF EXISTS idx_packets_device_identifier"
    execute "DROP INDEX IF EXISTS idx_packets_recent_only"
    execute "DROP INDEX IF EXISTS idx_packets_weather_by_callsign"
    execute "DROP INDEX IF EXISTS idx_packets_sender_id_desc"

    # Reset statistics to default
    execute "ALTER TABLE packets ALTER COLUMN sender SET STATISTICS DEFAULT"
    execute "ALTER TABLE packets ALTER COLUMN received_at SET STATISTICS DEFAULT"
    execute "ALTER TABLE packets ALTER COLUMN base_callsign SET STATISTICS DEFAULT"
  end
end
