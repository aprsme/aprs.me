defmodule Aprsme.Repo.Migrations.AddAdditionalPerformanceIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Add compound indexes for common query patterns
    create_if_not_exists index(:packets, [:received_at, :has_position, :data_type],
                           concurrently: true
                         )

    create_if_not_exists index(:packets, [:sender, :data_type, :received_at], concurrently: true)

    # Add index for weather symbol combinations (frequently used in queries)
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_weather_symbol_idx
    ON packets (received_at DESC, lat, lon)
    WHERE (symbol_table_id = '/' AND symbol_code = '_')
    OR (symbol_table_id = '\\' AND symbol_code = '_')
    """

    # Add partial index for recent packets with position (most common map query)
    # Note: Removing time-based predicate as it's not immutable
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_recent_positioned_idx
    ON packets (received_at DESC, lat, lon, sender)
    WHERE has_position = true
    """

    # Add index for callsign case-insensitive searches
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_upper_idx
    ON packets (UPPER(sender), received_at DESC)
    """

    # Add composite index for region-based queries
    create_if_not_exists index(:packets, [:region, :data_type, :received_at], concurrently: true)

    # Add index for device lookups (performance optimization)
    create_if_not_exists index(:packets, [:device_identifier, :received_at], concurrently: true)
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_device_identifier_received_at_idx"
    execute "DROP INDEX IF EXISTS packets_region_data_type_received_at_idx"
    execute "DROP INDEX IF EXISTS packets_sender_upper_idx"
    execute "DROP INDEX IF EXISTS packets_recent_positioned_idx"
    execute "DROP INDEX IF EXISTS packets_weather_symbol_idx"
    execute "DROP INDEX IF EXISTS packets_sender_data_type_received_at_idx"
    execute "DROP INDEX IF EXISTS packets_received_at_has_position_data_type_idx"
  end
end
