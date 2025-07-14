defmodule Aprsme.Repo.Migrations.AddPerformanceIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Compound index for callsign history queries
    create_if_not_exists index(:packets, [:base_callsign, "received_at DESC"],
                           name: :packets_base_callsign_received_at_idx,
                           concurrently: true
                         )

    # Covering index for sender queries to avoid table lookups
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_covering_idx 
    ON packets(sender, received_at DESC) 
    INCLUDE (lat, lon, data_type, has_position)
    """

    # Partial index for weather packets (without time constraint for immutability)
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_weather_recent_idx 
    ON packets(received_at DESC) 
    WHERE data_type IN ('weather', 'Weather', 'WX', 'wx')
    """

    # Spatial index for location queries
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_location_recent_idx 
    ON packets USING GIST (location) 
    WHERE has_position = true
    """

    # Index for device lookups
    create_if_not_exists index(:packets, [:device_identifier, :received_at],
                           name: :packets_device_received_idx,
                           concurrently: true
                         )

    # Index for cleanup worker queries (ascending for old packets)
    create_if_not_exists index(:packets, [:received_at],
                           name: :packets_received_at_asc_idx,
                           concurrently: true
                         )
  end

  def down do
    drop_if_exists index(:packets, [:base_callsign, :received_at],
                     name: :packets_base_callsign_received_at_idx
                   )

    execute "DROP INDEX IF EXISTS packets_sender_received_covering_idx"
    execute "DROP INDEX IF EXISTS packets_weather_recent_idx"
    execute "DROP INDEX IF EXISTS packets_location_recent_idx"

    drop_if_exists index(:packets, [:device_identifier, :received_at],
                     name: :packets_device_received_idx
                   )

    drop_if_exists index(:packets, [:received_at], name: :packets_received_at_asc_idx)
  end
end
