defmodule Aprsme.Repo.Migrations.OptimizeInsertPerformance do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Drop heavy indexes that slow down INSERTs and recreate with better selectivity

    # Drop the covering index - it's expensive for INSERTs and rarely used
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_sender_received_covering_idx"

    # Drop the weather index and recreate with more selectivity
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_weather_recent_idx"

    # Create a more selective weather index that only includes recent data
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_weather_selective_idx 
    ON packets(received_at DESC) 
    WHERE data_type IN ('weather', 'Weather', 'WX', 'wx')
    """

    # Create a partial index for device lookups that only includes recent data
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_device_received_idx"

    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_device_recent_idx 
    ON packets(device_identifier, received_at DESC) 
    WHERE device_identifier IS NOT NULL
    """

    # Optimize the spatial index to be partial for better INSERT performance
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_location_recent_idx"

    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_location_selective_idx 
    ON packets USING GIST (location) 
    WHERE has_position = true
    """

    # Add a lightweight index just for the most common query pattern - recent data only
    create_if_not_exists index(:packets, [:received_at],
                           name: :packets_recent_hour_idx,
                           concurrently: true
                         )
  end

  def down do
    # Restore the original indexes
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_covering_idx 
    ON packets(sender, received_at DESC) 
    INCLUDE (lat, lon, data_type, has_position)
    """

    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_weather_recent_idx 
    ON packets(received_at DESC) 
    WHERE data_type IN ('weather', 'Weather', 'WX', 'wx')
    """

    create_if_not_exists index(:packets, [:device_identifier, :received_at],
                           name: :packets_device_received_idx,
                           concurrently: true
                         )

    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_location_recent_idx 
    ON packets USING GIST (location) 
    WHERE has_position = true
    """

    # Drop the new selective indexes
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_weather_selective_idx"
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_device_recent_idx"
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_location_selective_idx"
    drop_if_exists index(:packets, [:received_at], name: :packets_recent_hour_idx)
  end
end
