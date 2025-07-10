defmodule Aprsme.Repo.Migrations.OptimizeInfoWeatherPages do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Critical index for get_other_ssids function on info page
    create_if_not_exists(
      index(:packets, [:base_callsign, :received_at],
        name: :packets_base_callsign_received_at_idx,
        concurrently: true,
        comment: "Index for finding other SSIDs of same base callsign"
      )
    )

    # Add spatial index for get_nearby_stations if not exists
    # This is crucial for the ST_Distance calculations
    execute """
            CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_location_spatial_idx 
            ON packets USING GIST (location)
            WHERE has_position = true;
            """,
            ""

    # Optimize the get_nearby_stations query with a specialized index
    # This combines spatial and temporal filtering
    execute """
            CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_spatial_temporal_idx
            ON packets (received_at DESC, base_callsign)
            WHERE has_position = true;
            """,
            ""

    # Add index for weather page callsign history queries
    # This optimizes the initial load of weather data
    create_if_not_exists(
      index(:packets, [:sender, :data_type, :received_at],
        name: :packets_weather_history_idx,
        where: "data_type = 'weather'",
        concurrently: true,
        comment: "Optimized index for weather history queries"
      )
    )

    # Analyze tables to update statistics for query planner
    execute "ANALYZE packets;", ""
  end

  def down do
    drop_if_exists(
      index(:packets, [:base_callsign, :received_at],
        name: :packets_base_callsign_received_at_idx
      )
    )

    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_location_spatial_idx;", ""
    execute "DROP INDEX CONCURRENTLY IF EXISTS packets_spatial_temporal_idx;", ""

    drop_if_exists(
      index(:packets, [:sender, :data_type, :received_at], name: :packets_weather_history_idx)
    )
  end
end
