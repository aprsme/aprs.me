defmodule Aprs.Repo.Migrations.EnablePostgisAndMigrateLocationData do
  use Ecto.Migration

  def up do
    # Enable PostGIS extension
    execute("CREATE EXTENSION IF NOT EXISTS postgis")

    # Add geometry column for storing point data with SRID 4326 (WGS84)
    alter table(:packets) do
      add(:location, :geometry, null: true)
    end

    # Create a spatial index on the location column for efficient spatial queries
    execute("CREATE INDEX packets_location_idx ON packets USING GIST (location)")

    # Migrate existing lat/lon data to PostGIS geometry points
    # Only migrate records that have both lat and lon values
    execute("""
    UPDATE packets
    SET location = ST_SetSRID(ST_MakePoint(lon, lat), 4326)
    WHERE lat IS NOT NULL
      AND lon IS NOT NULL
      AND lat BETWEEN -90 AND 90
      AND lon BETWEEN -180 AND 180
    """)

    # Add constraint to ensure valid geometry (optional but recommended)
    execute("""
    ALTER TABLE packets
    ADD CONSTRAINT packets_location_valid
    CHECK (ST_IsValid(location) OR location IS NULL)
    """)

    # Update has_position field based on the new location column
    execute("""
    UPDATE packets
    SET has_position = (location IS NOT NULL)
    """)

    # Add index on has_position for efficient filtering
    create_if_not_exists(index(:packets, [:has_position]))

    # Add compound index for common queries (has_position + received_at)
    create_if_not_exists(index(:packets, [:has_position, :received_at]))

    # Add spatial index with additional filters for common APRS queries
    # Note: Using a fixed timestamp instead of NOW() to make it immutable
    execute("""
    CREATE INDEX IF NOT EXISTS packets_location_recent_idx
    ON packets USING GIST (location)
    WHERE has_position = true
    """)
  end

  def down do
    # Remove spatial indexes
    execute("DROP INDEX IF EXISTS packets_location_recent_idx")
    drop_if_exists(index(:packets, [:has_position, :received_at]))
    drop_if_exists(index(:packets, [:has_position]))

    # Remove constraint
    execute("ALTER TABLE packets DROP CONSTRAINT IF EXISTS packets_location_valid")

    # Remove spatial index
    execute("DROP INDEX IF EXISTS packets_location_idx")

    # Add back lat/lon columns for rollback
    alter table(:packets) do
      add(:lat, :float)
      add(:lon, :float)
    end

    # Restore lat/lon data from geometry
    execute("""
    UPDATE packets
    SET lat = ST_Y(location), lon = ST_X(location)
    WHERE location IS NOT NULL
    """)

    # Remove geometry column
    alter table(:packets) do
      remove(:location)
    end

    # Note: We don't drop the PostGIS extension as other parts of the system might use it
    # execute("DROP EXTENSION IF EXISTS postgis")
  end
end
