defmodule Aprsme.Repo.Migrations.CompleteOptimizedIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Add has_weather column without GENERATED clause first
    # We'll populate it with a trigger instead
    execute """
    ALTER TABLE packets 
    ADD COLUMN IF NOT EXISTS has_weather boolean DEFAULT false
    """

    # Create trigger to maintain has_weather
    execute """
    CREATE OR REPLACE FUNCTION update_has_weather()
    RETURNS TRIGGER AS $$
    BEGIN
      NEW.has_weather := (
        NEW.temperature IS NOT NULL OR
        NEW.humidity IS NOT NULL OR
        NEW.pressure IS NOT NULL OR
        NEW.wind_speed IS NOT NULL OR
        NEW.wind_direction IS NOT NULL OR
        NEW.rain_1h IS NOT NULL
      );
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    DROP TRIGGER IF EXISTS update_has_weather_trigger ON packets;
    """

    execute """
    CREATE TRIGGER update_has_weather_trigger
    BEFORE INSERT OR UPDATE ON packets
    FOR EACH ROW
    EXECUTE FUNCTION update_has_weather();
    """

    # Update existing rows in batches to avoid timeout
    execute """
    UPDATE packets 
    SET has_weather = (
      temperature IS NOT NULL OR
      humidity IS NOT NULL OR
      pressure IS NOT NULL OR
      wind_speed IS NOT NULL OR
      wind_direction IS NOT NULL OR
      rain_1h IS NOT NULL
    )
    WHERE id IN (
      SELECT id FROM packets 
      WHERE has_weather IS NULL
      LIMIT 100000
    );
    """

    # Create remaining indexes
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_has_weather 
    ON packets (has_weather, sender, received_at DESC)
    WHERE has_weather = true
    """

    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_region_position_time 
    ON packets (region, has_position, received_at DESC)
    WHERE has_position = true AND region IS NOT NULL
    """

    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_datatype_sender_time 
    ON packets (data_type, sender, received_at DESC)
    """

    # Analyze the table to update statistics
    execute "ANALYZE packets;"
  end

  def down do
    execute "DROP INDEX IF EXISTS idx_packets_datatype_sender_time;"
    execute "DROP INDEX IF EXISTS idx_packets_region_position_time;"
    execute "DROP INDEX IF EXISTS idx_packets_has_weather;"
    execute "DROP TRIGGER IF EXISTS update_has_weather_trigger ON packets;"
    execute "DROP FUNCTION IF EXISTS update_has_weather();"
    execute "ALTER TABLE packets DROP COLUMN IF EXISTS has_weather;"
  end
end
