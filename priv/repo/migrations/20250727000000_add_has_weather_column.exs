defmodule Aprsme.Repo.Migrations.AddHasWeatherColumn do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # This migration adds a generated column which requires a full table rewrite
    # It should be run during a maintenance window or when load is low
    
    # Set aggressive timeout for very large table
    execute "SET statement_timeout = '2h';"
    execute "SET lock_timeout = '30s';"
    
    # Log progress
    execute "SELECT NOW() AS start_time, 'Starting has_weather column addition' AS status;"
    
    # Add the column - this will rewrite the entire table
    # Consider running this during off-peak hours
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
    
    # Create index on the new column
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_has_weather 
    ON packets (has_weather, sender, received_at DESC)
    WHERE has_weather = true
    """
    
    # Drop the functional index if it exists (replaced by column-based index)
    execute "DROP INDEX IF EXISTS idx_packets_has_weather_functional;"
    
    # Update statistics
    execute "ANALYZE packets (has_weather);"
    
    # Reset timeouts
    execute "RESET statement_timeout;"
    execute "RESET lock_timeout;"
    
    # Log completion
    execute "SELECT NOW() AS end_time, 'Completed has_weather column addition' AS status;"
  end

  def down do
    execute "SET statement_timeout = '2h';"
    execute "SET lock_timeout = '30s';"
    
    # Drop the index first
    execute "DROP INDEX IF EXISTS idx_packets_has_weather;"
    
    # Drop the column (this will also rewrite the table)
    execute "ALTER TABLE packets DROP COLUMN IF EXISTS has_weather;"
    
    # Recreate the functional index
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_has_weather_functional 
    ON packets (sender, received_at DESC)
    WHERE (temperature IS NOT NULL 
       OR humidity IS NOT NULL 
       OR pressure IS NOT NULL 
       OR wind_speed IS NOT NULL 
       OR wind_direction IS NOT NULL 
       OR rain_1h IS NOT NULL)
    """
    
    execute "RESET statement_timeout;"
    execute "RESET lock_timeout;"
  end
end