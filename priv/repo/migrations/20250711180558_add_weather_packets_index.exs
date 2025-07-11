defmodule Aprsme.Repo.Migrations.AddWeatherPacketsIndex do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Add partial index for weather packets using CONCURRENTLY for better performance
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_weather_idx
    ON packets (received_at DESC)
    WHERE data_type = 'weather'
    OR (symbol_table_id = '/' AND symbol_code = '_')
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_weather_idx"
  end
end
