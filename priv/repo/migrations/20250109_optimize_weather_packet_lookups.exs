defmodule Aprsme.Repo.Migrations.OptimizeWeatherPacketLookups do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Create a covering index for the has_weather_packets? query
    # This index covers all fields needed by the query, avoiding table lookups
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_weather_lookup_idx
    ON packets (UPPER(sender), received_at DESC)
    INCLUDE (data_type, symbol_table_id, symbol_code)
    WHERE data_type = 'weather' 
       OR (symbol_table_id = '/' AND symbol_code = '_')
       OR (symbol_table_id = '\\' AND symbol_code = '_')
    """

    # Also create a more general index for all sender lookups with weather data
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_at_covering_idx
    ON packets (UPPER(sender), received_at DESC)
    INCLUDE (data_type, symbol_table_id, symbol_code)
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_sender_received_at_covering_idx"
    execute "DROP INDEX IF EXISTS packets_sender_weather_lookup_idx"
  end
end
