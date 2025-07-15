defmodule Aprsme.Repo.Migrations.OptimizePacketCountersAutovacuum do
  use Ecto.Migration

  def up do
    # Set aggressive autovacuum settings for the packet_counters table
    # This table has only one row that gets updated frequently
    execute """
    ALTER TABLE packet_counters SET (
      autovacuum_vacuum_scale_factor = 0.0,
      autovacuum_vacuum_threshold = 100,
      autovacuum_analyze_scale_factor = 0.0,
      autovacuum_analyze_threshold = 100,
      autovacuum_vacuum_cost_delay = 0,
      autovacuum_vacuum_cost_limit = 10000
    );
    """

    # Add a fillfactor to reduce page splits during updates
    execute """
    ALTER TABLE packet_counters SET (fillfactor = 50);
    """

    # Note: VACUUM FULL cannot run in a transaction, so we'll skip it here
    # You can run it manually after the migration:
    # VACUUM FULL packet_counters;

    # Create a more efficient counter mechanism using unlogged table for high-frequency updates
    # This is an alternative approach if the above doesn't solve the bloat issue
    execute """
    COMMENT ON TABLE packet_counters IS 'Single-row table for packet counts. Configured with aggressive autovacuum due to high update frequency.';
    """
  end

  def down do
    # Reset to default autovacuum settings
    execute """
    ALTER TABLE packet_counters RESET (
      autovacuum_vacuum_scale_factor,
      autovacuum_vacuum_threshold,
      autovacuum_analyze_scale_factor,
      autovacuum_analyze_threshold,
      autovacuum_vacuum_cost_delay,
      autovacuum_vacuum_cost_limit,
      fillfactor
    );
    """

    execute """
    COMMENT ON TABLE packet_counters IS NULL;
    """
  end
end
