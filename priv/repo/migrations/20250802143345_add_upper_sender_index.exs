defmodule Aprsme.Repo.Migrations.AddUpperSenderIndex do
  use Ecto.Migration
  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    # Create functional index for case-insensitive sender searches
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_upper_sender 
    ON packets (upper(sender))
    """

    # Also add index for base_callsign which is frequently searched
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_upper_base_callsign 
    ON packets (upper(base_callsign))
    """

    # Add compound index for sender with received_at for efficient sorting
    execute """
    CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_upper_sender_received_at 
    ON packets (upper(sender), received_at DESC)
    """
  end

  def down do
    execute "DROP INDEX IF EXISTS idx_packets_upper_sender"
    execute "DROP INDEX IF EXISTS idx_packets_upper_base_callsign"
    execute "DROP INDEX IF EXISTS idx_packets_upper_sender_received_at"
  end
end
