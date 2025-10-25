defmodule Aprsme.Repo.Migrations.AddCallsignSearchIndexes do
  use Ecto.Migration

  def up do
    # In test environment, skip index creation to avoid timeout issues
    # Indexes will be created in production where the table has actual data
    if Mix.env() != :test do
      # Add index on sender for pattern matching searches
      # Using text_pattern_ops for LIKE/ILIKE queries via raw SQL
      execute """
      CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_pattern_idx
      ON packets (sender text_pattern_ops)
      """

      # Add index on base_callsign for pattern matching
      execute """
      CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_base_callsign_pattern_idx
      ON packets (base_callsign text_pattern_ops)
      """

      # Add composite index for sender + received_at for efficient sorting
      execute """
      CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_at_idx
      ON packets (sender, received_at)
      """
    end
  end

  def down do
    execute "DROP INDEX IF EXISTS packets_sender_pattern_idx"
    execute "DROP INDEX IF EXISTS packets_base_callsign_pattern_idx"
    execute "DROP INDEX IF EXISTS packets_sender_received_at_idx"
  end
end
