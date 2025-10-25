defmodule Aprsme.Repo.Migrations.AddCallsignSearchIndexes do
  use Ecto.Migration
  @disable_ddl_transaction true

  def change do
    # Add index on sender for pattern matching searches
    # Using text_pattern_ops for LIKE/ILIKE queries via raw SQL
    execute(
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_pattern_idx ON packets (sender text_pattern_ops)",
      "DROP INDEX IF EXISTS packets_sender_pattern_idx"
    )

    # Add index on base_callsign for pattern matching
    execute(
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_base_callsign_pattern_idx ON packets (base_callsign text_pattern_ops)",
      "DROP INDEX IF EXISTS packets_base_callsign_pattern_idx"
    )

    # Add composite index for sender + received_at for efficient sorting
    create_if_not_exists index(:packets, [:sender, :received_at],
                           name: :packets_sender_received_at_idx
                         )
  end
end
