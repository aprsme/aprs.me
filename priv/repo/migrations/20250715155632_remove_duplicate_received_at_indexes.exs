defmodule Aprsme.Repo.Migrations.RemoveDuplicateReceivedAtIndexes do
  use Ecto.Migration

  def up do
    # Remove duplicate indexes on received_at column
    # Keep only packets_received_at_idx which is the most general purpose
    drop_if_exists index(:packets, [:received_at], name: :packets_received_at_asc_idx)
    drop_if_exists index(:packets, [:received_at], name: :packets_recent_hour_idx)
  end

  def down do
    # Recreate the removed indexes
    create_if_not_exists index(:packets, [:received_at],
                           name: :packets_received_at_asc_idx,
                           comment: "Index for cleanup worker queries (ascending for old packets)"
                         )

    create_if_not_exists index(:packets, [:received_at],
                           name: :packets_recent_hour_idx,
                           comment: "Index for recent packet queries"
                         )
  end
end
