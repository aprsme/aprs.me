defmodule Aprsme.Repo.Migrations.RemoveDuplicateIndexes do
  use Ecto.Migration

  def up do
    # Remove duplicate indexes to save 2.9GB of disk space

    # 1. Remove packets_id_idx (1324 MB) - redundant with primary key packets_pkey
    # The primary key already provides an index on the id column
    execute("DROP INDEX IF EXISTS packets_id_idx")

    # 2. Remove packets_location_spatial_idx (1060 MB) - redundant with packets_location_selective_idx
    # The selective index is more specific and provides better performance
    execute("DROP INDEX IF EXISTS packets_location_spatial_idx")

    # 3. Remove packets_received_at_index (550 MB) - redundant with packets_received_at_idx
    # Both indexes serve the same purpose
    execute("DROP INDEX IF EXISTS packets_received_at_index")

    # Also check for other potential duplicates mentioned in earlier migrations
    # Replaced by more specific indexes
    execute("DROP INDEX IF EXISTS packets_location_idx")

    # Log the cleanup
    execute("""
    DO $$
    BEGIN
      RAISE NOTICE 'Duplicate indexes removed successfully. Estimated space saved: ~2.9GB';
    END $$;
    """)
  end

  def down do
    # Recreate indexes if needed (though they are redundant)

    # Recreate packets_id_idx
    execute("""
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_id_idx
    ON packets(id)
    """)

    # Recreate packets_location_spatial_idx
    execute("""
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_location_spatial_idx 
    ON packets USING GIST (location)
    """)

    # Recreate packets_received_at_index (using original name from migration)
    execute("""
    CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_received_at_index
    ON packets(received_at)
    """)
  end
end
