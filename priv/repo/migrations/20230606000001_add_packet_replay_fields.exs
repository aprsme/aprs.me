defmodule Aprsme.Repo.Migrations.AddPacketReplayFields do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # Timestamp when the packet was received (can be different from inserted_at)
      add :received_at, :utc_datetime_usec

      # Geographic region information to help with filtering/querying
      add :region, :string

      # Store lat/lon as separate fields for simpler queries
      add :lat, :float
      add :lon, :float

      # Add indices for efficient querying
      add :has_position, :boolean, default: false
    end

    # Index for time-based queries (useful for replay)
    create index(:packets, [:received_at])

    # Index for region-based filtering
    create index(:packets, [:region])

    # Index for filtering packets with position data
    create index(:packets, [:has_position])

    # Indices for geographic filtering
    create index(:packets, [:lat])
    create index(:packets, [:lon])

    # Compound index for efficient replay queries (region + time)
    create index(:packets, [:region, :received_at])
  end
end
