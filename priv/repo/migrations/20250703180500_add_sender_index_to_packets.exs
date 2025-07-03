defmodule Aprsme.Repo.Migrations.AddSenderIndexToPackets do
  use Ecto.Migration

  def change do
    # Add index on sender field for efficient callsign filtering
    create index(:packets, [:sender])

    # Add compound index for sender + received_at for efficient time-based callsign queries
    create index(:packets, [:sender, :received_at])

    # Add compound index for sender + has_position for weather packet queries
    create index(:packets, [:sender, :has_position])
  end
end
