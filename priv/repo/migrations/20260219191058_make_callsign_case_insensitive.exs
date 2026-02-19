defmodule Aprsme.Repo.Migrations.MakeCallsignCaseInsensitive do
  use Ecto.Migration

  def change do
    # Drop the existing case-sensitive unique index
    drop unique_index(:users, [:callsign])

    # Create a case-insensitive unique index using LOWER()
    create unique_index(:users, ["lower(callsign)"], name: :users_callsign_lower_index)
  end
end
