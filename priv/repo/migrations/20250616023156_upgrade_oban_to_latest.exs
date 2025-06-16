defmodule Aprs.Repo.Migrations.UpgradeObanToLatest do
  use Ecto.Migration

  def up do
    # First, drop the existing migration and start fresh
    # This ensures we get all the latest schema changes
    Oban.Migrations.up()
  end

  def down do
    # This will rollback to the previous state
    Oban.Migrations.down()
  end
end
