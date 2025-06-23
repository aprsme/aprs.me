defmodule Aprs.Repo.Migrations.UpdateDevicesTimestampsToUtcUsec do
  use Ecto.Migration

  def change do
    alter table(:devices) do
      modify :inserted_at, :utc_datetime_usec, null: false
      modify :updated_at, :utc_datetime_usec, null: false
    end
  end
end
