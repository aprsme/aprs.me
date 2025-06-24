defmodule Aprsme.Repo.Migrations.AddDeviceIdentifierToPackets do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      add :device_identifier, :string
    end
  end
end
