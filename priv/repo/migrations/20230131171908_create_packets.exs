defmodule Aprs.Repo.Migrations.CreatePackets do
  use Ecto.Migration

  def change do
    create table(:packets, primary_key: false) do
      add(:id, :binary_id, primary_key: true)
      add(:base_callsign, :string)
      add(:data_type, :string)
      add(:destination, :string)
      add(:information_field, :string)
      add(:path, :string)
      add(:sender, :string)
      add(:ssid, :string)
      add(:data_extended, :map)

      timestamps()
    end
  end
end
