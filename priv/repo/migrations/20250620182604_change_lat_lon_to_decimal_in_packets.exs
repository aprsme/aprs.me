defmodule Aprs.Repo.Migrations.ChangeLatLonToDecimalInPackets do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      modify :lat, :decimal, precision: 10, scale: 6
      modify :lon, :decimal, precision: 10, scale: 6
    end
  end
end
