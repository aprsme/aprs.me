defmodule Aprsme.Repo.Migrations.AddWeatherIndexes do
  use Ecto.Migration

  def up do
    # Index for finding stations with weather data
    create index(:packets, [:temperature], where: "temperature IS NOT NULL")

    # Compound index for time-based weather queries
    create index(:packets, [:received_at, :temperature], where: "temperature IS NOT NULL")

    # Additional weather-related indexes for common queries
    create index(:packets, [:sender, :temperature], where: "temperature IS NOT NULL")
    create index(:packets, [:humidity], where: "humidity IS NOT NULL")
    create index(:packets, [:pressure], where: "pressure IS NOT NULL")
    create index(:packets, [:wind_speed], where: "wind_speed IS NOT NULL")
  end

  def down do
    drop index(:packets, [:temperature])
    drop index(:packets, [:received_at, :temperature])
    drop index(:packets, [:sender, :temperature])
    drop index(:packets, [:humidity])
    drop index(:packets, [:pressure])
    drop index(:packets, [:wind_speed])
  end
end
