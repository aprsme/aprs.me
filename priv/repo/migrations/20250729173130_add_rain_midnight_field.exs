defmodule Aprsme.Repo.Migrations.AddRainMidnightField do
  use Ecto.Migration

  def up do
    # Add the missing rain_midnight field
    alter table(:packets) do
      add :rain_midnight, :float
    end

    # Copy data from rain_since_midnight to rain_midnight if rain_since_midnight exists
    execute """
    DO $$
    BEGIN
      IF EXISTS (SELECT 1 FROM information_schema.columns 
                 WHERE table_name = 'packets' AND column_name = 'rain_since_midnight') THEN
        UPDATE packets SET rain_midnight = rain_since_midnight WHERE rain_since_midnight IS NOT NULL;
      END IF;
    END $$;
    """
  end

  def down do
    alter table(:packets) do
      remove :rain_midnight
    end
  end
end