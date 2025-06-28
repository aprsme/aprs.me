defmodule Aprsme.Repo.Migrations.BackfillLocationFieldForExistingPackets do
  use Ecto.Migration

  def up do
    # Update packets that have lat/lon but no location field
    execute """
    UPDATE packets
    SET location = ST_SetSRID(ST_MakePoint(lon, lat), 4326)
    WHERE lat IS NOT NULL
      AND lon IS NOT NULL
      AND location IS NULL
      AND has_position = true;
    """
  end

  def down do
    # Clear location field (if needed for rollback)
    execute """
    UPDATE packets
    SET location = NULL
    WHERE location IS NOT NULL;
    """
  end
end
