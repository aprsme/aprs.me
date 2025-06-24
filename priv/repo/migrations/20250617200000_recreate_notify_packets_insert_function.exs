defmodule Aprsme.Repo.Migrations.RecreateNotifyPacketsInsertFunction do
  use Ecto.Migration

  def up do
    execute "DROP TRIGGER IF EXISTS packets_notify_insert ON packets;"
    execute "DROP FUNCTION IF EXISTS notify_packets_insert();"

    execute """
    CREATE OR REPLACE FUNCTION notify_packets_insert() RETURNS trigger AS $$
    DECLARE
      payload TEXT;
    BEGIN
      payload := json_build_object(
        'id', NEW.id,
        'lat', NEW.lat,
        'lon', NEW.lon,
        'inserted_at', NEW.inserted_at
      )::text;
      PERFORM pg_notify('aprs_packets', payload);
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE TRIGGER packets_notify_insert
    AFTER INSERT ON packets
    FOR EACH ROW EXECUTE FUNCTION notify_packets_insert();
    """
  end

  def down do
    execute "DROP TRIGGER IF EXISTS packets_notify_insert ON packets;"
    execute "DROP FUNCTION IF EXISTS notify_packets_insert();"
  end
end
