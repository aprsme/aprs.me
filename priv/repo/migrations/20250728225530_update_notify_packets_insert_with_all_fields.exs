defmodule Aprsme.Repo.Migrations.UpdateNotifyPacketsInsertWithAllFields do
  use Ecto.Migration

  def up do
    # Drop existing trigger and function
    execute "DROP TRIGGER IF EXISTS packets_notify_insert ON packets;"
    execute "DROP FUNCTION IF EXISTS notify_packets_insert();"

    # Create updated function that includes all fields needed by the info page
    execute """
    CREATE OR REPLACE FUNCTION notify_packets_insert() RETURNS trigger AS $$
    DECLARE
      payload TEXT;
    BEGIN
      payload := json_build_object(
        'id', NEW.id,
        'sender', NEW.sender,
        'lat', NEW.lat,
        'lon', NEW.lon,
        'altitude', NEW.altitude,
        'course', NEW.course,
        'speed', NEW.speed,
        'symbol_table_id', NEW.symbol_table_id,
        'symbol_code', NEW.symbol_code,
        'device_identifier', NEW.device_identifier,
        'path', NEW.path,
        'received_at', NEW.received_at,
        'comment', NEW.comment,
        'raw_packet', NEW.raw_packet,
        'phg_power', NEW.phg_power,
        'phg_height', NEW.phg_height,
        'phg_gain', NEW.phg_gain,
        'phg_directivity', NEW.phg_directivity,
        'inserted_at', NEW.inserted_at
      )::text;
      PERFORM pg_notify('aprs_packets', payload);
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    # Recreate trigger
    execute """
    CREATE TRIGGER packets_notify_insert
    AFTER INSERT ON packets
    FOR EACH ROW EXECUTE FUNCTION notify_packets_insert();
    """
  end

  def down do
    # Drop updated trigger and function
    execute "DROP TRIGGER IF EXISTS packets_notify_insert ON packets;"
    execute "DROP FUNCTION IF EXISTS notify_packets_insert();"

    # Restore previous version with minimal fields
    execute """
    CREATE OR REPLACE FUNCTION notify_packets_insert() RETURNS trigger AS $$
    DECLARE
      payload TEXT;
    BEGIN
      payload := json_build_object(
        'id', NEW.id,
        'sender', NEW.sender,
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
end
