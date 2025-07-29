defmodule Aprsme.Repo.Migrations.FixPacketNotifyTrigger do
  use Ecto.Migration

  def up do
    # Drop existing trigger and function
    execute "DROP TRIGGER IF EXISTS packets_notify_insert ON packets;"
    execute "DROP FUNCTION IF EXISTS notify_packets_insert();"

    # Create updated function that only includes fields that exist in the packets table
    execute """
    CREATE OR REPLACE FUNCTION notify_packets_insert() RETURNS trigger AS $$
    DECLARE
      payload TEXT;
    BEGIN
      -- Build a comprehensive payload with all packet fields that exist
      payload := json_build_object(
        'id', NEW.id,
        'sender', NEW.sender,
        'ssid', NEW.ssid,
        'base_callsign', NEW.base_callsign,
        'lat', NEW.lat,
        'lon', NEW.lon,
        'altitude', NEW.altitude,
        'course', NEW.course,
        'speed', NEW.speed,
        'symbol_table_id', NEW.symbol_table_id,
        'symbol_code', NEW.symbol_code,
        'device_identifier', NEW.device_identifier,
        'path', NEW.path,
        'data_type', NEW.data_type,
        'received_at', NEW.received_at,
        'comment', NEW.comment,
        'raw_packet', NEW.raw_packet,
        'phg_power', NEW.phg_power,
        'phg_height', NEW.phg_height,
        'phg_gain', NEW.phg_gain,
        'phg_directivity', NEW.phg_directivity,
        'temperature', NEW.temperature,
        'humidity', NEW.humidity,
        'pressure', NEW.pressure,
        'wind_speed', NEW.wind_speed,
        'wind_direction', NEW.wind_direction,
        'wind_gust', NEW.wind_gust,
        'rain_1h', NEW.rain_1h,
        'rain_24h', NEW.rain_24h,
        'rain_midnight', NEW.rain_midnight,
        'luminosity', NEW.luminosity,
        'snow', NEW.snow,
        'has_weather', NEW.has_weather,
        'inserted_at', NEW.inserted_at
      )::text;
      
      -- Notify on the aprs_packets channel
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
    # Restore previous version (with the problematic fields)
    execute "DROP TRIGGER IF EXISTS packets_notify_insert ON packets;"
    execute "DROP FUNCTION IF EXISTS notify_packets_insert();"

    execute """
    CREATE OR REPLACE FUNCTION notify_packets_insert() RETURNS trigger AS $$
    DECLARE
      payload TEXT;
    BEGIN
      -- Build a comprehensive payload with all packet fields
      payload := json_build_object(
        'id', NEW.id,
        'sender', NEW.sender,
        'ssid', NEW.ssid,
        'base_callsign', NEW.base_callsign,
        'lat', NEW.lat,
        'lon', NEW.lon,
        'altitude', NEW.altitude,
        'course', NEW.course,
        'speed', NEW.speed,
        'symbol_table_id', NEW.symbol_table_id,
        'symbol_code', NEW.symbol_code,
        'device_identifier', NEW.device_identifier,
        'device_model', NEW.device_model,
        'device_vendor', NEW.device_vendor,
        'device_contact', NEW.device_contact,
        'device_class', NEW.device_class,
        'path', NEW.path,
        'data_type', NEW.data_type,
        'received_at', NEW.received_at,
        'comment', NEW.comment,
        'raw_packet', NEW.raw_packet,
        'phg_power', NEW.phg_power,
        'phg_height', NEW.phg_height,
        'phg_gain', NEW.phg_gain,
        'phg_directivity', NEW.phg_directivity,
        'temperature', NEW.temperature,
        'humidity', NEW.humidity,
        'pressure', NEW.pressure,
        'wind_speed', NEW.wind_speed,
        'wind_direction', NEW.wind_direction,
        'wind_gust', NEW.wind_gust,
        'rain_1h', NEW.rain_1h,
        'rain_24h', NEW.rain_24h,
        'rain_midnight', NEW.rain_midnight,
        'luminosity', NEW.luminosity,
        'snow', NEW.snow,
        'has_weather', NEW.has_weather,
        'inserted_at', NEW.inserted_at
      )::text;
      
      -- Notify on the aprs_packets channel
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
