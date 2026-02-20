defmodule Aprsme.Repo.Migrations.ConsolidatePacketsColumns do
  use Ecto.Migration

  @moduledoc """
  Consolidates the packets table by:
  - Dropping 10 dead parser-compat columns (never read)
  - Moving 14 display-only columns into a single `data` JSONB column
  - Fixing the notify_packets_insert() trigger to read from `data` JSONB

  The packets table is ephemeral (refills from APRS-IS stream) so data loss
  from DROP/recreate is acceptable.
  """

  def up do
    execute("SET LOCAL statement_timeout = '0'")

    # Drop existing partitioned table
    execute("DROP TABLE IF EXISTS packets CASCADE")

    # Recreate without the 24 dropped/moved columns, plus new `data` JSONB column
    execute("""
    CREATE TABLE packets (
      id uuid NOT NULL,
      base_callsign varchar,
      data_type varchar,
      destination varchar,
      path text,
      sender varchar,
      ssid varchar,
      data_extended jsonb,
      inserted_at timestamp NOT NULL,
      updated_at timestamp NOT NULL,
      received_at timestamp NOT NULL,
      region varchar,
      lat numeric,
      lon numeric,
      has_position boolean DEFAULT false,
      item_name text,
      object_name text,
      is_item boolean DEFAULT false,
      is_object boolean DEFAULT false,
      dao jsonb,
      raw_packet text,
      symbol_code varchar,
      symbol_table_id varchar,
      comment text,
      "timestamp" varchar,
      aprs_messaging boolean DEFAULT false,
      temperature double precision,
      humidity double precision,
      wind_speed double precision,
      wind_direction integer,
      wind_gust double precision,
      pressure double precision,
      rain_1h double precision,
      rain_24h double precision,
      rain_since_midnight double precision,
      manufacturer text,
      equipment_type text,
      course integer,
      speed double precision,
      altitude double precision,
      addressee text,
      message_text text,
      message_number varchar,
      location geometry(Point,4326),
      device_identifier text,
      snow double precision,
      has_weather boolean DEFAULT false,
      data jsonb DEFAULT '{}'::jsonb,
      PRIMARY KEY (id, received_at)
    ) PARTITION BY RANGE (received_at)
    """)

    # Default partition
    execute("CREATE TABLE packets_default PARTITION OF packets DEFAULT")

    # Daily partitions for recent past + near future
    today = Date.utc_today()

    for offset <- -8..2 do
      date = Date.add(today, offset)
      name = "packets_#{Calendar.strftime(date, "%Y%m%d")}"
      next_date = Date.add(date, 1)

      execute(
        "CREATE TABLE IF NOT EXISTS #{name} PARTITION OF packets FOR VALUES FROM ('#{date} 00:00:00') TO ('#{next_date} 00:00:00')"
      )
    end

    create_indexes()
    update_notify_trigger()
    create_triggers("packets")
  end

  def down do
    execute("SET LOCAL statement_timeout = '0'")
    execute("DROP TABLE IF EXISTS packets CASCADE")

    # Recreate original table with all columns (non-partitioned for simplicity)
    execute("""
    CREATE TABLE packets (
      id uuid NOT NULL PRIMARY KEY,
      base_callsign varchar,
      data_type varchar,
      destination varchar,
      information_field text,
      path text,
      sender varchar,
      ssid varchar,
      data_extended jsonb,
      inserted_at timestamp NOT NULL,
      updated_at timestamp NOT NULL,
      received_at timestamp NOT NULL,
      region varchar,
      lat numeric,
      lon numeric,
      has_position boolean DEFAULT false,
      item_name text,
      object_name text,
      is_item boolean DEFAULT false,
      is_object boolean DEFAULT false,
      dao jsonb,
      raw_packet text,
      symbol_code varchar,
      symbol_table_id varchar,
      comment text,
      "timestamp" varchar,
      aprs_messaging boolean DEFAULT false,
      temperature double precision,
      humidity double precision,
      wind_speed double precision,
      wind_direction integer,
      wind_gust double precision,
      pressure double precision,
      rain_1h double precision,
      rain_24h double precision,
      rain_since_midnight double precision,
      manufacturer text,
      equipment_type text,
      course integer,
      speed double precision,
      altitude double precision,
      addressee text,
      message_text text,
      message_number varchar,
      location geometry(Point,4326),
      device_identifier text,
      luminosity integer,
      snow double precision,
      phg_power integer,
      phg_height integer,
      phg_gain integer,
      phg_directivity integer,
      has_weather boolean DEFAULT false,
      rain_midnight double precision,
      position_ambiguity integer,
      posresolution double precision,
      format text,
      telemetry_seq integer,
      telemetry_vals integer[],
      telemetry_bits text,
      radiorange text,
      srccallsign varchar,
      dstcallsign varchar,
      body text,
      origpacket text,
      header text,
      alive integer DEFAULT 1,
      posambiguity integer,
      symboltable varchar,
      symbolcode varchar,
      messaging integer
    )
    """)

    restore_notify_trigger()
    create_triggers("packets")
  end

  defp create_indexes do
    # Time-series
    execute("CREATE INDEX idx_packets_received_desc ON packets (received_at DESC)")
    execute("CREATE INDEX idx_packets_received_brin ON packets USING brin (received_at)")

    # Sender/Callsign
    execute("CREATE INDEX idx_packets_sender_received ON packets (sender, received_at DESC)")

    execute(
      "CREATE INDEX idx_packets_upper_sender ON packets (upper(sender::text), received_at DESC)"
    )

    execute("CREATE INDEX idx_packets_sender_pattern ON packets (sender text_pattern_ops)")

    execute(
      "CREATE INDEX idx_packets_base_callsign_pattern ON packets (base_callsign text_pattern_ops)"
    )

    execute(
      "CREATE INDEX idx_packets_base_callsign_time ON packets (base_callsign, received_at DESC)"
    )

    execute("CREATE INDEX idx_packets_sender_id_desc ON packets (sender, id DESC)")

    execute(
      "CREATE INDEX idx_packets_upper_base_callsign ON packets (upper(base_callsign::text))"
    )

    execute(
      "CREATE INDEX idx_packets_sender_trgm ON packets USING gin (sender gin_trgm_ops) WHERE sender IS NOT NULL"
    )

    # Position/Geographic
    execute(
      "CREATE INDEX idx_packets_positioned ON packets (received_at DESC, lat, lon, sender) WHERE has_position = true"
    )

    execute(
      "CREATE INDEX idx_packets_position_time ON packets (has_position, received_at DESC) WHERE has_position = true"
    )

    execute(
      "CREATE INDEX idx_packets_location ON packets USING gist (location) WHERE has_position = true"
    )

    execute(
      "CREATE INDEX idx_packets_location_geography ON packets USING gist ((location::geography))"
    )

    execute(
      "CREATE INDEX idx_packets_spatial_temporal ON packets (received_at DESC, base_callsign) WHERE has_position = true"
    )

    execute(
      "CREATE INDEX idx_packets_sender_position ON packets (sender, received_at DESC) WHERE lat IS NOT NULL AND lon IS NOT NULL"
    )

    execute("CREATE INDEX idx_packets_lat ON packets (lat)")
    execute("CREATE INDEX idx_packets_lon ON packets (lon)")

    # Region
    execute("CREATE INDEX idx_packets_region ON packets (region)")
    execute("CREATE INDEX idx_packets_region_received ON packets (region, received_at)")

    execute(
      "CREATE INDEX idx_packets_region_position_time ON packets (region, has_position, received_at DESC) WHERE has_position = true AND region IS NOT NULL"
    )

    execute(
      "CREATE INDEX idx_packets_region_data_type ON packets (region, data_type, received_at)"
    )

    execute(
      "CREATE INDEX idx_packets_region_position ON packets (region, has_position, received_at)"
    )

    # Weather
    execute(
      "CREATE INDEX idx_packets_weather ON packets (sender, received_at DESC) WHERE temperature IS NOT NULL OR humidity IS NOT NULL OR pressure IS NOT NULL OR wind_speed IS NOT NULL OR wind_direction IS NOT NULL OR rain_1h IS NOT NULL"
    )

    execute(
      "CREATE INDEX idx_packets_has_weather ON packets (has_weather, sender, received_at DESC) WHERE has_weather = true"
    )

    execute(
      "CREATE INDEX idx_packets_temperature ON packets (temperature) WHERE temperature IS NOT NULL"
    )

    execute("CREATE INDEX idx_packets_humidity ON packets (humidity) WHERE humidity IS NOT NULL")
    execute("CREATE INDEX idx_packets_pressure ON packets (pressure) WHERE pressure IS NOT NULL")

    execute(
      "CREATE INDEX idx_packets_wind_speed ON packets (wind_speed) WHERE wind_speed IS NOT NULL"
    )

    execute(
      "CREATE INDEX idx_packets_received_temp ON packets (received_at, temperature) WHERE temperature IS NOT NULL"
    )

    execute(
      "CREATE INDEX idx_packets_sender_temp ON packets (sender, temperature) WHERE temperature IS NOT NULL"
    )

    execute(
      "CREATE INDEX idx_packets_weather_history ON packets (sender, data_type, received_at) WHERE data_type = 'weather'"
    )

    execute(
      "CREATE INDEX idx_packets_weather_selective ON packets (received_at DESC) WHERE data_type IN ('weather', 'Weather', 'WX', 'wx')"
    )

    execute(
      "CREATE INDEX idx_packets_weather_symbol ON packets (received_at DESC, lat, lon) WHERE (symbol_table_id = '/' AND symbol_code = '_') OR (symbol_table_id = E'\\\\' AND symbol_code = '_')"
    )

    execute(
      "CREATE INDEX idx_packets_sender_weather_lookup ON packets (upper(sender::text), received_at DESC) INCLUDE (data_type, symbol_table_id, symbol_code) WHERE data_type = 'weather' OR (symbol_table_id = '/' AND symbol_code = '_') OR (symbol_table_id = E'\\\\' AND symbol_code = '_')"
    )

    # Data type & Device
    execute(
      "CREATE INDEX idx_packets_datatype_sender_time ON packets (data_type, sender, received_at DESC)"
    )

    execute(
      "CREATE INDEX idx_packets_device_identifier ON packets (device_identifier) WHERE device_identifier IS NOT NULL"
    )

    execute(
      "CREATE INDEX idx_packets_device_received ON packets (device_identifier, received_at) WHERE device_identifier IS NOT NULL"
    )

    # Path
    execute(
      "CREATE INDEX idx_packets_path_pattern ON packets (path text_pattern_ops) WHERE path IS NOT NULL AND path <> ''"
    )

    execute(
      "CREATE INDEX idx_packets_path_trgm ON packets USING gin (path gin_trgm_ops) WHERE path IS NOT NULL AND path <> '' AND path !~ '^TCPIP' AND path !~ ',TCPIP'"
    )

    # Digipeater/stations heard
    execute(
      "CREATE INDEX idx_packets_digipeater ON packets (sender, received_at DESC) WHERE location IS NOT NULL"
    )

    execute(
      "CREATE INDEX idx_packets_stations_heard ON packets (received_at DESC, sender, lat, lon) WHERE path IS NOT NULL AND path <> '' AND path !~ '^TCPIP' AND path !~ ',TCPIP' AND lat IS NOT NULL AND lon IS NOT NULL"
    )

    # Symbol
    execute(
      "CREATE INDEX idx_packets_symbol_time ON packets (symbol_table_id, symbol_code, received_at)"
    )

    # JSONB data column — GIN index for key lookups
    execute(
      "CREATE INDEX idx_packets_data_gin ON packets USING gin (data) WHERE data IS NOT NULL AND data <> '{}'::jsonb"
    )
  end

  # Updated trigger reads PHG/luminosity/rain_midnight from data JSONB
  defp update_notify_trigger do
    execute("DROP FUNCTION IF EXISTS notify_packets_insert() CASCADE")

    execute("""
    CREATE OR REPLACE FUNCTION notify_packets_insert() RETURNS trigger AS $$
    DECLARE
      payload TEXT;
    BEGIN
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
        'phg_power', NEW.data->>'phg_power',
        'phg_height', NEW.data->>'phg_height',
        'phg_gain', NEW.data->>'phg_gain',
        'phg_directivity', NEW.data->>'phg_directivity',
        'temperature', NEW.temperature,
        'humidity', NEW.humidity,
        'pressure', NEW.pressure,
        'wind_speed', NEW.wind_speed,
        'wind_direction', NEW.wind_direction,
        'wind_gust', NEW.wind_gust,
        'rain_1h', NEW.rain_1h,
        'rain_24h', NEW.rain_24h,
        'rain_midnight', NEW.data->>'rain_midnight',
        'luminosity', NEW.data->>'luminosity',
        'snow', NEW.snow,
        'has_weather', NEW.has_weather,
        'inserted_at', NEW.inserted_at
      )::text;

      PERFORM pg_notify('aprs_packets', payload);

      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """)
  end

  # Restore original trigger for rollback
  defp restore_notify_trigger do
    execute("DROP FUNCTION IF EXISTS notify_packets_insert() CASCADE")

    execute("""
    CREATE OR REPLACE FUNCTION notify_packets_insert() RETURNS trigger AS $$
    DECLARE
      payload TEXT;
    BEGIN
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

      PERFORM pg_notify('aprs_packets', payload);

      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """)
  end

  defp create_triggers(table) do
    execute(
      "CREATE TRIGGER update_has_weather_trigger BEFORE INSERT OR UPDATE ON #{table} FOR EACH ROW EXECUTE FUNCTION update_has_weather()"
    )

    execute(
      "CREATE TRIGGER packets_notify_insert AFTER INSERT ON #{table} FOR EACH ROW EXECUTE FUNCTION notify_packets_insert()"
    )

    execute(
      "CREATE TRIGGER packet_insert_sequence AFTER INSERT ON #{table} FOR EACH ROW EXECUTE FUNCTION increment_packet_sequence()"
    )

    execute(
      "CREATE TRIGGER packet_delete_sequence AFTER DELETE ON #{table} FOR EACH ROW EXECUTE FUNCTION decrement_packet_sequence()"
    )
  end
end
