defmodule Aprsme.Repo.Migrations.PartitionPacketsTable do
  use Ecto.Migration

  def up do
    # Disable statement timeout for DDL on large table with many indexes
    execute("SET LOCAL statement_timeout = '0'")

    # Drop existing non-partitioned table (data is ephemeral, refills from APRS-IS stream)
    execute("DROP TABLE IF EXISTS packets CASCADE")

    # Create partitioned table with composite PK required for partitioning
    execute("""
    CREATE TABLE packets (
      id uuid NOT NULL,
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
      messaging integer,
      PRIMARY KEY (id, received_at)
    ) PARTITION BY RANGE (received_at)
    """)

    # Default partition catches rows outside named partitions
    execute("CREATE TABLE packets_default PARTITION OF packets DEFAULT")

    # Create daily partitions for recent past + near future
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
    create_triggers("packets")
  end

  def down do
    execute("DROP TABLE IF EXISTS packets CASCADE")

    # Recreate as regular non-partitioned table
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
