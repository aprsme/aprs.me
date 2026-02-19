defmodule Aprsme.Repo.Migrations.DropUnusedPacketIndexes do
  use Ecto.Migration

  @disable_ddl_transaction true
  @disable_migration_lock true

  # 29 unused indexes on the packets table totaling ~4.6 GB.
  # Categories: exact duplicates, subsumed by better indexes,
  # dead code (lower() when codebase uses upper()), and
  # indexes with no matching query pattern in the codebase.

  def up do
    # Exact duplicates
    drop_if_exists index(:packets, [], name: :idx_packets_weather_by_callsign)
    drop_if_exists index(:packets, [], name: :packets_sender_upper_idx)
    drop_if_exists index(:packets, [], name: :packets_count_idx)
    drop_if_exists index(:packets, [], name: :idx_packets_sender_upper)
    drop_if_exists index(:packets, [], name: :idx_packets_upper_sender)

    # Subsumed by better indexes
    drop_if_exists index(:packets, [], name: :packets_sender_received_at_covering_idx)
    drop_if_exists index(:packets, [], name: :packets_sender_received_at_index)
    drop_if_exists index(:packets, [], name: :packets_sender_index)
    drop_if_exists index(:packets, [], name: :packets_has_position_index)
    drop_if_exists index(:packets, [], name: :packets_has_position_received_at_index)
    drop_if_exists index(:packets, [], name: :packets_received_at_has_position_data_type_index)
    drop_if_exists index(:packets, [], name: :packets_device_identifier_index)

    # Dead code — lower() indexes, codebase only uses upper()
    drop_if_exists index(:packets, [], name: :packets_sender_lower_idx)
    drop_if_exists index(:packets, [], name: :packets_sender_lower_received_at_idx)

    # No matching query pattern in codebase
    drop_if_exists index(:packets, [], name: :packets_addressee_index)
    drop_if_exists index(:packets, [], name: :packets_is_item_index)
    drop_if_exists index(:packets, [], name: :packets_is_object_index)
    drop_if_exists index(:packets, [], name: :packets_item_name_index)
    drop_if_exists index(:packets, [], name: :packets_object_name_index)
    drop_if_exists index(:packets, [], name: :packets_manufacturer_index)
    drop_if_exists index(:packets, [], name: :packets_phg_height_index)
    drop_if_exists index(:packets, [], name: :packets_phg_power_index)
    drop_if_exists index(:packets, [], name: :packets_timestamp_index)
    drop_if_exists index(:packets, [], name: :packets_sender_has_position_index)
    drop_if_exists index(:packets, [], name: :packets_data_type_index)
    drop_if_exists index(:packets, [], name: :packets_symbol_table_id_index)
    drop_if_exists index(:packets, [], name: :packets_symbol_code_index)
    drop_if_exists index(:packets, [], name: :idx_packets_datatype_sender_time)
    drop_if_exists index(:packets, [], name: :packets_sender_data_type_received_at_index)

    # Duplicate pairs — ASC variant when queries always ORDER BY DESC
    # packets_base_callsign_received_at_idx (ASC) duplicates idx_packets_base_callsign_time (DESC)
    drop_if_exists index(:packets, [], name: :packets_base_callsign_received_at_idx)
    # packets_received_at_idx (ASC) subsumed by idx_packets_recent_only (DESC) + BRIN
    drop_if_exists index(:packets, [], name: :packets_received_at_idx)
  end

  def down do
    # Exact duplicates
    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_weather_by_callsign ON packets USING btree (sender, received_at DESC) WHERE ((temperature IS NOT NULL) OR (humidity IS NOT NULL) OR (pressure IS NOT NULL) OR (wind_speed IS NOT NULL) OR (wind_direction IS NOT NULL) OR (rain_1h IS NOT NULL))"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_upper_idx ON packets USING btree (upper((sender)::text), received_at DESC)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_count_idx ON packets USING btree (id) WHERE (id IS NOT NULL)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_sender_upper ON packets USING btree (upper((sender)::text))"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_upper_sender ON packets USING btree (upper((sender)::text))"

    # Subsumed
    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_at_covering_idx ON packets USING btree (upper((sender)::text), received_at DESC) INCLUDE (data_type, symbol_table_id, symbol_code)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_received_at_index ON packets USING btree (sender, received_at)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_index ON packets USING btree (sender)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_has_position_index ON packets USING btree (has_position)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_has_position_received_at_index ON packets USING btree (has_position, received_at)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_received_at_has_position_data_type_index ON packets USING btree (received_at, has_position, data_type)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_device_identifier_index ON packets USING btree (device_identifier)"

    # lower() indexes
    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_lower_idx ON packets USING btree (lower((sender)::text)) WHERE (sender IS NOT NULL)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_lower_received_at_idx ON packets USING btree (lower((sender)::text), received_at DESC)"

    # No matching query pattern
    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_addressee_index ON packets USING btree (addressee)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_is_item_index ON packets USING btree (is_item)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_is_object_index ON packets USING btree (is_object)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_item_name_index ON packets USING btree (item_name)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_object_name_index ON packets USING btree (object_name)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_manufacturer_index ON packets USING btree (manufacturer)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_phg_height_index ON packets USING btree (phg_height)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_phg_power_index ON packets USING btree (phg_power)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_timestamp_index ON packets USING btree (\"timestamp\")"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_has_position_index ON packets USING btree (sender, has_position)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_data_type_index ON packets USING btree (data_type)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_symbol_table_id_index ON packets USING btree (symbol_table_id)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_symbol_code_index ON packets USING btree (symbol_code)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_packets_datatype_sender_time ON packets USING btree (data_type, sender, received_at DESC)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_sender_data_type_received_at_index ON packets USING btree (sender, data_type, received_at)"

    # Duplicate pairs
    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_base_callsign_received_at_idx ON packets USING btree (base_callsign, received_at)"

    execute "CREATE INDEX CONCURRENTLY IF NOT EXISTS packets_received_at_idx ON packets USING btree (received_at)"
  end
end
