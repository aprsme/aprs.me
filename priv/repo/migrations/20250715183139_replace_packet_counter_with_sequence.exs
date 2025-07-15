defmodule Aprsme.Repo.Migrations.ReplacePacketCounterWithSequence do
  use Ecto.Migration

  def up do
    # Get current count before making changes
    execute """
    DO $$
    DECLARE
      current_count BIGINT;
    BEGIN
      SELECT count INTO current_count FROM packet_counters WHERE counter_type = 'total_packets';
      
      -- Ensure count is at least 1 (sequences can't start at 0)
      IF current_count IS NULL OR current_count < 1 THEN
        current_count := 1;
      END IF;
      
      -- Create a sequence starting from current count
      EXECUTE format('CREATE SEQUENCE packet_count_seq START WITH %s', current_count);
    END $$;
    """

    # Drop the triggers that update the table
    execute "DROP TRIGGER IF EXISTS packet_insert_counter ON packets;"
    execute "DROP TRIGGER IF EXISTS packet_delete_counter ON packets;"

    # Create new trigger functions that use the sequence
    execute """
    CREATE OR REPLACE FUNCTION increment_packet_sequence()
    RETURNS TRIGGER AS $$
    BEGIN
      PERFORM nextval('packet_count_seq');
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE OR REPLACE FUNCTION decrement_packet_sequence()
    RETURNS TRIGGER AS $$
    BEGIN
      PERFORM setval('packet_count_seq', GREATEST(0, currval('packet_count_seq') - 1), true);
      RETURN OLD;
    END;
    $$ LANGUAGE plpgsql;
    """

    # Create new triggers
    execute """
    CREATE TRIGGER packet_insert_sequence
    AFTER INSERT ON packets
    FOR EACH ROW
    EXECUTE FUNCTION increment_packet_sequence();
    """

    execute """
    CREATE TRIGGER packet_delete_sequence
    AFTER DELETE ON packets
    FOR EACH ROW
    EXECUTE FUNCTION decrement_packet_sequence();
    """

    # Update the get_packet_count function to use the sequence
    execute """
    CREATE OR REPLACE FUNCTION get_packet_count()
    RETURNS BIGINT AS $$
    BEGIN
      RETURN currval('packet_count_seq');
    EXCEPTION
      WHEN object_not_in_prerequisite_state THEN
        -- If currval hasn't been called yet in this session, get the last value
        RETURN last_value FROM packet_count_seq;
    END;
    $$ LANGUAGE plpgsql STABLE;
    """

    # Keep the packet_counters table for now but stop updating it
    execute """
    COMMENT ON TABLE packet_counters IS 'DEPRECATED: This table is no longer actively updated. Use get_packet_count() function which reads from packet_count_seq sequence.';
    """
  end

  def down do
    # Restore the original triggers
    execute "DROP TRIGGER IF EXISTS packet_insert_sequence ON packets;"
    execute "DROP TRIGGER IF EXISTS packet_delete_sequence ON packets;"

    # Restore original trigger functions
    execute """
    CREATE OR REPLACE FUNCTION increment_packet_counter()
    RETURNS TRIGGER AS $$
    BEGIN
      UPDATE packet_counters 
      SET count = count + 1,
          updated_at = NOW()
      WHERE counter_type = 'total_packets';
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE OR REPLACE FUNCTION decrement_packet_counter()
    RETURNS TRIGGER AS $$
    BEGIN
      UPDATE packet_counters 
      SET count = GREATEST(0, count - 1),
          updated_at = NOW()
      WHERE counter_type = 'total_packets';
      RETURN OLD;
    END;
    $$ LANGUAGE plpgsql;
    """

    # Update the count in the table from the sequence
    execute """
    UPDATE packet_counters 
    SET count = (SELECT last_value FROM packet_count_seq),
        updated_at = NOW()
    WHERE counter_type = 'total_packets';
    """

    # Restore triggers
    execute """
    CREATE TRIGGER packet_insert_counter
    AFTER INSERT ON packets
    FOR EACH ROW
    EXECUTE FUNCTION increment_packet_counter();
    """

    execute """
    CREATE TRIGGER packet_delete_counter
    AFTER DELETE ON packets
    FOR EACH ROW
    EXECUTE FUNCTION decrement_packet_counter();
    """

    # Restore original get_packet_count function
    execute """
    CREATE OR REPLACE FUNCTION get_packet_count()
    RETURNS BIGINT AS $$
    BEGIN
      RETURN (SELECT count FROM packet_counters WHERE counter_type = 'total_packets');
    END;
    $$ LANGUAGE plpgsql STABLE;
    """

    # Drop the sequence
    execute "DROP SEQUENCE IF EXISTS packet_count_seq;"

    # Remove functions
    execute "DROP FUNCTION IF EXISTS increment_packet_sequence();"
    execute "DROP FUNCTION IF EXISTS decrement_packet_sequence();"

    # Remove comment
    execute """
    COMMENT ON TABLE packet_counters IS NULL;
    """
  end
end
