defmodule Aprsme.Repo.Migrations.AddTruncateResetForPacketCounter do
  use Ecto.Migration

  def up do
    # Create a manual reset function that can be called when needed
    execute """
    CREATE OR REPLACE FUNCTION reset_packet_counter()
    RETURNS void AS $$
    BEGIN
      -- Reset the sequence
      PERFORM setval('packet_count_seq', 0, false);
      
      -- Update the deprecated table
      UPDATE packet_counters 
      SET count = 0, updated_at = NOW()
      WHERE counter_type = 'total_packets';
    END;
    $$ LANGUAGE plpgsql;
    """

    # Fix the get_packet_count function to handle the case when sequence is at 0
    execute """
    CREATE OR REPLACE FUNCTION get_packet_count()
    RETURNS BIGINT AS $$
    DECLARE
      seq_exists BOOLEAN;
      current_val BIGINT;
    BEGIN
      -- Check if sequence exists
      SELECT EXISTS (
        SELECT 1 FROM pg_sequences 
        WHERE schemaname = 'public' AND sequencename = 'packet_count_seq'
      ) INTO seq_exists;
      
      IF NOT seq_exists THEN
        -- Fallback to counting if sequence doesn't exist
        RETURN (SELECT COUNT(*) FROM packets);
      END IF;
      
      -- Try to get current value
      BEGIN
        current_val := currval('packet_count_seq');
      EXCEPTION
        WHEN object_not_in_prerequisite_state THEN
          -- If currval hasn't been called yet, get last_value
          SELECT last_value INTO current_val FROM packet_count_seq;
      END;
      
      -- If sequence is at 0 and hasn't been used (is_called = false), return 0
      IF current_val = 0 THEN
        RETURN 0;
      ELSE
        RETURN current_val;
      END IF;
    END;
    $$ LANGUAGE plpgsql STABLE;
    """
  end

  def down do
    # Drop the reset function
    execute "DROP FUNCTION IF EXISTS reset_packet_counter();"

    # Restore the original get_packet_count function
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
  end
end
