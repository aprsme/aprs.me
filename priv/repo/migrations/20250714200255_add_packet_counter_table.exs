defmodule Aprsme.Repo.Migrations.AddPacketCounterTable do
  use Ecto.Migration

  def up do
    # Create a dedicated counter table
    create table(:packet_counters) do
      add :counter_type, :string, null: false
      add :count, :bigint, null: false, default: 0
      timestamps()
    end

    create unique_index(:packet_counters, [:counter_type])

    # Insert initial counter with current count
    execute """
    INSERT INTO packet_counters (counter_type, count, inserted_at, updated_at)
    SELECT 'total_packets', COUNT(*), NOW(), NOW()
    FROM packets;
    """

    # Create function to increment counter
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

    # Create function to decrement counter
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

    # Create triggers
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

    # Create a fast function to get the count
    execute """
    CREATE OR REPLACE FUNCTION get_packet_count()
    RETURNS BIGINT AS $$
    BEGIN
      RETURN (SELECT count FROM packet_counters WHERE counter_type = 'total_packets');
    END;
    $$ LANGUAGE plpgsql STABLE;
    """

    # Create index on the function for even faster access
    execute """
    CREATE INDEX idx_packet_count ON packet_counters (counter_type) WHERE counter_type = 'total_packets';
    """
  end

  def down do
    # Drop triggers
    execute "DROP TRIGGER IF EXISTS packet_insert_counter ON packets;"
    execute "DROP TRIGGER IF EXISTS packet_delete_counter ON packets;"

    # Drop functions
    execute "DROP FUNCTION IF EXISTS increment_packet_counter();"
    execute "DROP FUNCTION IF EXISTS decrement_packet_counter();"
    execute "DROP FUNCTION IF EXISTS get_packet_count();"

    # Drop table
    drop table(:packet_counters)
  end
end
