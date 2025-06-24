defmodule Aprsme.Repo.Migrations.NotifyOnBadpacketsInsert do
  use Ecto.Migration

  def up do
    execute """
    CREATE OR REPLACE FUNCTION notify_badpackets_insert() RETURNS trigger AS $$
    BEGIN
      PERFORM pg_notify('aprs_events', 'bad_packet');
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute "DROP TRIGGER IF EXISTS badpackets_notify_insert ON badpackets;"

    execute """
    CREATE TRIGGER badpackets_notify_insert
    AFTER INSERT ON badpackets
    FOR EACH ROW EXECUTE FUNCTION notify_badpackets_insert();
    """
  end

  def down do
    execute "DROP TRIGGER IF EXISTS badpackets_notify_insert ON badpackets;"
    execute "DROP FUNCTION IF EXISTS notify_badpackets_insert();"
  end
end
