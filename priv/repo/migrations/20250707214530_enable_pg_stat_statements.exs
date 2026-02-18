defmodule Aprsme.Repo.Migrations.EnablePgStatStatements do
  use Ecto.Migration

  def up do
    execute """
    DO $$
    BEGIN
      CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
    EXCEPTION WHEN insufficient_privilege THEN
      RAISE NOTICE 'Skipping pg_stat_statements: insufficient privileges (requires superuser)';
    END;
    $$
    """
  end

  def down do
    execute "DROP EXTENSION IF EXISTS pg_stat_statements"
  end
end
