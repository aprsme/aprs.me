defmodule Aprsme.Repo.Migrations.AddParserImprovements do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # Position resolution and format
      add :posresolution, :float
      add :format, :string

      # Telemetry fields
      add :telemetry_seq, :integer
      add :telemetry_vals, {:array, :integer}
      add :telemetry_bits, :string
    end
  end
end
