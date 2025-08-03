defmodule Aprsme.Repo.Migrations.AddParserCompatibilityFields do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # Position resolution and format fields from enhanced parser
      add_if_not_exists :posresolution, :float
      add_if_not_exists :format, :string

      # Additional message field
      add_if_not_exists :addressee, :string
      add_if_not_exists :message_text, :string
      add_if_not_exists :message_number, :string

      # Telemetry fields
      add_if_not_exists :telemetry_seq, :integer
      add_if_not_exists :telemetry_vals, {:array, :integer}
      add_if_not_exists :telemetry_bits, :string
    end
  end
end
