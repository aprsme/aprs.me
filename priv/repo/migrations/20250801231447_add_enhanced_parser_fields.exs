defmodule Aprsme.Repo.Migrations.AddEnhancedParserFields do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # Radio range field
      add_if_not_exists :radiorange, :string

      # Standard parser compatibility fields
      add_if_not_exists :srccallsign, :string
      add_if_not_exists :dstcallsign, :string
      add_if_not_exists :body, :string
      add_if_not_exists :origpacket, :string
      add_if_not_exists :header, :string
      add_if_not_exists :alive, :integer, default: 1
      add_if_not_exists :posambiguity, :integer
      add_if_not_exists :symboltable, :string
      add_if_not_exists :symbolcode, :string
      add_if_not_exists :messaging, :integer
    end
  end
end
