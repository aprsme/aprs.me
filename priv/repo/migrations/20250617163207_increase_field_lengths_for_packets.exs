defmodule Aprsme.Repo.Migrations.IncreaseFieldLengthsForPackets do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # Change fields that might contain long strings from varchar(255) to text
      modify :information_field, :text, from: :string
      modify :path, :text, from: :string
      modify :manufacturer, :text, from: :string
      modify :equipment_type, :text, from: :string
      modify :addressee, :text, from: :string
    end
  end
end
