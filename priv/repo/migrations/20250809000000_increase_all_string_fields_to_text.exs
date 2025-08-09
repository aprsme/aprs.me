defmodule Aprsme.Repo.Migrations.IncreaseAllStringFieldsToText do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # Fields that commonly have longer content
      modify :comment, :text, from: :string
      modify :message_text, :text, from: :string
      modify :raw_packet, :text, from: :string
      modify :body, :text, from: :string
      modify :origpacket, :text, from: :string
      modify :header, :text, from: :string

      # Radio range can be long
      modify :radiorange, :text, from: :string

      # Object/item names can potentially be long
      modify :item_name, :text, from: :string
      modify :object_name, :text, from: :string

      # Telemetry bits field
      modify :telemetry_bits, :text, from: :string

      # Device identifier could be long
      modify :device_identifier, :text, from: :string

      # Format field might have long descriptions
      modify :format, :text, from: :string
    end
  end
end
