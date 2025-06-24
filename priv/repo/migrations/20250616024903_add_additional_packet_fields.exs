defmodule Aprsme.Repo.Migrations.AddAdditionalPacketFields do
  use Ecto.Migration

  def change do
    alter table(:packets) do
      # Store the original raw packet string as received from APRS-IS
      add :raw_packet, :text

      # Store symbol information as separate columns for easier querying
      add :symbol_code, :string
      add :symbol_table_id, :string

      # Additional useful fields that may be in the parsed data
      add :comment, :text
      add :timestamp, :string
      add :aprsme_messaging, :boolean, default: false

      # Weather data fields (for weather packets)
      add :temperature, :float
      add :humidity, :float
      add :wind_speed, :float
      add :wind_direction, :integer
      add :wind_gust, :float
      add :pressure, :float
      add :rain_1h, :float
      add :rain_24h, :float
      add :rain_since_midnight, :float

      # Equipment/status information
      add :manufacturer, :string
      add :equipment_type, :string
      add :course, :integer
      add :speed, :float
      add :altitude, :float

      # Message-specific fields
      add :addressee, :string
      add :message_text, :text
      add :message_number, :string
    end

    # Add indices for commonly queried fields
    create index(:packets, [:symbol_code])
    create index(:packets, [:symbol_table_id])
    create index(:packets, [:timestamp])
    create index(:packets, [:addressee])
    create index(:packets, [:manufacturer])
  end
end
