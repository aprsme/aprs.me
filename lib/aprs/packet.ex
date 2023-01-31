defmodule Aprs.Packet do
  use Aprs.Schema
  import Ecto.Changeset
  alias Aprs.DataExtended

  schema "packets" do
    field(:base_callsign, :string)
    field(:data_type, :string)
    field(:destination, :string)
    field(:information_field, :string)
    field(:path, :string)
    field(:sender, :string)
    field(:ssid, :string)
    embeds_one(:data_extended, DataExtended)

    timestamps()
  end

  @doc false
  def changeset(packet, attrs) do
    packet
    |> cast(attrs, [
      :base_callsign,
      :data_type,
      :destination,
      :information_field,
      :path,
      :sender,
      :ssid
    ])
    |> validate_required([
      :base_callsign,
      :data_type,
      :destination,
      :information_field,
      :path,
      :sender,
      :ssid
    ])
  end
end
