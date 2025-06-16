defmodule Aprs.Packet do
  @moduledoc false
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
    field(:received_at, :utc_datetime_usec)
    field(:region, :string)
    field(:lat, :float)
    field(:lon, :float)
    field(:has_position, :boolean, default: false)
    embeds_one(:data_extended, DataExtended)

    timestamps()
  end

  @doc false
  def changeset(packet, attrs) do
    # Convert atom data_type to string
    attrs = normalize_data_type(attrs)

    packet
    |> cast(attrs, [
      :base_callsign,
      :data_type,
      :destination,
      :information_field,
      :path,
      :sender,
      :ssid,
      :received_at,
      :region,
      :lat,
      :lon,
      :has_position
    ])
    |> validate_required([
      :base_callsign,
      :data_type,
      :destination,
      :information_field,
      :path,
      :sender,
      :ssid,
      :received_at
    ])
    |> maybe_set_has_position()
  end

  defp maybe_set_has_position(changeset) do
    if (get_field(changeset, :lat) && get_field(changeset, :lon)) ||
         (get_change(changeset, :data_extended) &&
            get_change(changeset, :data_extended).latitude &&
            get_change(changeset, :data_extended).longitude) do
      put_change(changeset, :has_position, true)
    else
      changeset
    end
  end

  # Convert atom data_type to string for storage
  defp normalize_data_type(%{data_type: data_type} = attrs) when is_atom(data_type) do
    %{attrs | data_type: to_string(data_type)}
  end

  defp normalize_data_type(%{"data_type" => data_type} = attrs) when is_atom(data_type) do
    %{attrs | "data_type" => to_string(data_type)}
  end

  # Handle :data_type key access format
  defp normalize_data_type(attrs) when is_map(attrs) do
    if Map.has_key?(attrs, :data_type) and is_atom(attrs.data_type) do
      %{attrs | data_type: to_string(attrs.data_type)}
    else
      attrs
    end
  end

  defp normalize_data_type(attrs), do: attrs
end
