defmodule Aprsme.PacketFieldWhitelist do
  @moduledoc """
  Maintains a whitelist of allowed fields for APRS packets based on the database schema.
  This ensures only valid fields are passed to database operations.
  """

  # Define all valid fields from the Packet schema
  # This list is derived from the schema definition in lib/aprsme/packet.ex
  # Using string list to handle both atom and string keys consistently
  @allowed_fields ~w[
    addressee
    altitude
    aprs_messaging
    base_callsign
    comment
    course
    dao
    data
    data_type
    destination
    device_identifier
    equipment_type
    has_position
    has_weather
    humidity
    is_item
    is_object
    item_name
    lat
    location
    lon
    manufacturer
    message_number
    message_text
    object_name
    path
    pressure
    rain_1h
    rain_24h
    rain_since_midnight
    raw_packet
    received_at
    region
    sender
    snow
    speed
    ssid
    symbol_code
    symbol_table_id
    temperature
    timestamp
    wind_direction
    wind_gust
    wind_speed
    inserted_at
    updated_at
  ]

  @doc """
  Returns the list of allowed field names as atoms.
  """
  def allowed_fields, do: @allowed_fields

  @doc """
  Filters a map to only include allowed fields.
  Handles both atom and string keys.
  """
  def filter_fields(attrs) when is_map(attrs) do
    attrs
    |> Enum.filter(fn {key, _value} ->
      key_str = to_string(key)
      key_str in @allowed_fields
    end)
    |> Map.new()
  end

  @doc """
  Checks if a field is allowed.
  """
  def allowed?(field) when is_atom(field), do: to_string(field) in @allowed_fields
  def allowed?(field) when is_binary(field), do: field in @allowed_fields
  def allowed?(_), do: false

  @doc """
  Returns a list of fields that are not allowed from the given map.
  Useful for debugging.
  """
  def invalid_fields(attrs) when is_map(attrs) do
    attrs
    |> Map.keys()
    |> Enum.reject(&allowed?/1)
    |> Enum.map(&to_string/1)
    |> Enum.sort()
  end
end
