defmodule Parser.Types.MicE do
  @moduledoc """
  Type struct for MicE
  """
  @behaviour Access

  defstruct lat_degrees: 0,
            lat_minutes: 0,
            lat_fractional: 0,
            lat_direction: :unknown,
            lon_direction: :unknown,
            longitude_offset: 0,
            message_code: nil,
            message_description: nil,
            dti: nil,
            heading: 0,
            lon_degrees: 0,
            lon_minutes: 0,
            lon_fractional: 0,
            speed: 0,
            manufacturer: "Unknown",
            message: "",
            symbol_table_id: "/",
            symbol_code: ">"

  @doc """
  Implements the Access behaviour for MicE struct.
  This allows us to access fields using the access syntax: struct[:field]
  Additionally, it provides support for dynamic access via get_in/update_in/etc.

  Fetch a key from the MicE struct.
  Special handling for :latitude and :longitude which are calculated from components.
  """
  def fetch(mic_e, :latitude) do
    # Calculate decimal latitude from components
    if is_number(mic_e.lat_degrees) and is_number(mic_e.lat_minutes) do
      lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0
      lat = if mic_e.lat_direction == :south, do: -lat, else: lat
      {:ok, lat}
    else
      :error
    end
  end

  def fetch(mic_e, :longitude) do
    # Calculate decimal longitude from components
    if is_number(mic_e.lon_degrees) and is_number(mic_e.lon_minutes) do
      lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0
      lon = if mic_e.lon_direction == :west, do: -lon, else: lon
      {:ok, lon}
    else
      :error
    end
  end

  def fetch(mic_e, key) when is_atom(key) do
    # Fall back to struct field access for other keys
    Map.fetch(mic_e, key)
  end

  # Handle string keys by converting to atom and trying again
  def fetch(mic_e, key) when is_binary(key) do
    atom_key = String.to_existing_atom(key)
    fetch(mic_e, atom_key)
  rescue
    ArgumentError ->
      # If the atom doesn't exist, return :error
      :error
  end

  @doc """
  Gets a value and updates it with the given function.
  """
  def get_and_update(mic_e, key, fun) do
    value =
      case key do
        :latitude ->
          if is_number(mic_e.lat_degrees) and is_number(mic_e.lat_minutes) do
            lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0
            if mic_e.lat_direction == :south, do: -lat, else: lat
          end

        :longitude ->
          if is_number(mic_e.lon_degrees) and is_number(mic_e.lon_minutes) do
            lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0
            if mic_e.lon_direction == :west, do: -lon, else: lon
          end

        key when is_binary(key) ->
          # Handle string keys by converting to atom if it exists
          try do
            atom_key = String.to_existing_atom(key)
            Map.get(mic_e, atom_key)
          rescue
            ArgumentError ->
              nil
          end

        _ ->
          Map.get(mic_e, key)
      end

    case fun.(value) do
      {get, update} -> {get, Map.put(mic_e, key, update)}
      :pop -> {value, Map.put(mic_e, key, nil)}
    end
  end

  @doc """
  Removes the given key from the struct with the default implementation.
  """
  def pop(mic_e, key) when is_atom(key) do
    {Map.get(mic_e, key), Map.put(mic_e, key, nil)}
  end

  # Handle string keys by converting to atom if it exists
  def pop(mic_e, key) when is_binary(key) do
    atom_key = String.to_existing_atom(key)
    {Map.get(mic_e, atom_key), Map.put(mic_e, atom_key, nil)}
  rescue
    ArgumentError ->
      # If the atom doesn't exist, return nil and unchanged struct
      {nil, mic_e}
  end
end
