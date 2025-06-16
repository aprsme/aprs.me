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
            manufacturer: :unknown,
            message: ""

  @doc """
  Implements the Access behaviour for MicE struct.
  This allows us to access fields using the access syntax: struct[:field]
  Additionally, it provides support for dynamic access via get_in/update_in/etc.

  Fetch a key from the MicE struct.
  Special handling for :latitude and :longitude which are calculated from components.
  """
  def fetch(mic_e, :latitude) do
    # Calculate decimal latitude from components
    lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0
    lat = if mic_e.lat_direction == :south, do: -lat, else: lat
    {:ok, lat}
  end

  def fetch(mic_e, :longitude) do
    # Calculate decimal longitude from components
    lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0
    lon = if mic_e.lon_direction == :west, do: -lon, else: lon
    {:ok, lon}
  end

  def fetch(mic_e, key) when is_atom(key) do
    # Fall back to struct field access for other keys
    Map.fetch(mic_e, key)
  end

  @doc """
  Gets a value and updates it with the given function.
  """
  def get_and_update(mic_e, key, fun) do
    value =
      case key do
        :latitude ->
          lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0
          if mic_e.lat_direction == :south, do: -lat, else: lat

        :longitude ->
          lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0
          if mic_e.lon_direction == :west, do: -lon, else: lon

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
  def pop(mic_e, key) do
    {Map.get(mic_e, key), Map.put(mic_e, key, nil)}
  end
end
