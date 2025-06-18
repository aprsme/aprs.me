defmodule Parser.Types.MicE do
  @moduledoc """
  Type struct for MicE
  """
  @behaviour Access

  @type direction :: :north | :south | :east | :west | :unknown

  @type t :: %__MODULE__{
          lat_degrees: number(),
          lat_minutes: number(),
          lat_fractional: number(),
          lat_direction: direction(),
          lon_direction: direction(),
          longitude_offset: number(),
          message_code: String.t() | nil,
          message_description: String.t() | nil,
          dti: String.t() | nil,
          heading: number(),
          lon_degrees: number(),
          lon_minutes: number(),
          lon_fractional: number(),
          speed: number(),
          manufacturer: String.t(),
          message: String.t(),
          symbol_table_id: String.t(),
          symbol_code: String.t()
        }

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
  @spec fetch(t(), atom() | String.t()) :: {:ok, any()} | :error
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
  @spec get_and_update(t(), atom() | String.t(), (any() -> {any(), any()} | :pop)) :: {any(), t()}
  def get_and_update(mic_e, key, fun) do
    value = get_value(mic_e, key)
    apply_update_function(mic_e, key, value, fun)
  end

  @spec get_value(t(), atom() | String.t()) :: any()
  defp get_value(mic_e, :latitude), do: calculate_latitude(mic_e)
  defp get_value(mic_e, :longitude), do: calculate_longitude(mic_e)
  defp get_value(mic_e, key) when is_binary(key), do: get_string_key_value(mic_e, key)
  defp get_value(mic_e, key), do: Map.get(mic_e, key)

  @spec calculate_latitude(t()) :: float() | nil
  defp calculate_latitude(mic_e) do
    if is_number(mic_e.lat_degrees) and is_number(mic_e.lat_minutes) do
      lat = mic_e.lat_degrees + mic_e.lat_minutes / 60.0
      if mic_e.lat_direction == :south, do: -lat, else: lat
    end
  end

  @spec calculate_longitude(t()) :: float() | nil
  defp calculate_longitude(mic_e) do
    if is_number(mic_e.lon_degrees) and is_number(mic_e.lon_minutes) do
      lon = mic_e.lon_degrees + mic_e.lon_minutes / 60.0
      if mic_e.lon_direction == :west, do: -lon, else: lon
    end
  end

  @spec get_string_key_value(t(), String.t()) :: any()
  defp get_string_key_value(mic_e, key) do
    atom_key = String.to_existing_atom(key)
    Map.get(mic_e, atom_key)
  rescue
    ArgumentError ->
      nil
  end

  @spec apply_update_function(t(), atom() | String.t(), any(), (any() -> {any(), any()} | :pop)) :: {any(), t()}
  defp apply_update_function(mic_e, key, value, fun) do
    case fun.(value) do
      {get, update} -> {get, Map.put(mic_e, key, update)}
      :pop -> {value, Map.put(mic_e, key, nil)}
    end
  end

  @doc """
  Removes the given key from the struct with the default implementation.
  """
  @spec pop(t(), atom() | String.t()) :: {any(), t()}
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
