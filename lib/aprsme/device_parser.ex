defmodule Aprsme.DeviceParser do
  @moduledoc """
  Extracts device identifiers from APRS packet data using pattern matching.
  """

  @doc """
  Extracts a device identifier from packet data.
  Returns the device identifier string or nil if not found.
  """
  @spec extract_device_identifier(map()) :: String.t() | nil
  def extract_device_identifier(packet_data) when is_map(packet_data) do
    find_device_identifier(packet_data)
  end

  def extract_device_identifier(_), do: nil

  # Pattern matching for different data structures
  defp find_device_identifier(%{device_identifier: id}) when is_binary(id) and id != "", do: id
  defp find_device_identifier(%{"device_identifier" => id}) when is_binary(id) and id != "", do: id
  defp find_device_identifier(%{destination: dest}) when is_binary(dest) and dest != "", do: dest
  defp find_device_identifier(%{"destination" => dest}) when is_binary(dest) and dest != "", do: dest

  defp find_device_identifier(%{data_extended: data_ext} = packet) when is_map(data_ext) do
    extract_from_symbol_data(data_ext) || find_device_identifier(Map.delete(packet, :data_extended))
  end

  defp find_device_identifier(%{"data_extended" => data_ext} = packet) when is_map(data_ext) do
    extract_from_symbol_data(data_ext) || find_device_identifier(Map.delete(packet, "data_extended"))
  end

  defp find_device_identifier(_), do: nil

  # Pattern matching for symbol data extraction
  defp extract_from_symbol_data(%{symbol_table_id: table, symbol_code: code})
       when is_binary(table) and is_binary(code) do
    "#{table}#{code}"
  end

  defp extract_from_symbol_data(%{"symbol_table_id" => table, "symbol_code" => code})
       when is_binary(table) and is_binary(code) do
    "#{table}#{code}"
  end

  defp extract_from_symbol_data(_), do: nil

  @doc """
  Normalizes a device identifier to a canonical form.
  Handles both string and atom inputs with pattern matching.
  """
  @spec normalize_device_identifier(String.t() | atom() | nil) :: String.t() | nil
  def normalize_device_identifier(nil), do: nil
  def normalize_device_identifier(id) when is_atom(id), do: Atom.to_string(id)
  def normalize_device_identifier(id) when is_binary(id), do: String.trim(id)
  def normalize_device_identifier(_), do: nil
end
