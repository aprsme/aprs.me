defmodule Aprsme.DeviceParser do
  @moduledoc """
  Extracts device identifiers from APRS packet data.
  """

  @doc """
  Extracts a device identifier from packet data.
  Returns the device identifier string or nil if not found.
  """
  @spec extract_device_identifier(map()) :: String.t() | nil
  def extract_device_identifier(packet_data) do
    extract_from_destination(packet_data) ||
      extract_from_device_identifier(packet_data) ||
      extract_from_data_extended(packet_data)
  end

  defp extract_from_destination(packet_data) do
    get_field_value(packet_data, :destination) || get_field_value(packet_data, "destination")
  end

  defp extract_from_device_identifier(packet_data) do
    get_field_value(packet_data, :device_identifier) || get_field_value(packet_data, "device_identifier")
  end

  defp get_field_value(packet_data, key) do
    if Map.has_key?(packet_data, key) and not is_nil(packet_data[key]) do
      packet_data[key]
    end
  end

  defp extract_from_data_extended(packet_data) do
    data_extended = Map.get(packet_data, :data_extended) || %{}

    # Try to extract from symbol information
    symbol_table_id = Map.get(data_extended, :symbol_table_id) || Map.get(data_extended, "symbol_table_id")
    symbol_code = Map.get(data_extended, :symbol_code) || Map.get(data_extended, "symbol_code")

    if symbol_table_id && symbol_code do
      "#{symbol_table_id}#{symbol_code}"
    end
  end
end
