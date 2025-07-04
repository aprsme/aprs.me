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
    cond do
      Map.has_key?(packet_data, :destination) and not is_nil(packet_data[:destination]) ->
        packet_data[:destination]
      Map.has_key?(packet_data, "destination") and not is_nil(packet_data["destination"]) ->
        packet_data["destination"]
      Map.has_key?(packet_data, :device_identifier) and not is_nil(packet_data[:device_identifier]) ->
        packet_data[:device_identifier]
      Map.has_key?(packet_data, "device_identifier") and not is_nil(packet_data["device_identifier"]) ->
        packet_data["device_identifier"]
      true ->
        extract_from_data_extended(packet_data)
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
