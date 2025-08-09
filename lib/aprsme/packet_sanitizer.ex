defmodule Aprsme.PacketSanitizer do
  @moduledoc """
  Sanitizes packet data to ensure it fits within database constraints.
  This module provides truncation for long strings to prevent database errors.
  """

  # Define max lengths for fields that might still have constraints
  # Even though we're migrating to text, this provides safety
  @max_lengths %{
    # Keep reasonable limits for key fields
    base_callsign: 20,
    sender: 20,
    destination: 20,
    ssid: 10,
    data_type: 50,
    symbol_code: 5,
    symbol_table_id: 5,
    region: 50,
    timestamp: 50,
    message_number: 20,
    addressee: 50,
    symboltable: 5,
    symbolcode: 5,
    srccallsign: 20,
    dstcallsign: 20,

    # These fields can be longer but still have sanity limits
    path: 500,
    manufacturer: 100,
    equipment_type: 100,
    format: 100,
    device_identifier: 255,
    item_name: 100,
    object_name: 100,

    # Very long fields - these will be TEXT in DB but we still
    # want to prevent abuse with extremely long data
    information_field: 5000,
    raw_packet: 5000,
    comment: 2000,
    message_text: 2000,
    body: 5000,
    origpacket: 5000,
    header: 1000,
    radiorange: 1000,
    telemetry_bits: 1000
  }

  @doc """
  Sanitizes a packet map by truncating string fields that exceed maximum lengths.
  """
  @spec sanitize_packet(map()) :: map()
  def sanitize_packet(packet) when is_map(packet) do
    Enum.reduce(packet, %{}, fn {key, value}, acc ->
      sanitized_value = sanitize_field(key, value)
      Map.put(acc, key, sanitized_value)
    end)
  end

  defp sanitize_field(key, value) when is_binary(value) do
    case Map.get(@max_lengths, key) do
      nil ->
        # No limit defined, return as-is
        value

      max_length ->
        truncate_string(value, max_length)
    end
  end

  defp sanitize_field(_key, value), do: value

  defp truncate_string(string, max_length) when byte_size(string) <= max_length do
    string
  end

  defp truncate_string(string, max_length) do
    # Use binary_part to safely truncate at byte boundaries
    # This prevents splitting UTF-8 characters
    truncated = binary_part(string, 0, max_length)

    # Ensure we don't end in the middle of a UTF-8 character
    if String.valid?(truncated) do
      truncated
    else
      truncate_to_valid_utf8(string, max_length)
    end
  end

  defp truncate_to_valid_utf8(string, max_length) do
    # Work backwards from max_length to find a valid UTF-8 boundary
    truncate_to_valid_utf8(string, max_length - 1, max_length)
  end

  defp truncate_to_valid_utf8(_string, 0, _original_max), do: ""

  defp truncate_to_valid_utf8(string, current_length, original_max) do
    truncated = binary_part(string, 0, current_length)

    if String.valid?(truncated) do
      truncated
    else
      truncate_to_valid_utf8(string, current_length - 1, original_max)
    end
  end
end
