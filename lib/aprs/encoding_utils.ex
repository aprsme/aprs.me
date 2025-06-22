defmodule Aprs.EncodingUtils do
  @moduledoc """
  Utilities for handling encoding issues in APRS packet data.

  APRS packets can contain arbitrary bytes that may not be valid UTF-8,
  which causes issues when trying to JSON encode the data for transmission
  to web clients.
  """

  alias Parser.Types.MicE

  @doc """
  Sanitizes a binary to ensure it can be safely JSON encoded.

  If the binary is valid UTF-8, returns it as-is.
  If it contains invalid UTF-8 sequences, replaces them with the Unicode
  replacement character (�) or removes them entirely.

  ## Examples

      iex> Aprs.EncodingUtils.sanitize_string("Hello World")
      "Hello World"

      iex> Aprs.EncodingUtils.sanitize_string(<<72, 101, 108, 108, 111, 211, 87, 111, 114, 108, 100>>)
      "HelloWorld"
  """
  @spec sanitize_string(binary() | nil | any()) :: binary() | nil | any()
  def sanitize_string(binary) when is_binary(binary) do
    # Always scrub problematic bytes, even from valid UTF-8 strings
    # PostgreSQL rejects null bytes and other control characters even if they're valid UTF-8
    scrub_problematic_bytes(binary)
  end

  def sanitize_string(nil), do: nil
  def sanitize_string(other), do: other

  @doc """
  Sanitizes all string fields in an APRS packet to ensure safe JSON encoding.
  """
  @spec sanitize_packet(struct() | map()) :: struct() | map()
  def sanitize_packet(%Aprs.Packet{} = packet) do
    %{
      packet
      | information_field: sanitize_string(packet.information_field),
        data_extended: sanitize_data_extended(packet.data_extended)
    }
  end

  def sanitize_packet(packet) when is_map(packet) do
    packet
    |> Map.update(:information_field, nil, &sanitize_string/1)
    |> Map.update(:data_extended, nil, &sanitize_data_extended/1)
  end

  @doc """
  Sanitizes string fields in the data_extended structure.
  """
  @spec sanitize_data_extended(nil | map() | MicE.t() | any()) :: nil | map() | MicE.t() | any()
  def sanitize_data_extended(nil), do: nil

  def sanitize_data_extended(%{comment: comment} = data_extended) when is_map(data_extended) do
    %{data_extended | comment: sanitize_string(comment)}
  end

  def sanitize_data_extended(%MicE{message: message} = mic_e) do
    %{mic_e | message: sanitize_string(message)}
  end

  def sanitize_data_extended(data_extended) when is_map(data_extended) do
    # Handle generic maps by sanitizing all string values
    Enum.reduce(data_extended, %{}, fn {key, value}, acc ->
      sanitized_value = sanitize_map_value(value)
      Map.put(acc, key, sanitized_value)
    end)
  end

  def sanitize_data_extended(data_extended), do: data_extended

  defp sanitize_map_value(val) when is_binary(val), do: sanitize_string(val)
  defp sanitize_map_value(val), do: val

  # Private helper functions

  @spec scrub_problematic_bytes(binary()) :: String.t()
  defp scrub_problematic_bytes(binary) do
    # Handle both invalid UTF-8 sequences and problematic control characters
    cleaned = clean_binary_by_validity(binary, String.valid?(binary))
    String.trim(cleaned)
  end

  defp clean_binary_by_validity(binary, true) do
    # String is valid UTF-8, just remove control characters
    binary
    # Remove null bytes
    |> String.replace(<<0>>, "")
    # Remove other control chars
    |> String.replace(~r/[\x01-\x08\x0B\x0C\x0E-\x1F\x7F]/, "")
  end

  defp clean_binary_by_validity(binary, false) do
    # String has invalid UTF-8, filter byte by byte
    binary
    |> :binary.bin_to_list()
    |> Enum.filter(&valid_utf8_byte?/1)
    |> :binary.list_to_bin()
    |> ensure_valid_utf8()
  end

  # Check if byte should be kept (ASCII printable + safe whitespace)
  @spec valid_utf8_byte?(integer()) :: boolean()
  defp valid_utf8_byte?(byte) when byte >= 32 and byte <= 126, do: true
  defp valid_utf8_byte?(byte) when byte in [9, 10, 13], do: true
  defp valid_utf8_byte?(_), do: false

  # Ensure the final result is valid UTF-8
  @spec ensure_valid_utf8(binary()) :: binary()
  defp ensure_valid_utf8(binary) do
    if String.valid?(binary) do
      binary
    else
      try_convert_utf8(binary)
    end
  end

  defp try_convert_utf8(binary) do
    case :unicode.characters_to_binary(binary, :latin1, :utf8) do
      result when is_binary(result) ->
        result

      _ ->
        # Last resort: keep only ASCII
        fallback_to_ascii(binary)
    end
  end

  defp fallback_to_ascii(binary) do
    binary
    |> :binary.bin_to_list()
    |> Enum.filter(fn byte -> byte >= 32 and byte <= 126 end)
    |> :binary.list_to_bin()
  end

  @doc """
  Converts a binary to a hex string representation for debugging.

  ## Examples

      iex> Aprs.EncodingUtils.to_hex(<<72, 101, 108, 108, 111>>)
      "48656C6C6F"
  """
  @spec to_hex(binary()) :: String.t()
  def to_hex(binary) when is_binary(binary) do
    binary
    |> :binary.bin_to_list()
    |> Enum.map(&Integer.to_string(&1, 16))
    |> Enum.map_join("", &String.pad_leading(&1, 2, "0"))
  end

  @doc """
  Returns information about a binary's encoding validity.

  ## Examples

      iex> Aprs.EncodingUtils.encoding_info("Hello")
      %{valid_utf8: true, byte_count: 5, char_count: 5}

      iex> Aprs.EncodingUtils.encoding_info(<<72, 101, 211, 108, 111>>)
      %{valid_utf8: false, byte_count: 5, invalid_at: 2}
  """
  @spec encoding_info(binary()) :: map()
  def encoding_info(binary) when is_binary(binary) do
    valid = String.valid?(binary)
    byte_count = byte_size(binary)

    base_info = %{
      valid_utf8: valid,
      byte_count: byte_count
    }

    add_encoding_details(base_info, binary, valid)
  end

  defp add_encoding_details(base_info, binary, true) do
    Map.put(base_info, :char_count, String.length(binary))
  end

  defp add_encoding_details(base_info, binary, false) do
    # Try to find where the invalid sequence starts
    invalid_at = find_invalid_byte_position(binary, 0)
    Map.put(base_info, :invalid_at, invalid_at)
  end

  @spec find_invalid_byte_position(binary(), non_neg_integer()) :: non_neg_integer() | nil
  defp find_invalid_byte_position(<<>>, _pos), do: nil

  defp find_invalid_byte_position(<<head::binary-size(1), tail::binary>>, pos) do
    if String.valid?(head) do
      find_invalid_byte_position(tail, pos + 1)
    else
      pos
    end
  end

  defp find_invalid_byte_position(_, pos), do: pos
end
