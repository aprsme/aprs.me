defmodule Aprsme.Encoding do
  @moduledoc """
  Encoding utilities for handling APRS packet data.
  Provides functions for sanitizing strings, converting encodings,
  and validating data.
  """

  @doc """
  Sanitizes a binary to ensure it can be safely JSON encoded.
  Handles latin1 conversion and removes control characters.
  """
  @spec sanitize_string(binary()) :: binary()
  def sanitize_string(input) when is_binary(input) do
    input
    |> try_utf8_conversion()
    |> clean_control_characters()
  end

  @spec sanitize_string(any()) :: binary()
  def sanitize_string(_), do: ""

  @doc """
  Type-safe float conversion with validation
  """
  @spec to_float_safe(binary()) :: {:ok, float()} | nil
  def to_float_safe(value) when is_binary(value) do
    sanitized = value
                |> String.trim()
                |> String.slice(0, 30)  # Reasonable max length for a number

    case Float.parse(sanitized) do
      {f, _} when f > -9.0e15 and f < 9.0e15 ->
        {:ok, f}
      _ ->
        nil
    end
  end

  @spec to_float_safe(any()) :: nil
  def to_float_safe(_), do: nil

  @doc """
  Convert binary to hex string
  """
  @spec to_hex(binary()) :: binary()
  def to_hex(input) when is_binary(input) do
    Base.encode16(input)
  end

  @spec to_hex(any()) :: binary()
  def to_hex(_), do: ""

  @doc """
  Check if a value looks like it has weather data
  """
  @spec has_weather_data(any(), any(), any(), any()) :: boolean()
  def has_weather_data(temperature, humidity, wind_speed, pressure) do
    not is_nil(temperature) or
    not is_nil(humidity) or
    not is_nil(wind_speed) or
    not is_nil(pressure)
  end

  @doc """
  Get encoding information about a binary
  """
  @spec encoding_info(binary()) :: %{
    valid_utf8: boolean(),
    byte_count: non_neg_integer(),
    char_count: non_neg_integer() | nil,
    invalid_at: non_neg_integer() | nil
  }
  def encoding_info(input) when is_binary(input) do
    byte_count = byte_size(input)
    
    case String.valid?(input) do
      true ->
        %{
          valid_utf8: true,
          byte_count: byte_count,
          char_count: String.length(input),
          invalid_at: nil
        }
      false ->
        invalid_pos = find_invalid_byte_position(input)
        %{
          valid_utf8: false,
          byte_count: byte_count,
          char_count: nil,
          invalid_at: invalid_pos
        }
    end
  end

  @spec encoding_info(any()) :: %{
    valid_utf8: boolean(),
    byte_count: non_neg_integer(),
    char_count: nil,
    invalid_at: nil
  }
  def encoding_info(_) do
    %{
      valid_utf8: false,
      byte_count: 0,
      char_count: nil,
      invalid_at: nil
    }
  end

  # Private functions

  @spec try_utf8_conversion(binary()) :: binary()
  defp try_utf8_conversion(input) do
    if String.valid?(input) do
      input
    else
      # Try latin1 to UTF-8 conversion
      latin1_to_utf8(input)
    end
  end

  @spec latin1_to_utf8(binary()) :: binary()
  defp latin1_to_utf8(input) do
    try do
      input
      |> :binary.bin_to_list()
      |> Enum.map(&latin1_char_to_utf8/1)
      |> IO.iodata_to_binary()
    rescue
      _ -> ""
    end
  end

  @spec latin1_char_to_utf8(0..255) :: binary()
  defp latin1_char_to_utf8(byte) when byte <= 127 do
    <<byte>>
  end

  defp latin1_char_to_utf8(byte) do
    # For Latin1, values 128-255 map to Unicode U+0080 to U+00FF
    # In UTF-8, these become 2-byte sequences: 110xxxxx 10xxxxxx
    <<0xC0 + div(byte, 64), 0x80 + rem(byte, 64)>>
  end

  @spec clean_control_characters(binary()) :: binary()
  defp clean_control_characters(s) do
    s
    |> String.graphemes()
    |> Enum.filter(&valid_grapheme?/1)
    |> Enum.join()
    |> String.trim()
  end

  @spec valid_grapheme?(String.grapheme()) :: boolean()
  defp valid_grapheme?(grapheme) do
    case String.to_charlist(grapheme) do
      [cp] ->
        # Allow tab (0x09), newline (0x0A), carriage return (0x0D)
        # Remove other control characters
        case cp do
          9 -> true
          10 -> true
          13 -> true
          c when c >= 0 and c <= 31 -> false
          127 -> false
          c when c >= 128 and c <= 159 -> false
          _ -> true
        end
      _ ->
        # Multi-codepoint grapheme, keep it
        true
    end
  end

  @spec find_invalid_byte_position(binary()) :: non_neg_integer() | nil
  defp find_invalid_byte_position(input) do
    input
    |> :binary.bin_to_list()
    |> Enum.with_index()
    |> Enum.find_value(fn {byte, index} ->
      if byte > 127 and not valid_utf8_continuation?(input, index) do
        index
      end
    end)
  end

  @spec valid_utf8_continuation?(binary(), non_neg_integer()) :: boolean()
  defp valid_utf8_continuation?(binary, pos) do
    try do
      <<_::binary-size(pos), char::utf8, _::binary>> = binary
      true
    rescue
      _ -> false
    end
  end
end