defmodule Aprsme.EncodingUtils do
  @moduledoc """
  Encoding utilities for handling APRS packet data.
  Provides functions for sanitizing strings, converting encodings,
  and validating data.
  """

  alias Aprs.Types.MicE
  alias Aprsme.Encoding

  @doc """
  Sanitizes a binary to ensure it can be safely JSON encoded.

  If the binary is valid UTF-8, returns it as-is.
  If it contains invalid UTF-8 sequences, replaces them with the Unicode
  replacement character (�) or removes them entirely.

  ## Examples

      iex> Aprsme.EncodingUtils.sanitize_string("Hello World")
      "Hello World"
      
      iex> Aprsme.EncodingUtils.sanitize_string(<<72, 101, 108, 108, 111, 211, 87, 111, 114, 108, 100>>)
      "HelloÓWorld"
  """
  @spec sanitize_string(binary() | nil | any()) :: binary() | nil | any()
  def sanitize_string(binary) when is_binary(binary) do
    Encoding.sanitize_string(binary)
  end

  def sanitize_string(nil), do: nil
  def sanitize_string(other), do: other

  @doc """
  Converts various types to float with validation for safety.

  ## Examples

      iex> Aprsme.EncodingUtils.to_float(1)
      1.0
      iex> Aprsme.EncodingUtils.to_float(1.5)
      1.5
      iex> Aprsme.EncodingUtils.to_float("2.3")
      2.3
      iex> Aprsme.EncodingUtils.to_float("bad")
      nil
      iex> Aprsme.EncodingUtils.to_float(nil)
      nil
  """
  @spec to_float(any()) :: float() | nil
  def to_float(value) when is_float(value) do
    # In Elixir, floats are always finite (no infinity or NaN)
    value
  end

  def to_float(value) when is_integer(value) do
    if value >= -9.0e15 and value <= 9.0e15 do
      value * 1.0
    else
      nil
    end
  end

  def to_float(value) when is_binary(value) do
    # Sanitize and use Gleam's safe conversion
    sanitized = value |> sanitize_string() |> to_string()

    case Encoding.to_float_safe(sanitized) do
      {:ok, f} -> f
      nil -> nil
    end
  end

  def to_float(%Decimal{} = value) do
    Decimal.to_float(value)
  end

  def to_float(_), do: nil


  @doc """
  Converts various types to Decimal for database storage.

  ## Examples

      iex> is_struct(Aprsme.EncodingUtils.to_decimal(1), Decimal)
      true
      iex> is_struct(Aprsme.EncodingUtils.to_decimal(1.5), Decimal)
      true
      iex> is_struct(Aprsme.EncodingUtils.to_decimal("2.3"), Decimal)
      true
      iex> Aprsme.EncodingUtils.to_decimal("bad")
      nil
      iex> Aprsme.EncodingUtils.to_decimal(nil)
      nil
  """
  @spec to_decimal(any()) :: Decimal.t() | nil
  def to_decimal(%Decimal{} = d), do: d
  def to_decimal(f) when is_float(f), do: Decimal.from_float(f)
  def to_decimal(i) when is_integer(i), do: Decimal.new(i)

  def to_decimal(s) when is_binary(s) do
    case Decimal.parse(s) do
      {d, _} ->
        d

      :error ->
        case Float.parse(s) do
          {f, _} -> Decimal.from_float(f)
          :error -> nil
        end
    end
  end

  def to_decimal(_), do: nil

  @doc """
  Sanitizes all string fields in packet data before database storage.

  ## Examples

      iex> Aprsme.EncodingUtils.sanitize_packet_strings(["abc", <<255>>])
      ["abc", "ÿ"]
      iex> Aprsme.EncodingUtils.sanitize_packet_strings(%{"foo" => <<0, 65, 66, 67>>})
      %{"foo" => "ABC"}
      iex> Aprsme.EncodingUtils.sanitize_packet_strings(nil)
      nil
  """
  @spec sanitize_packet_strings(any()) :: any()
  def sanitize_packet_strings(%DateTime{} = dt), do: dt
  def sanitize_packet_strings(%NaiveDateTime{} = ndt), do: ndt
  def sanitize_packet_strings(%_struct{} = struct), do: struct |> Map.from_struct() |> sanitize_packet_strings()
  def sanitize_packet_strings(list) when is_list(list), do: Enum.map(list, &sanitize_packet_strings/1)

  def sanitize_packet_strings(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {key, value}, acc ->
      Map.put(acc, key, sanitize_packet_strings(value))
    end)
  end

  def sanitize_packet_strings(binary) when is_binary(binary) do
    s = sanitize_string(binary)
    if is_binary(s), do: s, else: ""
  end

  def sanitize_packet_strings(other), do: other

  @doc """
  Normalizes data_type field to ensure it's always a string.

  ## Examples

      iex> Aprsme.EncodingUtils.normalize_data_type(%{data_type: :weather})
      %{data_type: "weather"}
      iex> Aprsme.EncodingUtils.normalize_data_type(%{"data_type" => :foo})
      %{"data_type" => "foo"}
      iex> Aprsme.EncodingUtils.normalize_data_type(%{data_type: "bar"})
      %{data_type: "bar"}
      iex> Aprsme.EncodingUtils.normalize_data_type(%{"data_type" => "baz"})
      %{"data_type" => "baz"}
      iex> Aprsme.EncodingUtils.normalize_data_type(%{foo: 1})
      %{foo: 1}
  """
  @spec normalize_data_type(map()) :: map()
  def normalize_data_type(%{data_type: data_type} = attrs) when is_atom(data_type) do
    %{attrs | data_type: to_string(data_type)}
  end

  def normalize_data_type(%{"data_type" => data_type} = attrs) when is_atom(data_type) do
    %{attrs | "data_type" => to_string(data_type)}
  end

  def normalize_data_type(attrs) when is_map(attrs) do
    case {Map.has_key?(attrs, :data_type), Map.get(attrs, :data_type)} do
      {true, data_type} when is_atom(data_type) ->
        %{attrs | data_type: to_string(data_type)}

      _ ->
        attrs
    end
  end

  def normalize_data_type(attrs), do: attrs

  @doc """
  List of weather-related fields used for packet classification.

  ## Examples

      iex> Aprsme.EncodingUtils.weather_fields() |> Enum.member?(:temperature)
      true
      iex> :foo in Aprsme.EncodingUtils.weather_fields()
      false
  """
  @spec weather_fields() :: [atom()]
  def weather_fields do
    [
      :temperature,
      :humidity,
      :wind_speed,
      :wind_direction,
      :wind_gust,
      :pressure,
      :rain_1h,
      :rain_24h,
      :rain_since_midnight,
      :snow,
      :luminosity
    ]
  end

  @doc """
  Sanitizes all string fields in an APRS packet to ensure safe JSON encoding.

  ## Examples

      iex> result = Aprsme.EncodingUtils.sanitize_packet(%{"information_field" => <<0, 65, 66, 67>>, "data_extended" => %{"comment" => <<0, 68, 69, 70>>}})
      iex> result["information_field"] == "ABC"
      true
      iex> result["data_extended"]["comment"] == "DEF"
      true
  """
  @spec sanitize_packet(struct() | map()) :: struct() | map()
  def sanitize_packet(%Aprsme.Packet{} = packet) do
    %{
      packet
      | information_field: sanitize_string(packet.information_field),
        data_extended: sanitize_data_extended(packet.data_extended)
    }
  end

  def sanitize_packet(packet) when is_map(packet) do
    # Handle all known string fields, checking for both atom and string keys
    string_fields = [
      :information_field,
      :comment,
      :path,
      :raw_packet,
      :destination,
      :sender,
      :base_callsign,
      :ssid,
      :manufacturer,
      :equipment_type,
      :message_text,
      :addressee,
      :symbol_code,
      :symbol_table_id,
      :dao,
      :timestamp,
      :device_identifier
    ]

    # Sanitize all string fields
    sanitized =
      Enum.reduce(string_fields, packet, fn field, acc ->
        atom_key = field
        string_key = to_string(field)

        cond do
          Map.has_key?(acc, atom_key) ->
            Map.update(acc, atom_key, nil, &sanitize_string/1)

          Map.has_key?(acc, string_key) ->
            Map.update(acc, string_key, nil, &sanitize_string/1)

          true ->
            acc
        end
      end)

    # Handle data_extended separately
    cond do
      Map.has_key?(sanitized, :data_extended) ->
        Map.update(sanitized, :data_extended, nil, &sanitize_data_extended/1)

      Map.has_key?(sanitized, "data_extended") ->
        Map.update(sanitized, "data_extended", nil, &sanitize_data_extended/1)

      true ->
        sanitized
    end
  end

  @doc """
  Sanitizes string fields in the data_extended structure.

  ## Examples

      iex> Aprsme.EncodingUtils.sanitize_data_extended(%{"comment" => <<0, 65, 66, 67>>})
      %{"comment" => "ABC"}
      iex> Aprsme.EncodingUtils.sanitize_data_extended(nil)
      nil
  """
  @spec sanitize_data_extended(nil | map() | MicE.t() | any()) :: nil | map() | MicE.t() | any()
  def sanitize_data_extended(nil), do: nil

  def sanitize_data_extended(%{comment: comment} = data_extended) when is_map(data_extended) do
    %{data_extended | comment: sanitize_string(comment)}
  end

  def sanitize_data_extended(%MicE{message: message} = mic_e) do
    %{mic_e | message: sanitize_string(message)}
  end

  def sanitize_data_extended(data_extended) when is_map(data_extended) and not is_struct(data_extended) do
    # Handle generic maps by sanitizing all string values
    Enum.reduce(data_extended, %{}, fn {key, value}, acc ->
      sanitized_value = sanitize_map_value(value)
      Map.put(acc, key, sanitized_value)
    end)
  end

  def sanitize_data_extended(data_extended), do: data_extended

  @spec sanitize_map_value(any()) :: any()
  defp sanitize_map_value(val) when is_binary(val), do: sanitize_string(val)
  defp sanitize_map_value(val), do: val

  @doc """
  Converts a binary to a hex string representation for debugging.

  ## Examples

      iex> Aprsme.EncodingUtils.to_hex(<<72, 101, 108, 108, 111>>)
      "48656C6C6F"
  """
  @spec to_hex(binary()) :: String.t()
  def to_hex(binary) when is_binary(binary) do
    Encoding.to_hex(binary)
  end

  @doc """
  Returns information about a binary's encoding validity.

  ## Examples

      iex> Aprsme.EncodingUtils.encoding_info("Hello")
      %{valid_utf8: true, byte_count: 5, char_count: 5, invalid_at: nil}
      iex> Aprsme.EncodingUtils.encoding_info(<<72, 101, 211, 108, 111>>)
      %{valid_utf8: false, byte_count: 5, char_count: nil, invalid_at: 2}
  """
  @spec encoding_info(binary()) :: map()
  def encoding_info(binary) when is_binary(binary) do
    Encoding.encoding_info(binary)
  end
end
