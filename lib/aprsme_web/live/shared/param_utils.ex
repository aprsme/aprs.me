defmodule AprsmeWeb.Live.Shared.ParamUtils do
  @moduledoc """
  Shared parameter parsing, validation, and sanitization utilities.
  Used across multiple LiveView modules for consistent parameter handling.
  """

  @doc """
  Parse float value within specified range with sanitization.
  """
  @spec parse_float_in_range(binary() | any(), float(), float(), float()) :: float()
  def parse_float_in_range(str, default, min, max) when is_binary(str) do
    # Sanitize input first
    sanitized = sanitize_numeric_string(str)

    case Float.parse(sanitized) do
      {val, ""} when val >= min and val <= max ->
        if finite?(val), do: val, else: default

      {val, _remainder} when val >= min and val <= max ->
        # Accept even with trailing characters, but validate the number
        if finite?(val), do: val, else: default

      _ ->
        default
    end
  end

  def parse_float_in_range(_, default, _, _), do: default

  @doc """
  Parse integer value within specified range with sanitization.
  """
  @spec parse_int_in_range(binary() | any(), integer(), integer(), integer()) :: integer()
  def parse_int_in_range(str, default, min, max) when is_binary(str) do
    # Sanitize input first
    sanitized = sanitize_numeric_string(str)

    case Integer.parse(sanitized) do
      {val, ""} when val >= min and val <= max ->
        val

      {val, _remainder} when val >= min and val <= max ->
        # Accept even with trailing characters
        val

      _ ->
        default
    end
  end

  def parse_int_in_range(_, default, _, _), do: default

  @doc """
  Sanitize numeric strings to prevent injection attacks.
  """
  @spec sanitize_numeric_string(binary() | any()) :: binary()
  def sanitize_numeric_string(str) when is_binary(str) do
    # Remove any potentially dangerous characters, keeping only numbers, dots, minus, and e/E for scientific notation
    str
    |> String.trim()
    |> String.replace(~r/[^\d\.\-eE]/, "")
    # Prevent extremely long inputs
    |> limit_string_length(20)
  end

  def sanitize_numeric_string(_), do: ""

  @doc """
  Limit string length to prevent DoS attacks.
  """
  @spec limit_string_length(binary(), integer()) :: binary()
  def limit_string_length(str, max_length) when byte_size(str) > max_length do
    :binary.part(str, 0, max_length)
  end

  def limit_string_length(str, _), do: str

  @doc """
  Check if a float is finite (not infinity or NaN).
  """
  @spec finite?(float() | any()) :: boolean()
  def finite?(float) when is_float(float) do
    # NaN != NaN
    float != :infinity and float != :neg_infinity and float == float
  end

  def finite?(_), do: false

  @doc """
  Safely parse and clamp coordinate values.
  """
  @spec safe_parse_coordinate(binary() | number() | any(), float(), float(), float()) :: float()
  def safe_parse_coordinate(value, default, min, max) when is_binary(value) do
    case parse_float_in_range(value, default, min, max) do
      ^default -> default
      parsed -> clamp_coordinate(parsed, min, max)
    end
  end

  def safe_parse_coordinate(value, default, min, max) when is_number(value) do
    if finite_number?(value) do
      clamp_coordinate(value, min, max)
    else
      default
    end
  end

  def safe_parse_coordinate(_, default, _, _), do: default

  @doc """
  Clamp coordinate value within bounds.
  """
  @spec clamp_coordinate(number(), number(), number()) :: float()
  def clamp_coordinate(value, min, max) when is_number(value) do
    value |> max(min) |> min(max)
  end

  def clamp_coordinate(_, _, _), do: 0.0

  @doc """
  Clamp zoom level within valid range.
  """
  @spec clamp_zoom(binary() | integer() | float() | any()) :: integer()
  def clamp_zoom(zoom) when is_binary(zoom) do
    parse_int_in_range(zoom, 5, 1, 20)
  end

  def clamp_zoom(zoom) when is_integer(zoom) do
    max(1, min(20, zoom))
  end

  def clamp_zoom(zoom) when is_float(zoom) do
    max(1, min(20, trunc(zoom)))
  end

  def clamp_zoom(_), do: 5

  @doc """
  Check if a number is finite.
  """
  @spec finite_number?(number() | any()) :: boolean()
  def finite_number?(num) when is_number(num) do
    # Convert integer to float for check
    finite?(num / 1.0)
  end

  def finite_number?(_), do: false

  @doc """
  Validate callsign format.
  """
  @spec valid_callsign?(binary()) :: boolean()
  def valid_callsign?(callsign) when is_binary(callsign) do
    # Basic callsign validation - alphanumeric with optional dash and suffix
    String.match?(callsign, ~r/^[A-Z0-9]{1,6}(-[A-Z0-9]{1,2})?$/i)
  end

  def valid_callsign?(_), do: false

  @doc """
  Sanitize path string for RF path visualization.
  """
  @spec sanitize_path_string(binary() | any()) :: binary()
  def sanitize_path_string(path) when is_binary(path) do
    # Remove potentially dangerous characters, keep only alphanumeric, dash, comma, space, asterisk
    path
    |> String.trim()
    |> String.replace(~r/[^A-Za-z0-9\-,\s\*]/, "")
    |> limit_string_length(200)
  end

  def sanitize_path_string(_), do: ""

  @doc """
  Converts various numeric types to float for consistent display.
  """
  @spec to_float(any()) :: float()
  def to_float(value) do
    Aprsme.EncodingUtils.to_float(value) || 0.0
  end
end
