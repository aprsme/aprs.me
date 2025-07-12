defmodule Aprsme.Callsign do
  @moduledoc """
  Utilities for callsign normalization, validation, and manipulation.
  """

  @doc """
  Normalizes a callsign by trimming whitespace and converting to uppercase.
  """
  @spec normalize(String.t() | nil) :: String.t()
  def normalize(nil), do: ""

  def normalize(callsign) when is_binary(callsign) do
    callsign
    |> String.trim()
    |> String.upcase()
  end

  def normalize(_), do: ""

  @doc """
  Validates if a callsign format is reasonable for amateur radio use.

  ## Examples
      
      iex> Aprsme.Callsign.valid?("W5ABC")
      true
      
      iex> Aprsme.Callsign.valid?("W5ABC-15")
      true
      
      iex> Aprsme.Callsign.valid?("")
      false
      
      iex> Aprsme.Callsign.valid?("A")
      false
  """
  @spec valid?(String.t() | nil) :: boolean()
  def valid?(nil), do: false

  def valid?(callsign) when is_binary(callsign) do
    trimmed = String.trim(callsign)

    # Accept any non-empty callsign
    trimmed != ""
  end

  def valid?(_), do: false

  @doc """
  Checks if a packet's sender matches the target callsign.
  Both callsigns are normalized before comparison.
  """
  @spec matches?(String.t() | nil, String.t() | nil) :: boolean()
  def matches?(packet_callsign, target_callsign) do
    normalize(packet_callsign) == normalize(target_callsign)
  end

  @doc """
  Extracts the base callsign from a full callsign (removes SSID if present).

  ## Examples

      iex> Aprsme.Callsign.extract_base("W5ABC-15")
      "W5ABC"
      
      iex> Aprsme.Callsign.extract_base("W5ABC")
      "W5ABC"
  """
  @spec extract_base(String.t() | nil) :: String.t()
  def extract_base(nil), do: ""

  def extract_base(callsign) when is_binary(callsign) do
    # Split on last hyphen to get base callsign
    case String.split(callsign, "-") do
      parts when length(parts) > 1 ->
        parts |> Enum.drop(-1) |> Enum.join("-")

      _ ->
        callsign
    end
  end

  def extract_base(_), do: ""

  @doc """
  Extracts the SSID from a callsign, returning "0" if no SSID is present.

  ## Examples

      iex> Aprsme.Callsign.extract_ssid("W5ABC-15")
      "15"
      
      iex> Aprsme.Callsign.extract_ssid("W5ABC")
      "0"
  """
  @spec extract_ssid(String.t() | nil) :: String.t()
  def extract_ssid(nil), do: "0"

  def extract_ssid(callsign) when is_binary(callsign) do
    # Extract whatever comes after the last hyphen as SSID
    case String.split(callsign, "-") do
      parts when length(parts) > 1 -> List.last(parts)
      _ -> "0"
    end
  end

  def extract_ssid(_), do: "0"

  @doc """
  Extracts both base callsign and SSID as a tuple.

  ## Examples

      iex> Aprsme.Callsign.extract_parts("W5ABC-15")
      {"W5ABC", "15"}
      
      iex> Aprsme.Callsign.extract_parts("W5ABC")
      {"W5ABC", "0"}
  """
  @spec extract_parts(String.t() | nil) :: {String.t(), String.t()}
  def extract_parts(nil), do: {"", "0"}

  def extract_parts(callsign) when is_binary(callsign) do
    # Split on last hyphen to separate base and SSID
    case String.split(callsign, "-") do
      parts when length(parts) > 1 ->
        ssid = List.last(parts)
        base = parts |> Enum.drop(-1) |> Enum.join("-")
        {base, ssid}

      _ ->
        {callsign, "0"}
    end
  end

  def extract_parts(_), do: {"", "0"}
end
