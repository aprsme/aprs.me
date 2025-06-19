defmodule Parser.AX25 do
  @moduledoc """
  AX.25 callsign and path parsing/validation for APRS packets.
  """

  @doc """
  Parse and validate an AX.25 callsign. Returns {:ok, {base, ssid}} or {:error, reason}.
  """
  @spec parse_callsign(String.t()) :: {:ok, {String.t(), String.t()}} | {:error, String.t()}
  def parse_callsign(callsign) do
    cond do
      not is_binary(callsign) ->
        {:error, "Invalid callsign format"}

      byte_size(callsign) == 0 ->
        {:error, "Empty callsign"}

      String.contains?(callsign, "-") ->
        case String.split(callsign, "-") do
          [base, ssid] -> {:ok, {base, ssid}}
          _ -> {:ok, {callsign, "0"}}
        end

      true ->
        {:ok, {callsign, "0"}}
    end
  end

  @doc """
  Parse and validate an AX.25 path. Returns {:ok, [String.t()]} or {:error, reason}.
  """
  @spec parse_path(String.t()) :: {:ok, [String.t()]} | {:error, String.t()}
  def parse_path(_path) do
    # Stub: actual logic to be implemented
    {:error, "Not yet implemented"}
  end
end
