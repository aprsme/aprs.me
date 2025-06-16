defmodule Aprs.Passcode do
  @moduledoc """
  Module for generating APRS passcodes from callsigns.
  The passcode is a hash of the callsign used for authentication with APRS-IS servers.
  """

  @doc """
  Generates an APRS passcode for a given callsign.

  ## Parameters
    - callsign: The amateur radio callsign (e.g., "W5ISP")

  ## Returns
    - The generated passcode as an integer

  ## Examples
      iex> Aprs.Passcode.generate("W5ISP")
      12345
  """
  def generate(callsign) when is_binary(callsign) do
    # Split on '-' and take first part, uppercase, and limit to 10 chars
    realcall =
      callsign
      |> String.split("-")
      |> List.first()
      |> String.upcase()
      |> String.slice(0, 10)

    # Convert string to charlist for easier processing
    chars = String.to_charlist(realcall)

    # Initial hash value
    hash = 0x73E2

    # Process characters in pairs
    hash =
      Enum.reduce_every(chars, 2, hash, fn [char1, char2], acc ->
        acc
        |> Bitwise.bxor(Bitwise.bsl(char1, 8))
        |> Bitwise.bxor(char2)
      end)

    # Handle odd length callsigns
    hash =
      if rem(length(chars), 2) == 1 do
        last_char = List.last(chars)
        Bitwise.bxor(hash, Bitwise.bsl(last_char, 8))
      else
        hash
      end

    # Return final hash masked to 15 bits
    Bitwise.band(hash, 0x7FFF)
  end
end
