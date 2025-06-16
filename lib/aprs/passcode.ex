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
      15748
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

    # Process characters in pairs, incrementing by 2 each time
    hash =
      Enum.reduce(0..div(length(chars) - 1, 2), hash, fn i, acc ->
        current_char = Enum.at(chars, i * 2)
        next_char = if i * 2 + 1 < length(chars), do: Enum.at(chars, i * 2 + 1)
        step1 = Bitwise.bxor(acc, Bitwise.bsl(current_char, 8))

        step2 =
          if next_char do
            Bitwise.bxor(step1, next_char)
          else
            step1
          end

        step2
      end)

    # Return final hash masked to 15 bits
    Bitwise.band(hash, 0x7FFF)
  end
end
