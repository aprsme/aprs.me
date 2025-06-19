defmodule Parser.NMEA do
  @moduledoc """
  NMEA sentence parsing for APRS packets.
  """

  alias Parser.Types.ParseError

  @doc """
  Parse an NMEA sentence string. Returns a struct or ParseError.
  """
  @spec parse(String.t()) :: map() | ParseError.t()
  def parse(_nmea_sentence) do
    # Stub: actual logic to be implemented
    %ParseError{
      error_code: :not_implemented,
      error_message: "NMEA parsing not yet implemented",
      raw_data: nil
    }
  end
end
