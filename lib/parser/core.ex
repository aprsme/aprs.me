defmodule Parser.Core do
  @moduledoc """
  Main entry point for APRS packet parsing. Delegates to submodules for specific formats.
  """

  alias Parser.Types.Packet
  alias Parser.Types.ParseError

  @doc """
  Parse an APRS packet string into a Packet struct or return a ParseError struct.
  """
  @spec parse(String.t()) :: {:ok, Packet.t()} | {:error, ParseError.t()}
  def parse(_packet) do
    # Stub: actual logic will delegate to submodules
    {:error,
     %ParseError{
       error_code: :not_implemented,
       error_message: "Not yet implemented",
       raw_data: nil
     }}
  end
end
