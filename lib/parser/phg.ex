defmodule Parser.PHG do
  @moduledoc """
  PHG/DFS parsing for APRS packets.
  """

  alias Parser.Types.ParseError

  @doc """
  Parse a PHG/DFS string. Returns a struct or ParseError.
  """
  @spec parse(String.t()) :: map() | ParseError.t()
  def parse(_phg_str) do
    # Stub: actual logic to be implemented
    %ParseError{
      error_code: :not_implemented,
      error_message: "PHG/DFS parsing not yet implemented",
      raw_data: nil
    }
  end
end
