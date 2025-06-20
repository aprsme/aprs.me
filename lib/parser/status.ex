defmodule Parser.Status do
  @moduledoc """
  APRS status parsing.
  """

  @doc """
  Parse an APRS status string. Returns a struct or error.
  """
  @spec parse(String.t()) :: map() | nil
  def parse(<<">", status_text::binary>>), do: %{status_text: status_text, data_type: :status}
  def parse(data), do: %{status_text: data, data_type: :status}
end
