defmodule Parser.KISSHelpers do
  @moduledoc """
  KISS/TNC2 conversion helpers for APRS.
  """

  @spec kiss_to_tnc2(binary()) :: binary() | map()
  def kiss_to_tnc2(<<0xC0, 0x00, rest::binary>>) do
    tnc2 =
      rest
      |> String.trim_trailing(<<0xC0>>)
      |> String.replace(<<0xDB, 0xDC>>, <<0xC0>>)
      |> String.replace(<<0xDB, 0xDD>>, <<0xDB>>)

    tnc2
  end

  def kiss_to_tnc2(_), do: %{error_code: :packet_invalid, error_message: "Unknown error"}

  @spec tnc2_to_kiss(binary()) :: binary()
  def tnc2_to_kiss(tnc2) do
    escaped =
      tnc2
      |> String.replace(<<0xDB>>, <<0xDB, 0xDD>>)
      |> String.replace(<<0xC0>>, <<0xDB, 0xDC>>)

    <<0xC0, 0x00>> <> escaped <> <<0xC0>>
  end
end
