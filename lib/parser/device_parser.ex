defmodule Parser.DeviceParser do
  @moduledoc """
  Extracts device identifier (TOCALL or Mic-E) from APRS packets.
  """

  @doc """
  Extract the device identifier from a packet map or raw packet string.
  """
  def extract_device_identifier(%{destination: dest}) when is_binary(dest) do
    # TOCALL is usually the first 6 chars of destination
    String.slice(dest, 0, 6)
  end

  def extract_device_identifier(%{data_type: :mic_e, destination: dest}) when is_binary(dest) do
    # Mic-E uses destination for device ID
    String.slice(dest, 0, 6)
  end

  def extract_device_identifier(packet) when is_binary(packet) do
    # Try to parse out the destination field from raw packet
    case Regex.run(~r/^[^>]+>([^,]+),/, packet) do
      [_, dest] -> String.slice(dest, 0, 6)
      _ -> nil
    end
  end

  def extract_device_identifier(_), do: nil
end
