defmodule Aprsme.PacketsFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Aprsme.Packets` context.
  """

  alias Aprsme.Callsign
  alias Aprsme.Packet
  alias Aprsme.Repo

  @doc """
  Generate a packet.
  """
  def packet_fixture(attrs \\ %{}) do
    base_attrs = %{
      sender: "TEST-1",
      base_callsign: "TEST",
      ssid: "1",
      destination: "APRS",
      received_at: DateTime.utc_now(),
      lat: Decimal.new("40.7128"),
      lon: Decimal.new("-74.0060"),
      has_position: true,
      raw_packet: "TEST-1>APRS:=4042.77N/07400.36W>Test packet",
      data_type: "position",
      path: "APRS",
      data_extended: %{
        symbol_table_id: "/",
        symbol_code: ">",
        comment: "Test packet"
      }
    }

    # Extract base_callsign and ssid from sender if provided
    final_attrs =
      case Map.get(attrs, :sender) do
        nil ->
          base_attrs

        sender ->
          {base, ssid} = Callsign.extract_parts(sender)

          Map.merge(base_attrs, %{base_callsign: base, ssid: ssid})
      end

    {:ok, packet} =
      attrs
      |> Enum.into(final_attrs)
      |> then(&Packet.changeset(%Packet{}, &1))
      |> Repo.insert()

    packet
  end
end
