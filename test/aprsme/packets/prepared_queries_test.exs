defmodule Aprsme.Packets.PreparedQueriesTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.Packet
  alias Aprsme.Packets.PreparedQueries
  alias Aprsme.Repo

  defp create_positioned_packet(attrs) do
    defaults = %{
      sender: "TEST-1",
      base_callsign: "TEST",
      ssid: "1",
      destination: "APRS",
      data_type: "position",
      lat: Decimal.new("33.0000"),
      lon: Decimal.new("-96.0000"),
      received_at: DateTime.truncate(DateTime.utc_now(), :second)
    }

    merged = Map.merge(defaults, attrs)

    %Packet{}
    |> Packet.changeset(Map.from_struct(Map.merge(%Packet{}, merged)))
    |> Repo.insert()
  end

  describe "get_latest_packets_for_callsigns/1" do
    test "returns full packet structs for multiple callsigns" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          comment: "Digi TX"
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "N5TXZ-10",
          base_callsign: "N5TXZ",
          ssid: "10",
          lat: Decimal.new("33.2000"),
          lon: Decimal.new("-96.5000"),
          comment: "iGate"
        })

      result = PreparedQueries.get_latest_packets_for_callsigns(["K5GVL-10", "N5TXZ-10"])

      assert length(result) == 2
      assert Enum.all?(result, &is_struct(&1, Packet))

      senders = Enum.map(result, & &1.sender)
      assert "K5GVL-10" in senders
      assert "N5TXZ-10" in senders

      k5gvl = Enum.find(result, &(&1.sender == "K5GVL-10"))
      assert_in_delta k5gvl.lat, 33.1, 0.01
      assert_in_delta k5gvl.lon, -96.6, 0.01
    end

    test "returns empty list for empty input" do
      assert PreparedQueries.get_latest_packets_for_callsigns([]) == []
    end

    test "returns empty list for nonexistent callsigns" do
      result = PreparedQueries.get_latest_packets_for_callsigns(["NONEXIST-1", "FAKE-2"])
      assert result == []
    end

    test "returns latest packet when multiple exist for a callsign" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second)
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("34.0000"),
          lon: Decimal.new("-97.0000"),
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      result = PreparedQueries.get_latest_packets_for_callsigns(["K5GVL-10"])

      assert length(result) == 1
      packet = hd(result)
      assert is_struct(packet, Packet)
      assert_in_delta packet.lat, 34.0, 0.01
      assert_in_delta packet.lon, -97.0, 0.01
    end
  end

  describe "get_latest_positions_for_callsigns/1" do
    test "returns positions for multiple callsigns" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000")
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "N5TXZ-10",
          base_callsign: "N5TXZ",
          ssid: "10",
          lat: Decimal.new("33.2000"),
          lon: Decimal.new("-96.5000")
        })

      result = PreparedQueries.get_latest_positions_for_callsigns(["K5GVL-10", "N5TXZ-10"])

      assert length(result) == 2
      callsigns = Enum.map(result, & &1.callsign)
      assert "K5GVL-10" in callsigns
      assert "N5TXZ-10" in callsigns

      k5gvl = Enum.find(result, &(&1.callsign == "K5GVL-10"))
      assert_in_delta k5gvl.lat, 33.1, 0.01
      assert_in_delta k5gvl.lng, -96.6, 0.01
    end

    test "returns empty list for empty input" do
      assert PreparedQueries.get_latest_positions_for_callsigns([]) == []
    end

    test "returns empty list for nonexistent callsigns" do
      result = PreparedQueries.get_latest_positions_for_callsigns(["NONEXIST-1", "FAKE-2"])
      assert result == []
    end

    test "is case-insensitive" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000")
        })

      result = PreparedQueries.get_latest_positions_for_callsigns(["k5gvl-10"])

      assert length(result) == 1
      assert hd(result).callsign == "K5GVL-10"
    end

    test "returns latest position when multiple packets exist for a callsign" do
      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("33.1000"),
          lon: Decimal.new("-96.6000"),
          received_at: DateTime.add(DateTime.utc_now(), -3600, :second)
        })

      {:ok, _} =
        create_positioned_packet(%{
          sender: "K5GVL-10",
          base_callsign: "K5GVL",
          ssid: "10",
          lat: Decimal.new("34.0000"),
          lon: Decimal.new("-97.0000"),
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      result = PreparedQueries.get_latest_positions_for_callsigns(["K5GVL-10"])

      assert length(result) == 1
      position = hd(result)
      assert_in_delta position.lat, 34.0, 0.01
      assert_in_delta position.lng, -97.0, 0.01
    end
  end
end
