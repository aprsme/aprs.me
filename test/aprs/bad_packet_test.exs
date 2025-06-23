defmodule Aprs.BadPacketTest do
  use Aprs.DataCase, async: true

  alias Aprs.BadPacket
  alias Aprs.Repo

  describe "changeset/2" do
    test "valid changeset with all fields" do
      attrs = %{
        raw_packet: "N0CALL>APRS:>Test packet",
        error_message: "Invalid format",
        error_type: "parse_error",
        attempted_at: DateTime.utc_now()
      }

      changeset = BadPacket.changeset(%BadPacket{}, attrs)

      assert changeset.valid?
      assert get_change(changeset, :raw_packet) == attrs.raw_packet
      assert get_change(changeset, :error_message) == attrs.error_message
      assert get_change(changeset, :error_type) == attrs.error_type
      assert get_change(changeset, :attempted_at) == attrs.attempted_at
    end

    test "valid changeset with only required fields" do
      attrs = %{raw_packet: "N0CALL>APRS:>Test packet"}

      changeset = BadPacket.changeset(%BadPacket{}, attrs)

      assert changeset.valid?
      assert get_change(changeset, :raw_packet) == attrs.raw_packet
    end

    test "invalid changeset without raw_packet" do
      attrs = %{
        error_message: "Invalid format",
        error_type: "parse_error"
      }

      changeset = BadPacket.changeset(%BadPacket{}, attrs)

      refute changeset.valid?
      assert %{raw_packet: ["can't be blank"]} = errors_on(changeset)
    end

    test "invalid changeset with empty raw_packet" do
      attrs = %{raw_packet: ""}

      changeset = BadPacket.changeset(%BadPacket{}, attrs)

      refute changeset.valid?
      assert %{raw_packet: ["can't be blank"]} = errors_on(changeset)
    end

    test "invalid changeset with nil raw_packet" do
      attrs = %{raw_packet: nil}

      changeset = BadPacket.changeset(%BadPacket{}, attrs)

      refute changeset.valid?
      assert %{raw_packet: ["can't be blank"]} = errors_on(changeset)
    end
  end

  describe "recent/2" do
    setup do
      # Create test bad packets with different timestamps
      now = DateTime.utc_now()

      old_packet =
        insert_bad_packet(%{
          raw_packet: "OLD>APRS:>Old packet",
          attempted_at: DateTime.add(now, -3600, :second)
        })

      recent_packet =
        insert_bad_packet(%{
          raw_packet: "RECENT>APRS:>Recent packet",
          attempted_at: DateTime.add(now, -60, :second)
        })

      newest_packet =
        insert_bad_packet(%{
          raw_packet: "NEWEST>APRS:>Newest packet",
          attempted_at: now
        })

      %{
        old_packet: old_packet,
        recent_packet: recent_packet,
        newest_packet: newest_packet
      }
    end

    test "returns packets ordered by attempted_at descending", %{
      old_packet: old_packet,
      recent_packet: recent_packet,
      newest_packet: newest_packet
    } do
      result = Repo.all(BadPacket.recent())

      packet_ids = Enum.map(result, & &1.id)
      assert packet_ids == [newest_packet.id, recent_packet.id, old_packet.id]
    end

    test "limits results to specified number" do
      # Insert 5 more packets to test limiting
      for i <- 1..5 do
        insert_bad_packet(%{raw_packet: "TEST#{i}>APRS:>Test packet #{i}"})
      end

      result = BadPacket |> BadPacket.recent(3) |> Repo.all()

      assert length(result) == 3
    end

    test "works with custom query" do
      query = from(b in BadPacket, where: like(b.raw_packet, "%RECENT%"))
      result = query |> BadPacket.recent(10) |> Repo.all()

      assert length(result) == 1
      assert hd(result).raw_packet =~ "RECENT"
    end

    test "default limit is 100" do
      # Test that default limit is applied correctly
      query = BadPacket.recent()

      # Check the query structure contains limit
      assert %{limit: %{expr: 100}} = query
    end
  end

  describe "by_error_type/2" do
    setup do
      parse_error =
        insert_bad_packet(%{
          raw_packet: "PARSE>APRS:>Parse error packet",
          error_type: "parse_error"
        })

      format_error =
        insert_bad_packet(%{
          raw_packet: "FORMAT>APRS:>Format error packet",
          error_type: "format_error"
        })

      another_parse_error =
        insert_bad_packet(%{
          raw_packet: "PARSE2>APRS:>Another parse error",
          error_type: "parse_error"
        })

      %{
        parse_error: parse_error,
        format_error: format_error,
        another_parse_error: another_parse_error
      }
    end

    test "returns packets with specified error type", %{
      parse_error: parse_error,
      another_parse_error: another_parse_error
    } do
      result = "parse_error" |> BadPacket.by_error_type() |> Repo.all()

      packet_ids = result |> Enum.map(& &1.id) |> Enum.sort()
      expected_ids = Enum.sort([parse_error.id, another_parse_error.id])

      assert packet_ids == expected_ids
    end

    test "returns empty list for non-existent error type" do
      result = "non_existent_error" |> BadPacket.by_error_type() |> Repo.all()

      assert result == []
    end

    test "works with custom query" do
      query = from(b in BadPacket, where: like(b.raw_packet, "%FORMAT%"))
      result = query |> BadPacket.by_error_type("format_error") |> Repo.all()

      assert length(result) == 1
      assert hd(result).error_type == "format_error"
    end

    test "is case sensitive" do
      result = "Parse_Error" |> BadPacket.by_error_type() |> Repo.all()

      assert result == []
    end
  end

  describe "count_recent/1" do
    test "counts packets within specified hours" do
      now = DateTime.utc_now()

      # Insert packets at different times
      insert_bad_packet(%{
        raw_packet: "RECENT1>APRS:>Recent packet 1",
        # 30 minutes ago
        attempted_at: DateTime.add(now, -30 * 60, :second)
      })

      insert_bad_packet(%{
        raw_packet: "RECENT2>APRS:>Recent packet 2",
        # 2 hours ago
        attempted_at: DateTime.add(now, -2 * 3600, :second)
      })

      insert_bad_packet(%{
        raw_packet: "OLD>APRS:>Old packet",
        # 25 hours ago
        attempted_at: DateTime.add(now, -25 * 3600, :second)
      })

      count = 24 |> BadPacket.count_recent() |> Repo.one()

      assert count == 2
    end

    test "default is 24 hours" do
      now = DateTime.utc_now()

      insert_bad_packet(%{
        raw_packet: "RECENT>APRS:>Recent packet",
        # 12 hours ago
        attempted_at: DateTime.add(now, -12 * 3600, :second)
      })

      insert_bad_packet(%{
        raw_packet: "OLD>APRS:>Old packet",
        # 25 hours ago
        attempted_at: DateTime.add(now, -25 * 3600, :second)
      })

      count = Repo.one(BadPacket.count_recent())

      assert count == 1
    end

    test "returns 0 when no packets in time range" do
      now = DateTime.utc_now()

      insert_bad_packet(%{
        raw_packet: "OLD>APRS:>Old packet",
        # 25 hours ago
        attempted_at: DateTime.add(now, -25 * 3600, :second)
      })

      count = 1 |> BadPacket.count_recent() |> Repo.one()

      assert count == 0
    end

    test "handles edge case at exact boundary" do
      now = DateTime.utc_now()

      # Insert packet exactly 24 hours ago
      insert_bad_packet(%{
        raw_packet: "BOUNDARY>APRS:>Boundary packet",
        attempted_at: DateTime.add(now, -24 * 3600, :second)
      })

      count = 24 |> BadPacket.count_recent() |> Repo.one()

      # Should not include the packet exactly at the boundary
      assert count == 0
    end
  end

  # Helper function to insert bad packets
  defp insert_bad_packet(attrs) do
    default_attrs = %{
      raw_packet: "DEFAULT>APRS:>Default packet",
      error_message: "Test error",
      error_type: "test_error",
      attempted_at: DateTime.utc_now()
    }

    attrs = Map.merge(default_attrs, attrs)

    %BadPacket{}
    |> BadPacket.changeset(attrs)
    |> Repo.insert!()
  end
end
