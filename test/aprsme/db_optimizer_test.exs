defmodule Aprsme.DbOptimizerTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.DbOptimizer

  describe "calculate_optimal_batch_size/1" do
    test "returns max cap (2000) for small maps" do
      entries = [%{name: "a", value: 1}, %{name: "b", value: 2}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "uses default 1024 size when first entry is nil" do
      # With nil as first entry, estimate_entry_size returns 1024
      # available_memory = 14 * 1024 * 1024 = 14_680_064
      # max_batch = div(14_680_064, 1024) = 14_336
      # capped at min(14_336, 2000) = 2000
      entries = [nil, %{name: "a"}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "returns 2000 for single-element list with small map" do
      entries = [%{x: 1}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "returns minimum of 100 for extremely large entries" do
      # Create a map with very large values to push the estimated size way up
      # We need estimated_size > 14 * 1024 * 1024 / 100 = 146_800
      # Each binary value contributes its byte_size + 100 overhead
      big_value = String.duplicate("x", 200_000)
      entries = [%{data: big_value}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 100
    end
  end

  describe "copy_insert/3" do
    test "inserts rows when count is <= 1000 using regular_batch_insert" do
      now = NaiveDateTime.truncate(NaiveDateTime.utc_now(), :second)

      rows = [
        ["test_copy_small_1", 10, now, now],
        ["test_copy_small_2", 20, now, now]
      ]

      count =
        DbOptimizer.copy_insert(
          "packet_counters",
          ["counter_type", "count", "inserted_at", "updated_at"],
          rows
        )

      assert count == 2
    end

    test "falls back to regular_batch_insert when > 1000 rows (COPY fails in sandbox)" do
      now = NaiveDateTime.truncate(NaiveDateTime.utc_now(), :second)

      rows =
        for i <- 1..1001 do
          ["test_copy_large_#{i}", i, now, now]
        end

      count =
        DbOptimizer.copy_insert(
          "packet_counters",
          ["counter_type", "count", "inserted_at", "updated_at"],
          rows
        )

      # COPY will fail in sandbox, falls back to regular_batch_insert
      # Some rows may conflict on unique index but on_conflict: :nothing handles that
      assert is_integer(count)
      assert count > 0
    end
  end

  describe "optimized_batch_insert/3" do
    test "inserts valid entries and returns {count, 0}" do
      now = NaiveDateTime.truncate(NaiveDateTime.utc_now(), :second)

      entries = [
        %{
          "counter_type" => "test_optimized_1",
          "count" => 100,
          "inserted_at" => now,
          "updated_at" => now
        },
        %{
          "counter_type" => "test_optimized_2",
          "count" => 200,
          "inserted_at" => now,
          "updated_at" => now
        }
      ]

      assert {2, 0} = DbOptimizer.optimized_batch_insert("packet_counters", entries)
    end

    test "returns {0, 0} for empty list" do
      assert {0, 0} = DbOptimizer.optimized_batch_insert("packet_counters", [])
    end
  end

  describe "analyze_table/1" do
    test "returns :ok for packets table" do
      assert :ok = DbOptimizer.analyze_table("packets")
    end
  end

  describe "vacuum_table/1" do
    test "returns :error inside sandbox (VACUUM cannot run in transactions)" do
      assert :error = DbOptimizer.vacuum_table("packets")
    end
  end

  describe "get_connection_stats/0" do
    test "returns map with expected keys and integer values" do
      stats = DbOptimizer.get_connection_stats()

      assert is_map(stats)
      assert Map.has_key?(stats, :total)
      assert Map.has_key?(stats, :active)
      assert Map.has_key?(stats, :idle)
      assert Map.has_key?(stats, :idle_in_transaction)
      assert Map.has_key?(stats, :waiting)

      assert is_integer(stats.total)
      assert is_integer(stats.active)
      assert is_integer(stats.idle)
      assert is_integer(stats.idle_in_transaction)
      assert is_integer(stats.waiting)
    end
  end

  describe "calculate_optimal_batch_size/1 with various value types" do
    test "handles maps with float values" do
      entries = [%{temp: 98.6, pressure: 1013.25}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "handles maps with boolean values" do
      entries = [%{active: true, verified: false}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "handles maps with DateTime values" do
      entries = [%{created_at: DateTime.utc_now(), name: "test"}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "handles maps with Date values" do
      entries = [%{date: Date.utc_today(), label: "today"}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "handles maps with nil values" do
      entries = [%{name: nil, value: nil}]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "handles maps with mixed value types" do
      entries = [
        %{
          name: "test",
          count: 42,
          ratio: 3.14,
          active: true,
          created: DateTime.utc_now(),
          date: Date.utc_today(),
          extra: nil
        }
      ]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end

    test "handles non-map first entry as unknown type" do
      entries = ["just a string", "another"]

      assert DbOptimizer.calculate_optimal_batch_size(entries) == 2000
    end
  end

  describe "optimized_batch_insert/3 with invalid data" do
    test "handles insert errors and returns error count" do
      # Insert entries into a nonexistent table - should error
      entries = [%{"bad_col" => "value"}]

      {success, errors} = DbOptimizer.optimized_batch_insert("nonexistent_table_xyz", entries)

      assert success == 0
      assert errors == 1
    end
  end

  describe "vacuum_table/1 with options" do
    test "with full: true returns :error in sandbox" do
      assert :error = DbOptimizer.vacuum_table("packets", full: true)
    end

    test "with analyze: false returns :error in sandbox" do
      assert :error = DbOptimizer.vacuum_table("packets", analyze: false)
    end
  end
end
