defmodule Aprsme.CacheTest do
  use ExUnit.Case, async: true

  alias Aprsme.Cache

  @table_name :cache_test_table

  setup do
    :ets.new(@table_name, [:set, :public, :named_table])

    on_exit(fn ->
      try do
        :ets.delete(@table_name)
      rescue
        ArgumentError -> :ok
      end
    end)

    :ok
  end

  describe "put/4 and get/2" do
    test "stores and retrieves a value" do
      assert {:ok, true} = Cache.put(@table_name, :key, "value")
      assert {:ok, "value"} = Cache.get(@table_name, :key)
    end

    test "returns nil for missing keys" do
      assert {:ok, nil} = Cache.get(@table_name, :missing)
    end

    test "overwrites existing values" do
      Cache.put(@table_name, :key, "first")
      Cache.put(@table_name, :key, "second")
      assert {:ok, "second"} = Cache.get(@table_name, :key)
    end
  end

  describe "TTL support" do
    test "entry with TTL is returned before expiry" do
      Cache.put(@table_name, :key, "value", ttl: 60_000)
      assert {:ok, "value"} = Cache.get(@table_name, :key)
    end

    test "entry with expired TTL returns nil" do
      # Use a TTL of 1ms and sleep briefly
      Cache.put(@table_name, :key, "value", ttl: 1)
      Process.sleep(5)
      assert {:ok, nil} = Cache.get(@table_name, :key)
    end

    test "entry without TTL never expires" do
      Cache.put(@table_name, :key, "value")
      assert {:ok, "value"} = Cache.get(@table_name, :key)
    end

    test "expired entry is lazily deleted from ETS" do
      Cache.put(@table_name, :key, "value", ttl: 1)
      Process.sleep(5)

      # First get triggers lazy deletion
      Cache.get(@table_name, :key)

      # Verify entry is gone from ETS
      assert [] = :ets.lookup(@table_name, :key)
    end
  end

  describe "exists?/2" do
    test "returns true for existing key" do
      Cache.put(@table_name, :key, "value")
      assert Cache.exists?(@table_name, :key)
    end

    test "returns false for missing key" do
      refute Cache.exists?(@table_name, :missing)
    end

    test "returns false for expired key" do
      Cache.put(@table_name, :key, "value", ttl: 1)
      Process.sleep(5)
      refute Cache.exists?(@table_name, :key)
    end
  end

  describe "ttl/2" do
    test "returns nil for entries without TTL" do
      Cache.put(@table_name, :key, "value")
      assert {:ok, nil} = Cache.ttl(@table_name, :key)
    end

    test "returns remaining time for entries with TTL" do
      Cache.put(@table_name, :key, "value", ttl: 60_000)
      assert {:ok, remaining} = Cache.ttl(@table_name, :key)
      assert is_integer(remaining)
      assert remaining > 0
      assert remaining <= 60_000
    end

    test "returns nil for missing keys" do
      assert {:ok, nil} = Cache.ttl(@table_name, :missing)
    end
  end

  describe "del/2" do
    test "deletes an existing key" do
      Cache.put(@table_name, :key, "value")
      assert {:ok, true} = Cache.del(@table_name, :key)
      assert {:ok, nil} = Cache.get(@table_name, :key)
    end
  end

  describe "clear/1" do
    test "removes all entries" do
      Cache.put(@table_name, :a, 1)
      Cache.put(@table_name, :b, 2)
      assert {:ok, true} = Cache.clear(@table_name)
      assert {:ok, nil} = Cache.get(@table_name, :a)
      assert {:ok, nil} = Cache.get(@table_name, :b)
    end
  end

  describe "to_timeout/1" do
    test "converts time units to milliseconds" do
      assert Cache.to_timeout(hour: 1) == 3_600_000
      assert Cache.to_timeout(minute: 30) == 1_800_000
      assert Cache.to_timeout(day: 1) == 86_400_000
      assert Cache.to_timeout(second: 5) == 5_000
      assert Cache.to_timeout(hours: 2, minutes: 30) == 9_000_000
    end
  end

  describe "error handling" do
    test "get returns error for non-existent table" do
      assert {:error, :no_cache} = Cache.get(:nonexistent_table, :key)
    end

    test "put returns error for non-existent table" do
      assert {:error, :no_cache} = Cache.put(:nonexistent_table, :key, "value")
    end
  end
end
