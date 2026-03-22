defmodule Aprsme.DeviceCacheTest do
  use ExUnit.Case, async: false

  alias Aprsme.DeviceCache

  setup do
    pid = Process.whereis(DeviceCache)

    if pid do
      :erlang.unregister(DeviceCache)
    end

    on_exit(fn ->
      if pid && !Process.whereis(DeviceCache) do
        true = Process.register(pid, DeviceCache)
      end
    end)

    :ok
  end

  test "refresh_cache/0 returns :ok when the cache server is not running" do
    assert DeviceCache.refresh_cache() == :ok
  end

  test "lookup_device/1 returns nil on cache miss without crashing when server is not running" do
    assert DeviceCache.lookup_device("TEST-DEVICE") == nil
  end
end
