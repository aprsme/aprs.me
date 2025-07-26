defmodule Aprsme.Cache do
  @moduledoc """
  Cache abstraction layer that works with both Cachex and RedisCache.
  Provides a unified API regardless of the underlying implementation.
  """

  @doc """
  Get a value from cache
  """
  def get(cache_name, key) do
    if using_redis?() do
      Aprsme.RedisCache.get(cache_name, key)
    else
      Cachex.get(cache_name, key)
    end
  end

  @doc """
  Put a value in cache with optional TTL
  """
  def put(cache_name, key, value, opts \\ []) do
    if using_redis?() do
      # Convert TTL from milliseconds to seconds for Redis
      opts = convert_ttl_to_seconds(opts)
      Aprsme.RedisCache.put(cache_name, key, value, opts)
    else
      Cachex.put(cache_name, key, value, opts)
    end
  end

  @doc """
  Delete a key from cache
  """
  def del(cache_name, key) do
    if using_redis?() do
      Aprsme.RedisCache.del(cache_name, key)
    else
      Cachex.del(cache_name, key)
    end
  end

  @doc """
  Clear all keys from cache
  """
  def clear(cache_name) do
    if using_redis?() do
      Aprsme.RedisCache.clear(cache_name)
    else
      Cachex.clear(cache_name)
    end
  end

  @doc """
  Get cache statistics
  """
  def stats(cache_name) do
    if using_redis?() do
      Aprsme.RedisCache.stats(cache_name)
    else
      Cachex.stats(cache_name)
    end
  end

  @doc """
  Check if key exists
  """
  def exists?(cache_name, key) do
    if using_redis?() do
      Aprsme.RedisCache.exists?(cache_name, key)
    else
      Cachex.exists?(cache_name, key)
    end
  end

  @doc """
  Get TTL for a key
  """
  def ttl(cache_name, key) do
    if using_redis?() do
      Aprsme.RedisCache.ttl(cache_name, key)
    else
      Cachex.ttl(cache_name, key)
    end
  end

  # Helper functions

  defp using_redis? do
    System.get_env("REDIS_URL") != nil
  end

  defp convert_ttl_to_seconds(opts) do
    case Keyword.get(opts, :ttl) do
      nil ->
        opts

      ttl_ms when is_integer(ttl_ms) ->
        # Convert milliseconds to seconds
        Keyword.put(opts, :ttl, div(ttl_ms, 1000))

      _ ->
        opts
    end
  end

  @doc """
  Convert timeout keyword list to milliseconds
  """
  def to_timeout(opts) do
    # Cachex's to_timeout is private, so we implement our own
    Enum.reduce(opts, 0, fn
      {:second, n}, acc -> acc + n * 1000
      {:seconds, n}, acc -> acc + n * 1000
      {:minute, n}, acc -> acc + n * 60 * 1000
      {:minutes, n}, acc -> acc + n * 60 * 1000
      {:hour, n}, acc -> acc + n * 60 * 60 * 1000
      {:hours, n}, acc -> acc + n * 60 * 60 * 1000
      {:day, n}, acc -> acc + n * 24 * 60 * 60 * 1000
      {:days, n}, acc -> acc + n * 24 * 60 * 60 * 1000
      {:millisecond, n}, acc -> acc + n
      {:milliseconds, n}, acc -> acc + n
    end)
  end
end
