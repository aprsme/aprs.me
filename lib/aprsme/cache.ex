defmodule Aprsme.Cache do
  @moduledoc """
  Cache abstraction layer that works with both Cachex and RedisCache.
  Provides a unified API regardless of the underlying implementation.
  """

  @doc """
  Get a value from cache
  """
  def get(cache_name, key) do
    Cachex.get(cache_name, key)
  end

  @doc """
  Put a value in cache with optional TTL
  """
  def put(cache_name, key, value, opts \\ []) do
    Cachex.put(cache_name, key, value, opts)
  end

  @doc """
  Delete a key from cache
  """
  def del(cache_name, key) do
    Cachex.del(cache_name, key)
  end

  @doc """
  Clear all keys from cache
  """
  def clear(cache_name) do
    Cachex.clear(cache_name)
  end

  @doc """
  Get cache statistics
  """
  def stats(cache_name) do
    Cachex.stats(cache_name)
  end

  @doc """
  Check if key exists
  """
  def exists?(cache_name, key) do
    case Cachex.exists?(cache_name, key) do
      {:ok, exists?} -> exists?
      {:error, _reason} -> false
    end
  end

  @doc """
  Get TTL for a key
  """
  def ttl(cache_name, key) do
    Cachex.ttl(cache_name, key)
  end

  # Helper functions - no longer needed as we only use Cachex

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
