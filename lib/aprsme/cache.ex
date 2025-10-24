defmodule Aprsme.Cache do
  @moduledoc """
  Cache abstraction layer using ETS (Erlang Term Storage).
  Provides a unified API for in-memory caching.
  """

  @doc """
  Get a value from cache
  """
  def get(cache_name, key) do
    case :ets.lookup(cache_name, key) do
      [{^key, value}] -> {:ok, value}
      [] -> {:ok, nil}
    end
  rescue
    ArgumentError -> {:error, :no_cache}
  end

  @doc """
  Put a value in cache with optional TTL (TTL not implemented for ETS)
  """
  def put(cache_name, key, value, _opts \\ []) do
    try do
      :ets.insert(cache_name, {key, value})
      {:ok, true}
    rescue
      ArgumentError -> {:error, :no_cache}
    end
  end

  @doc """
  Delete a key from cache
  """
  def del(cache_name, key) do
    try do
      :ets.delete(cache_name, key)
      {:ok, true}
    rescue
      ArgumentError -> {:error, :no_cache}
    end
  end

  @doc """
  Clear all keys from cache
  """
  def clear(cache_name) do
    try do
      :ets.delete_all_objects(cache_name)
      {:ok, true}
    rescue
      ArgumentError -> {:error, :no_cache}
    end
  end

  @doc """
  Get cache statistics (simplified for ETS)
  """
  def stats(cache_name) do
    try do
      info = :ets.info(cache_name)
      {:ok, %{size: Keyword.get(info, :size, 0)}}
    rescue
      ArgumentError -> {:error, :no_cache}
    end
  end

  @doc """
  Check if key exists
  """
  def exists?(cache_name, key) do
    try do
      :ets.member(cache_name, key)
    rescue
      ArgumentError -> false
    end
  end

  @doc """
  Get TTL for a key (not supported in ETS, always returns nil)
  """
  def ttl(_cache_name, _key) do
    {:ok, nil}
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
