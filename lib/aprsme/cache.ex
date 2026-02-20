defmodule Aprsme.Cache do
  @moduledoc """
  Cache abstraction layer using ETS (Erlang Term Storage).
  Provides a unified API for in-memory caching with optional TTL support.

  Entries are stored as `{key, value, expires_at}` tuples where `expires_at`
  is a monotonic time in milliseconds, or `:infinity` for no expiration.
  Expired entries are lazily evicted on read.
  """

  @doc """
  Get a value from cache. Returns `{:ok, nil}` for expired entries.
  """
  def get(cache_name, key) do
    case :ets.lookup(cache_name, key) do
      [{^key, value, :infinity}] ->
        {:ok, value}

      [{^key, value, expires_at}] ->
        if System.monotonic_time(:millisecond) < expires_at do
          {:ok, value}
        else
          :ets.delete(cache_name, key)
          {:ok, nil}
        end

      # Support legacy {key, value} tuples during transition
      [{^key, value}] ->
        {:ok, value}

      [] ->
        {:ok, nil}
    end
  rescue
    ArgumentError -> {:error, :no_cache}
  end

  @doc """
  Put a value in cache with optional TTL.

  ## Options
    * `:ttl` - Time to live in milliseconds. Use `Cache.to_timeout/1` for convenience.
  """
  def put(cache_name, key, value, opts \\ []) do
    expires_at =
      case Keyword.get(opts, :ttl) do
        nil -> :infinity
        ttl when is_integer(ttl) and ttl > 0 -> System.monotonic_time(:millisecond) + ttl
        _ -> :infinity
      end

    :ets.insert(cache_name, {key, value, expires_at})
    {:ok, true}
  rescue
    ArgumentError -> {:error, :no_cache}
  end

  @doc """
  Delete a key from cache
  """
  def del(cache_name, key) do
    :ets.delete(cache_name, key)
    {:ok, true}
  rescue
    ArgumentError -> {:error, :no_cache}
  end

  @doc """
  Clear all keys from cache
  """
  def clear(cache_name) do
    :ets.delete_all_objects(cache_name)
    {:ok, true}
  rescue
    ArgumentError -> {:error, :no_cache}
  end

  @doc """
  Get cache statistics (simplified for ETS)
  """
  def stats(cache_name) do
    info = :ets.info(cache_name)
    {:ok, %{size: Keyword.get(info, :size, 0)}}
  rescue
    ArgumentError -> {:error, :no_cache}
  end

  @doc """
  Check if key exists and is not expired.
  """
  def exists?(cache_name, key) do
    case get(cache_name, key) do
      {:ok, nil} -> false
      {:ok, _} -> true
      _ -> false
    end
  end

  @doc """
  Get TTL for a key. Returns remaining milliseconds or nil if no TTL.
  """
  def ttl(cache_name, key) do
    case :ets.lookup(cache_name, key) do
      [{^key, _value, :infinity}] -> {:ok, nil}
      [{^key, _value, expires_at}] -> {:ok, max(0, expires_at - System.monotonic_time(:millisecond))}
      [{^key, _value}] -> {:ok, nil}
      [] -> {:ok, nil}
    end
  rescue
    ArgumentError -> {:ok, nil}
  end

  @doc """
  Convert timeout keyword list to milliseconds.
  """
  def to_timeout(opts) do
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
