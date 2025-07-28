defmodule Aprsme.RedisCache do
  @moduledoc """
  Redis-backed distributed cache implementation.
  Provides a similar API to Cachex but uses Redis for distributed caching.
  """

  require Logger

  # 5 minutes in seconds
  @default_ttl 300
  # 5 seconds timeout for Redis operations
  @redis_timeout 5000

  def child_spec(opts) do
    name = Keyword.fetch!(opts, :name)

    children = [
      {Redix, name: redis_name(name), host: redis_host(), port: redis_port(), password: redis_password()}
    ]

    %{
      id: {__MODULE__, name},
      type: :supervisor,
      start: {Supervisor, :start_link, [children, [strategy: :one_for_one, name: :"#{name}_redis_supervisor"]]}
    }
  end

  @doc """
  Get a value from the cache
  """
  def get(cache_name, key) do
    redis_key = make_redis_key(cache_name, key)

    case Redix.command(redis_name(cache_name), ["GET", redis_key], timeout: @redis_timeout) do
      {:ok, nil} ->
        {:ok, nil}

      {:ok, value} ->
        {:ok, deserialize(value)}

      {:error, reason} ->
        Logger.error("Redis GET error for #{redis_key}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Put a value in the cache with optional TTL
  """
  def put(cache_name, key, value, opts \\ []) do
    redis_key = make_redis_key(cache_name, key)
    ttl = Keyword.get(opts, :ttl, @default_ttl)
    serialized = serialize(value)

    case Redix.command(redis_name(cache_name), ["SETEX", redis_key, ttl, serialized], timeout: @redis_timeout) do
      {:ok, "OK"} ->
        {:ok, true}

      {:error, reason} ->
        Logger.error("Redis SETEX error for #{redis_key}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Delete a value from the cache
  """
  def del(cache_name, key) do
    redis_key = make_redis_key(cache_name, key)

    case Redix.command(redis_name(cache_name), ["DEL", redis_key], timeout: @redis_timeout) do
      {:ok, _} ->
        {:ok, true}

      {:error, reason} ->
        Logger.error("Redis DEL error for #{redis_key}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Delete multiple keys matching a pattern
  """
  def del_pattern(cache_name, pattern) do
    redis_pattern = make_redis_key(cache_name, pattern)

    # Use SCAN to find keys matching pattern
    case scan_keys(cache_name, redis_pattern) do
      {:ok, keys} when keys != [] ->
        case Redix.command(redis_name(cache_name), ["DEL" | keys], timeout: @redis_timeout) do
          {:ok, count} ->
            {:ok, count}

          {:error, reason} ->
            Logger.error("Redis DEL error for pattern #{redis_pattern}: #{inspect(reason)}")
            {:error, reason}
        end

      {:ok, []} ->
        {:ok, 0}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Clear all keys for a cache
  """
  def clear(cache_name) do
    pattern = make_redis_key(cache_name, "*")

    case scan_keys(cache_name, pattern) do
      {:ok, keys} when keys != [] ->
        case Redix.pipeline(redis_name(cache_name), Enum.map(keys, &["DEL", &1]), timeout: @redis_timeout) do
          {:ok, _results} ->
            {:ok, true}

          {:error, reason} ->
            Logger.error("Redis CLEAR error for #{cache_name}: #{inspect(reason)}")
            {:error, reason}
        end

      {:ok, []} ->
        {:ok, true}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Get cache statistics
  """
  def stats(cache_name) do
    pattern = make_redis_key(cache_name, "*")

    case scan_keys(cache_name, pattern) do
      {:ok, keys} ->
        {:ok,
         %{
           size: length(keys),
           keys: keys
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Check if a key exists
  """
  def exists?(cache_name, key) do
    redis_key = make_redis_key(cache_name, key)

    case Redix.command(redis_name(cache_name), ["EXISTS", redis_key], timeout: @redis_timeout) do
      {:ok, 1} -> true
      {:ok, 0} -> false
      {:error, _} -> false
    end
  end

  @doc """
  Get remaining TTL for a key
  """
  def ttl(cache_name, key) do
    redis_key = make_redis_key(cache_name, key)

    case Redix.command(redis_name(cache_name), ["TTL", redis_key], timeout: @redis_timeout) do
      # Key doesn't exist
      {:ok, -2} -> {:ok, nil}
      # Key exists but has no TTL
      {:ok, -1} -> {:ok, :infinity}
      {:ok, seconds} -> {:ok, seconds}
      {:error, reason} -> {:error, reason}
    end
  end

  # Private functions

  defp redis_name(cache_name), do: :"#{cache_name}_redis"

  defp make_redis_key(cache_name, key) when is_binary(key) do
    "aprsme:#{cache_name}:#{key}"
  end

  defp make_redis_key(cache_name, key) do
    "aprsme:#{cache_name}:#{:erlang.phash2(key)}"
  end

  defp serialize(value) do
    :erlang.term_to_binary(value)
  end

  defp deserialize(binary) when is_binary(binary) do
    :erlang.binary_to_term(binary)
  rescue
    _ -> nil
  end

  defp scan_keys(cache_name, pattern) do
    scan_keys(cache_name, pattern, "0", [])
  end

  defp scan_keys(cache_name, pattern, cursor, acc) do
    case Redix.command(redis_name(cache_name), ["SCAN", cursor, "MATCH", pattern, "COUNT", "100"],
           timeout: @redis_timeout
         ) do
      {:ok, [new_cursor, keys]} ->
        new_acc = acc ++ keys

        if new_cursor == "0" do
          {:ok, new_acc}
        else
          scan_keys(cache_name, pattern, new_cursor, new_acc)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp redis_host do
    redis_url = System.get_env("REDIS_URL", "redis://localhost:6379")
    uri = URI.parse(redis_url)
    uri.host || "localhost"
  end

  defp redis_port do
    redis_url = System.get_env("REDIS_URL", "redis://localhost:6379")
    uri = URI.parse(redis_url)
    uri.port || 6379
  end

  defp redis_password do
    redis_url = System.get_env("REDIS_URL", "redis://localhost:6379")
    uri = URI.parse(redis_url)

    case uri.userinfo do
      nil ->
        nil

      userinfo ->
        case String.split(userinfo, ":") do
          [_, password] -> password
          _ -> nil
        end
    end
  end
end
