defmodule Aprsme.RedisRateLimiter do
  @moduledoc """
  Redis-backed distributed rate limiter using sliding window algorithm.
  """

  require Logger

  # 5 seconds timeout for Redis operations
  @redis_timeout 5000

  def child_spec(_opts) do
    children = [
      {Redix, name: __MODULE__, host: redis_host(), port: redis_port(), password: redis_password()}
    ]

    %{
      id: __MODULE__,
      type: :supervisor,
      start: {Supervisor, :start_link, [children, [strategy: :one_for_one, name: :"#{__MODULE__}_supervisor"]]}
    }
  end

  @doc """
  Check rate limit using sliding window algorithm.

  ## Parameters
    - bucket: The rate limit bucket (e.g., "user:123" or "ip:192.168.1.1")
    - limit: Maximum number of requests allowed
    - window_ms: Time window in milliseconds
    
  ## Returns
    - {:allow, count} if under limit
    - {:deny, limit} if over limit
  """
  def check_rate(bucket, limit, window_ms) do
    now = System.system_time(:millisecond)
    window_start = now - window_ms
    key = make_key(bucket)

    # Lua script for atomic sliding window rate limiting
    lua_script = """
    local key = KEYS[1]
    local now = tonumber(ARGV[1])
    local window_start = tonumber(ARGV[2])
    local limit = tonumber(ARGV[3])

    -- Remove old entries
    redis.call('ZREMRANGEBYSCORE', key, 0, window_start)

    -- Count current entries
    local current = redis.call('ZCARD', key)

    if current < limit then
      -- Add new entry
      redis.call('ZADD', key, now, now)
      -- Set expiry
      redis.call('EXPIRE', key, math.ceil(ARGV[4] / 1000))
      return {1, current + 1}
    else
      return {0, limit}
    end
    """

    case Redix.command(__MODULE__, ["EVAL", lua_script, 1, key, now, window_start, limit, window_ms],
           timeout: @redis_timeout
         ) do
      {:ok, [1, count]} ->
        {:allow, count}

      {:ok, [0, limit]} ->
        {:deny, limit}

      {:error, reason} ->
        Logger.error("Redis rate limit error for #{bucket}: #{inspect(reason)}")
        # Fail open - allow request if Redis is down
        {:allow, 1}
    end
  end

  @doc """
  Reset rate limit for a bucket
  """
  def reset(bucket) do
    key = make_key(bucket)

    case Redix.command(__MODULE__, ["DEL", key], timeout: @redis_timeout) do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        Logger.error("Redis rate limit reset error for #{bucket}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Get current count for a bucket
  """
  def count(bucket, window_ms) do
    now = System.system_time(:millisecond)
    window_start = now - window_ms
    key = make_key(bucket)

    # Clean up old entries and count
    case Redix.pipeline(
           __MODULE__,
           [
             ["ZREMRANGEBYSCORE", key, 0, window_start],
             ["ZCARD", key]
           ],
           timeout: @redis_timeout
         ) do
      {:ok, [_, count]} ->
        {:ok, count}

      {:error, reason} ->
        Logger.error("Redis rate limit count error for #{bucket}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Get remaining requests for a bucket
  """
  def remaining(bucket, limit, window_ms) do
    case count(bucket, window_ms) do
      {:ok, current} -> {:ok, max(0, limit - current)}
      error -> error
    end
  end

  # Private functions

  defp make_key(bucket) do
    "aprsme:rate_limit:#{bucket}"
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
