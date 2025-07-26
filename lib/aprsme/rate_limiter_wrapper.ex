defmodule Aprsme.RateLimiterWrapper do
  @moduledoc """
  Wrapper that provides the same API for both Hammer and RedisRateLimiter
  """

  @doc """
  Check rate limit - compatible with Hammer.hit/3 API
  """
  def hit(bucket, scale_ms, limit) do
    if using_redis?() do
      Aprsme.RedisRateLimiter.check_rate(bucket, limit, scale_ms)
    else
      Aprsme.RateLimiter.hit(bucket, scale_ms, limit)
    end
  end

  @doc """
  Reset rate limit for a bucket
  """
  def reset(bucket) do
    if using_redis?() do
      Aprsme.RedisRateLimiter.reset(bucket)
    else
      # Hammer doesn't provide a reset function
      # Best we can do is return ok
      :ok
    end
  end

  @doc """
  Get current count for a bucket
  """
  def count(bucket, scale_ms) do
    if using_redis?() do
      Aprsme.RedisRateLimiter.count(bucket, scale_ms)
    else
      # Hammer doesn't provide a count function
      # Return 0 as default
      {:ok, 0}
    end
  end

  defp using_redis? do
    System.get_env("REDIS_URL") != nil
  end
end
