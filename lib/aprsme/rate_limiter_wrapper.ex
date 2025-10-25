defmodule Aprsme.RateLimiterWrapper do
  @moduledoc """
  Wrapper that provides the same API using ETS-based rate limiter
  """

  @doc """
  Check rate limit - compatible with Hammer.hit/3 API
  """
  def hit(bucket, scale_ms, limit) do
    Aprsme.RateLimiter.hit(bucket, scale_ms, limit)
  end

  @doc """
  Reset rate limit for a bucket
  """
  def reset(_bucket) do
    # ETS-based rate limiter doesn't provide a reset function
    # Return ok
    :ok
  end

  @doc """
  Get current count for a bucket
  """
  def count(_bucket, _scale_ms) do
    # ETS-based rate limiter doesn't provide a count function
    # Return 0 as default
    {:ok, 0}
  end
end
