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
end
