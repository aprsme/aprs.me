defmodule Aprsme.RateLimiter do
  @moduledoc """
  Rate limiter module using Hammer 7.0
  """
  use Hammer, backend: :ets
end
