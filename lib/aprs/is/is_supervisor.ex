defmodule Aprs.Is.IsSupervisor do
  @moduledoc false
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  @impl true
  def init(:ok) do
    children = [
      {Aprs.Is, []}
    ]

    # Supervisor will restart children max 3 times in 5 seconds
    # This prevents rapid reconnection attempts
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 3, max_seconds: 5)
  end
end
