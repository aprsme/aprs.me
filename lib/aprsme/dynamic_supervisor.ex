defmodule Aprsme.DynamicSupervisor do
  @moduledoc """
  Dynamic supervisor for managing processes that may be started/stopped at runtime.
  Used primarily for the APRS-IS connection which is managed by cluster leadership.
  """
  use DynamicSupervisor

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
