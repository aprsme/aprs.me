defmodule Aprsme.Cluster.Topology do
  @moduledoc """
  Wrapper for Cluster.Supervisor that only starts when clustering is enabled.
  """
  require Logger

  def child_spec(opts) do
    if Application.get_env(:aprsme, :cluster_enabled, false) do
      topologies = Application.get_env(:libcluster, :topologies, [])

      # Log the configuration for debugging
      Logger.info("Cluster.Topology starting with topologies: #{inspect(topologies)}")

      # Ensure we have valid topologies
      if topologies == [] or topologies == nil do
        Logger.warning("No libcluster topologies configured, clustering will not work")
        # Return a no-op spec
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :worker
        }
      else
        supervisor_opts = Keyword.merge([name: Aprsme.ClusterSupervisor], opts)
        {Cluster.Supervisor, [topologies, supervisor_opts]}
      end
    else
      # Return a no-op spec when clustering is disabled
      %{
        id: __MODULE__,
        start: {__MODULE__, :start_link, [opts]},
        type: :worker
      }
    end
  end

  def start_link(_opts), do: :ignore
end
