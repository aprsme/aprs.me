defmodule Aprsme.Cluster.Topology do
  @moduledoc """
  Cluster topology configuration for different environments.
  """

  def child_spec(opts) do
    cluster_config = config()

    if cluster_config do
      {Cluster.Supervisor, [cluster_config, opts]}
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

  defp config do
    if Application.get_env(:aprsme, :cluster_enabled, false) do
      kubernetes_config()
    end
  end

  defp kubernetes_config do
    [
      kubernetes: [
        strategy: Cluster.Strategy.Kubernetes.DNS,
        config: [
          service: "aprs-headless",
          application_name: "aprs",
          namespace: "aprs",
          polling_interval: 10_000
        ]
      ]
    ]
  end
end
