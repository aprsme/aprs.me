defmodule Aprsme.DeploymentNotifier do
  @moduledoc """
  Monitors for deployment changes and notifies connected clients.
  In k8s, this detects when the DEPLOYED_AT environment variable changes.
  """
  use GenServer

  require Logger

  @check_interval 30_000

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    # Schedule first check
    Process.send_after(self(), :check_deployment, @check_interval)

    # Get initial deployment timestamp
    deployed_at = Aprsme.Release.deployed_at()

    {:ok, %{deployed_at: deployed_at}}
  end

  def handle_info(:check_deployment, state) do
    # Schedule next check
    Process.send_after(self(), :check_deployment, @check_interval)

    # Get current deployment timestamp
    current_deployed_at = Aprsme.Release.deployed_at()

    # Check if deployment timestamp changed (shouldn't happen in same process, but useful for monitoring)
    if current_deployed_at == state.deployed_at do
      {:noreply, state}

      # Broadcast the new deployment
    else
      Logger.info("Deployment timestamp changed from #{state.deployed_at} to #{current_deployed_at}")

      Phoenix.PubSub.broadcast(
        Aprsme.PubSub,
        "deployment_events",
        {:new_deployment, %{deployed_at: current_deployed_at}}
      )

      {:noreply, %{state | deployed_at: current_deployed_at}}
    end
  end

  @doc """
  Notify about a new deployment immediately.
  This can be called from the release module when deployment is detected.
  """
  def notify_deployment(deployed_at) do
    Phoenix.PubSub.broadcast(
      Aprsme.PubSub,
      "deployment_events",
      {:new_deployment, %{deployed_at: deployed_at}}
    )
  end
end
