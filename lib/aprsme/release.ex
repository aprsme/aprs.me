defmodule Aprsme.Release do
  @moduledoc """
  Used for executing DB release tasks when run in production without Mix
  installed.
  """
  @app :aprsme

  def migrate do
    require Logger
    # Initialize deployment timestamp first
    deployed_at = init()
    Logger.info("Deployment timestamp: #{deployed_at}")

    # Gettext translations are automatically compiled during Mix compilation

    # Run migrations
    {:ok, _, _} = Ecto.Migrator.with_repo(Aprsme.Repo, &Ecto.Migrator.run(&1, :up, all: true))
  end

  def rollback(repo, version) do
    load_app()
    {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
  end

  defp load_app do
    Application.load(@app)
  end

  @doc """
  Initialize release-specific configuration.
  This is called during application startup.
  """
  def init do
    # Read deployment timestamp from file
    deployed_at = read_deployment_timestamp()

    # Add to application config
    Application.put_env(:aprsme, :deployed_at, deployed_at)

    deployed_at
  end

  @doc """
  Get the deployment timestamp.
  """
  def deployed_at do
    Application.get_env(:aprsme, :deployed_at) || DateTime.utc_now()
  end

  defp read_deployment_timestamp do
    case File.read("/app/deployed_at.txt") do
      {:ok, timestamp} ->
        case DateTime.from_iso8601(String.trim(timestamp)) do
          {:ok, datetime, _} -> datetime
          _ -> DateTime.utc_now()
        end

      _ ->
        # Fallback to current time if file doesn't exist
        DateTime.utc_now()
    end
  end
end
