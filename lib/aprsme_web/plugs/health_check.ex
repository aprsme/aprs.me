defmodule AprsmeWeb.Plugs.HealthCheck do
  @moduledoc """
  Health check plug that returns appropriate status based on application state.
  Used by Kubernetes liveness and readiness probes.
  """

  import Plug.Conn

  require Logger

  def init(opts), do: opts

  def call(%{request_path: "/health"} = conn, opts) do
    probe_type = opts[:probe_type] || :readiness

    case check_health(probe_type) do
      {:ok, message} ->
        conn
        |> put_resp_content_type("text/plain")
        |> send_resp(200, message)
        |> halt()

      {:error, message} ->
        conn
        |> put_resp_content_type("text/plain")
        |> send_resp(503, message)
        |> halt()
    end
  end

  def call(conn, _opts), do: conn

  defp check_health(:liveness) do
    # Liveness probe - only fails if the application is truly broken
    # Continue returning OK even during shutdown to prevent unnecessary restarts
    case basic_health_checks() do
      :ok -> {:ok, "OK"}
      {:error, reason} -> {:error, "Liveness check failed: #{reason}"}
    end
  end

  defp check_health(:readiness) do
    # Readiness probe - fails when shutting down to stop new traffic
    health_status = Application.get_env(:aprsme, :health_status, :healthy)

    cond do
      health_status == :draining ->
        {:error, "Application is draining connections"}

      shutting_down?() ->
        {:error, "Application is shutting down"}

      true ->
        case full_health_checks() do
          :ok -> {:ok, "OK"}
          {:error, reason} -> {:error, "Readiness check failed: #{reason}"}
        end
    end
  end

  # Basic checks for liveness
  # Check if the application is running
  defp basic_health_checks do
    _ = Application.get_env(:aprsme, :env)
    :ok
  rescue
    _ -> {:error, "Application not responding"}
  end

  defp full_health_checks do
    # Comprehensive checks for readiness
    with :ok <- check_database_connection(),
         :ok <- check_redis_connection() do
      check_pubsub()
    end
  end

  defp check_database_connection do
    case Ecto.Adapters.SQL.query(Aprsme.Repo, "SELECT 1", [], timeout: 1000) do
      {:ok, _} -> :ok
      _ -> {:error, "Database connection failed"}
    end
  rescue
    _ -> {:error, "Database check failed"}
  end

  defp check_redis_connection do
    if System.get_env("REDIS_URL") do
      try do
        case Redix.command(:query_cache_redis, ["PING"]) do
          {:ok, "PONG"} -> :ok
          _ -> {:error, "Redis connection failed"}
        end
      rescue
        _ -> {:error, "Redis check failed"}
      end
    else
      :ok
    end
  end

  defp check_pubsub do
    Phoenix.PubSub.broadcast(Aprsme.PubSub, "health_check", :ping)
    :ok
  rescue
    _ -> {:error, "PubSub check failed"}
  end

  defp shutting_down? do
    # Check if ShutdownHandler process exists and is shutting down
    case Process.whereis(Aprsme.ShutdownHandler) do
      nil ->
        # Process doesn't exist, not shutting down
        false

      pid when is_pid(pid) ->
        # Process exists, check if alive and call it
        if Process.alive?(pid) do
          try do
            GenServer.call(pid, :shutting_down?, 5000)
          catch
            :exit, _ -> false
            _, _ -> false
          end
        else
          false
        end
    end
  end
end
