defmodule AprsmeWeb.PageController do
  @moduledoc false
  use AprsmeWeb, :controller

  alias Aprsme.Cluster.LeaderElection

  def home(conn, _params) do
    render(conn, :home)
  end

  def packets(conn, _params) do
    render(conn, :packets)
  end

  def health(conn, _params) do
    # Use our health check plug logic
    health_status = Application.get_env(:aprsme, :health_status, :healthy)
    version = :aprsme |> Application.spec(:vsn) |> List.to_string()

    cond do
      health_status == :draining ->
        conn
        |> put_status(503)
        |> json(%{
          status: "draining",
          version: version,
          message: "Application is draining connections",
          timestamp: DateTime.utc_now()
        })

      Aprsme.ShutdownHandler.shutting_down?() ->
        conn
        |> put_status(503)
        |> json(%{
          status: "shutting_down",
          version: version,
          message: "Application is shutting down",
          timestamp: DateTime.utc_now()
        })

      true ->
        # Normal health check
        db_healthy =
          try do
            Aprsme.Repo.query!("SELECT 1")
            true
          rescue
            _ -> false
          end

        if db_healthy do
          json(conn, %{
            status: "ok",
            version: version,
            database: "connected",
            timestamp: DateTime.utc_now()
          })
        else
          conn
          |> put_status(503)
          |> json(%{
            status: "error",
            version: version,
            database: "disconnected",
            timestamp: DateTime.utc_now()
          })
        end
    end
  end

  def ready(conn, _params) do
    # Simple readiness check without database dependency
    # This is faster for startup health checks
    version = :aprsme |> Application.spec(:vsn) |> List.to_string()

    json(conn, %{
      status: "ready",
      version: version,
      timestamp: DateTime.utc_now()
    })
  end

  def status_json(conn, _params) do
    # Get cluster-wide APRS-IS connection status
    aprs_status = LeaderElection.get_cluster_aprs_status()

    # Get application version
    version = :aprsme |> Application.spec(:vsn) |> List.to_string()

    # Calculate uptime in a human-readable format
    uptime_display = format_uptime(aprs_status.uptime_seconds)

    json(conn, %{
      aprs_is: %{
        connected: aprs_status.connected,
        server: aprs_status.server,
        port: aprs_status.port,
        connected_at: aprs_status.connected_at,
        uptime_seconds: aprs_status.uptime_seconds,
        uptime_display: uptime_display,
        login_id: aprs_status.login_id,
        filter: aprs_status.filter
      },
      application: %{
        version: version,
        timestamp: DateTime.utc_now()
      }
    })
  end

  defp format_uptime(seconds) when seconds <= 0, do: "Not connected"

  defp format_uptime(seconds) do
    days = div(seconds, 86_400)
    hours = div(rem(seconds, 86_400), 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    cond do
      days > 0 -> "#{days}d #{hours}h #{minutes}m #{secs}s"
      hours > 0 -> "#{hours}h #{minutes}m #{secs}s"
      minutes > 0 -> "#{minutes}m #{secs}s"
      true -> "#{secs}s"
    end
  end
end
