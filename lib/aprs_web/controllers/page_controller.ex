defmodule AprsWeb.PageController do
  use AprsWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end

  def map(conn, _params) do
    # This serves the legacy JavaScript-based map at /old
    render(conn, :map)
  end

  def packets(conn, _params) do
    render(conn, :packets)
  end

  def health(conn, _params) do
    # Check database connection
    db_healthy =
      try do
        Aprs.Repo.query!("SELECT 1")
        true
      rescue
        _ -> false
      end

    # Get application version
    version = :aprs |> Application.spec(:vsn) |> List.to_string()

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

  def ready(conn, _params) do
    # Simple readiness check without database dependency
    # This is faster for startup health checks
    version = :aprs |> Application.spec(:vsn) |> List.to_string()

    json(conn, %{
      status: "ready",
      version: version,
      timestamp: DateTime.utc_now()
    })
  end

  def status_json(conn, _params) do
    # Get APRS-IS connection status
    aprs_status = Aprs.Is.get_status()

    # Get application version
    version = :aprs |> Application.spec(:vsn) |> List.to_string()

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
