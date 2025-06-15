defmodule AprsWeb.PageController do
  use AprsWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end

  def map(conn, _params) do
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
end
