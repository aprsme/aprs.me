defmodule AprsmeWeb.Plugs.ContentSecurityPolicy do
  @moduledoc """
  Sets Content Security Policy headers with Sentry integration support.
  """
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    put_resp_header(conn, "content-security-policy", csp_policy())
  end

  defp csp_policy do
    Enum.join(
      [
        "default-src 'self'",
        "script-src 'self' 'unsafe-inline' 'unsafe-eval' https://js.sentry-cdn.com https://unpkg.com https://cdn.jsdelivr.net",
        "style-src 'self' 'unsafe-inline' https://unpkg.com",
        "img-src 'self' data: https: blob:",
        "font-src 'self' data:",
        "connect-src 'self' wss://#{host()} https://*.ingest.sentry.io https://*.sentry.io https://nominatim.openstreetmap.org https://tile.openstreetmap.org https://*.tile.openstreetmap.org",
        "media-src 'self'",
        "object-src 'none'",
        "frame-ancestors 'none'",
        "base-uri 'self'",
        "form-action 'self'",
        "frame-src 'self'",
        "manifest-src 'self'",
        "worker-src 'self' blob:"
      ],
      "; "
    )
  end

  defp host do
    Application.get_env(:aprsme, AprsmeWeb.Endpoint)[:url][:host] || "localhost"
  end
end
