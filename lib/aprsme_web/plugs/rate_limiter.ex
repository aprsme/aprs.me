defmodule AprsmeWeb.Plugs.RateLimiter do
  @moduledoc """
  Rate limiting plug to prevent DoS attacks
  """
  import Phoenix.Controller
  import Plug.Conn

  def init(opts) do
    # Default options
    Keyword.merge(
      [
        # 1 minute
        scale: 60_000,
        # 100 requests per minute
        limit: 100,
        # Rate limit by IP address
        key: :ip,
        error_message: "Too many requests"
      ],
      opts
    )
  end

  def call(conn, opts) do
    key = get_key(conn, opts[:key])
    scale = opts[:scale]
    limit = opts[:limit]
    error_message = opts[:error_message]

    case Hammer.check_rate("rate_limit:#{key}", scale, limit) do
      {:allow, _count} ->
        conn

      {:deny, _count} ->
        conn
        |> put_status(:too_many_requests)
        |> json(%{error: error_message})
        |> halt()
    end
  end

  defp get_key(conn, :ip) do
    case get_req_header(conn, "x-forwarded-for") do
      [ip | _] -> ip
      [] -> conn.remote_ip |> :inet.ntoa() |> to_string()
    end
  end

  defp get_key(conn, :user_agent) do
    case get_req_header(conn, "user-agent") do
      [ua | _] -> ua
      [] -> "unknown"
    end
  end

  defp get_key(conn, custom_key) when is_function(custom_key) do
    custom_key.(conn)
  end

  defp get_key(_conn, key) when is_binary(key) do
    key
  end
end
