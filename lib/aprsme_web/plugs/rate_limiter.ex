defmodule AprsmeWeb.Plugs.RateLimiter do
  @moduledoc """
  Rate limiting plug to prevent DoS attacks
  """
  import Phoenix.Controller
  import Plug.Conn

  @type key_type :: :ip | :user_agent | (Plug.Conn.t() -> String.t()) | String.t()
  @type init_opts :: [
          scale: integer(),
          limit: integer(),
          key: key_type(),
          error_message: String.t()
        ]

  @spec init(Keyword.t()) :: init_opts()
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

  @spec call(Plug.Conn.t(), init_opts()) :: Plug.Conn.t()
  def call(conn, opts) do
    key = get_key(conn, opts[:key])
    scale = opts[:scale]
    limit = opts[:limit]
    error_message = opts[:error_message]

    case Aprsme.RateLimiterWrapper.hit("rate_limit:#{key}", scale, limit) do
      {:allow, _count} ->
        conn

      {:deny, _retry_after} ->
        conn
        |> put_status(:too_many_requests)
        |> json(%{error: error_message})
        |> halt()
    end
  end

  @spec get_key(Plug.Conn.t(), key_type()) :: String.t()
  defp get_key(conn, :ip) do
    # Check headers in order of preference
    case {get_req_header(conn, "cf-connecting-ip"), get_req_header(conn, "x-forwarded-for"),
          get_req_header(conn, "x-real-ip")} do
      # Cloudflare header takes precedence
      {[cf | _], _, _} ->
        cf

      # Then standard X-Forwarded-For header
      {[], [forwarded | _], _} ->
        forwarded |> String.split(",") |> List.first() |> String.trim()

      # Then X-Real-IP header
      {[], [], [real | _]} ->
        real

      # Fall back to remote_ip
      {[], [], []} ->
        conn.remote_ip |> :inet.ntoa() |> to_string()
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
