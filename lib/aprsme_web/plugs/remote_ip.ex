defmodule AprsmeWeb.Plugs.RemoteIp do
  @moduledoc """
  Plug that extracts the real client IP from Cloudflare or proxy headers
  and sets `conn.remote_ip` so downstream logging shows the actual client address.

  Header priority:
  1. `CF-Connecting-IP` (Cloudflare)
  2. `X-Forwarded-For` (first IP in the chain)
  """
  @behaviour Plug

  import Plug.Conn

  require Logger

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    conn =
      case get_client_ip(conn) do
        {:ok, ip} -> %{conn | remote_ip: ip}
        :error -> conn
      end

    Logger.metadata(remote_ip: conn.remote_ip |> :inet.ntoa() |> to_string())
    conn
  end

  defp get_client_ip(conn) do
    with :error <- parse_cf_header(conn) do
      parse_forwarded_for(conn)
    end
  end

  defp parse_cf_header(conn) do
    case get_req_header(conn, "cf-connecting-ip") do
      [ip_str | _] -> parse_ip(ip_str)
      [] -> :error
    end
  end

  defp parse_forwarded_for(conn) do
    case get_req_header(conn, "x-forwarded-for") do
      [value | _] ->
        value
        |> String.split(",")
        |> List.first()
        |> parse_ip()

      [] ->
        :error
    end
  end

  defp parse_ip(ip_str) do
    case ip_str |> String.trim() |> String.to_charlist() |> :inet.parse_address() do
      {:ok, ip} -> {:ok, ip}
      {:error, _} -> :error
    end
  end
end
