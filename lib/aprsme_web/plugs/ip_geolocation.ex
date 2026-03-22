defmodule AprsmeWeb.Plugs.IPGeolocation do
  @moduledoc """
  Plug that extracts geolocation from Cloudflare's CF-IPLatitude and CF-IPLongitude
  headers on initial page load. Results are stored in the session to avoid repeated parsing.
  """
  @behaviour Plug

  import Plug.Conn

  @impl true
  def init(opts), do: opts

  @impl true
  def call(%{method: "GET"} = conn, _opts) do
    conn
    |> get_session(:ip_geolocation)
    |> handle_geolocation(conn)
  end

  def call(conn, _opts), do: conn

  defp handle_geolocation(nil, conn), do: extract_cf_headers(conn)
  defp handle_geolocation(_cached, conn), do: conn

  defp extract_cf_headers(conn) do
    with [lat_str] <- get_req_header(conn, "cf-iplatitude"),
         [lng_str] <- get_req_header(conn, "cf-iplongitude"),
         {lat, ""} <- Float.parse(lat_str),
         {lng, ""} <- Float.parse(lng_str),
         true <- lat >= -90 and lat <= 90 and lng >= -180 and lng <= 180 do
      put_session(conn, :ip_geolocation, %{"lat" => lat, "lng" => lng})
    else
      _ -> conn
    end
  end
end
