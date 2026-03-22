defmodule AprsmeWeb.Plugs.IPGeolocation do
  @moduledoc """
  Plug that extracts geolocation from Cloudflare's CF-IPLatitude and CF-IPLongitude
  headers on initial page load. Results are stored in the session to avoid repeated parsing.
  """
  @behaviour Plug

  import Plug.Conn

  require Logger

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
    lat_header = get_req_header(conn, "cf-iplatitude")
    lng_header = get_req_header(conn, "cf-iplongitude")

    Logger.info(
      "Cloudflare geolocation headers: cf-iplatitude=#{inspect(lat_header)}, cf-iplongitude=#{inspect(lng_header)}"
    )

    with [lat_str] <- lat_header,
         [lng_str] <- lng_header,
         {lat, ""} <- Float.parse(lat_str),
         {lng, ""} <- Float.parse(lng_str),
         true <- lat >= -90 and lat <= 90 and lng >= -180 and lng <= 180 do
      Logger.info("IP geolocation resolved: lat=#{lat}, lng=#{lng}")
      put_session(conn, :ip_geolocation, %{"lat" => lat, "lng" => lng})
    else
      _ ->
        Logger.info("No valid Cloudflare geolocation headers found")
        conn
    end
  end
end
