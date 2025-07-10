defmodule AprsmeWeb.Plugs.IPGeolocation do
  @moduledoc """
  Plug that performs IP-based geolocation on initial page load.
  Results are stored in the session to avoid repeated API calls.
  """
  import Plug.Conn
  require Logger

  @behaviour Plug

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    # Only run for the main map page
    if conn.request_path == "/" && conn.method == "GET" do
      case get_session(conn, :ip_geolocation) do
        nil ->
          # No cached geolocation, fetch it
          perform_geolocation(conn)

        _cached ->
          # Already have geolocation in session
          conn
      end
    else
      conn
    end
  end

  defp perform_geolocation(conn) do
    ip = get_client_ip(conn)

    if valid_ip_for_geolocation?(ip) do
      case fetch_ip_location(ip) do
        {:ok, location} ->
          put_session(conn, :ip_geolocation, location)

        {:error, reason} ->
          Logger.debug("IP geolocation failed for #{ip}: #{inspect(reason)}")
          conn
      end
    else
      conn
    end
  end

  defp get_client_ip(conn) do
    # Check for forwarded IP first (when behind proxy/load balancer)
    forwarded_for = get_req_header(conn, "x-forwarded-for")

    case forwarded_for do
      [forwarded | _] ->
        # Take the first IP from the X-Forwarded-For header
        forwarded
        |> String.split(",")
        |> List.first()
        |> String.trim()

      [] ->
        # Fall back to remote_ip
        case conn.remote_ip do
          {a, b, c, d} -> "#{a}.#{b}.#{c}.#{d}"
          {a, b, c, d, e, f, g, h} -> "#{a}:#{b}:#{c}:#{d}:#{e}:#{f}:#{g}:#{h}"
          _ -> nil
        end
    end
  end

  defp valid_ip_for_geolocation?(ip) when is_binary(ip) do
    # Skip local/private IPs
    not (String.starts_with?(ip, "127.") or
           String.starts_with?(ip, "::1") or
           String.starts_with?(ip, "10.") or
           String.starts_with?(ip, "172.16.") or
           String.starts_with?(ip, "172.17.") or
           String.starts_with?(ip, "172.18.") or
           String.starts_with?(ip, "172.19.") or
           String.starts_with?(ip, "172.20.") or
           String.starts_with?(ip, "172.21.") or
           String.starts_with?(ip, "172.22.") or
           String.starts_with?(ip, "172.23.") or
           String.starts_with?(ip, "172.24.") or
           String.starts_with?(ip, "172.25.") or
           String.starts_with?(ip, "172.26.") or
           String.starts_with?(ip, "172.27.") or
           String.starts_with?(ip, "172.28.") or
           String.starts_with?(ip, "172.29.") or
           String.starts_with?(ip, "172.30.") or
           String.starts_with?(ip, "172.31.") or
           String.starts_with?(ip, "192.168."))
  end

  defp valid_ip_for_geolocation?(_), do: false

  defp fetch_ip_location(ip) do
    case Req.get("https://ip-api.com/json/#{ip}") do
      {:ok, %Req.Response{status: 200, body: body}} ->
        parse_ip_api_response(body)

      {:ok, _response} ->
        {:error, :invalid_response}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_ip_api_response(body) when is_map(body) do
    case body do
      %{"status" => "success", "lat" => lat, "lon" => lon}
      when is_number(lat) and is_number(lon) ->
        if lat >= -90 and lat <= 90 and lon >= -180 and lon <= 180 do
          {:ok, %{"lat" => lat / 1.0, "lng" => lon / 1.0}}
        else
          {:error, :invalid_coordinates}
        end

      _data ->
        {:error, :api_failure}
    end
  end

  defp parse_ip_api_response(body) when is_binary(body) do
    case Jason.decode(body) do
      {:ok, decoded} -> parse_ip_api_response(decoded)
      {:error, _} -> {:error, :json_decode_error}
    end
  end

  defp parse_ip_api_response(_), do: {:error, :invalid_response}
end