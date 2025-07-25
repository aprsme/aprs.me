defmodule AprsmeWeb.Plugs.IPGeolocation do
  @moduledoc """
  Plug that performs IP-based geolocation on initial page load.
  Results are stored in the session to avoid repeated API calls.
  """
  @behaviour Plug

  import Plug.Conn

  require Logger

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
    Logger.info("IP geolocation: Starting lookup for IP #{ip}")

    if valid_ip_for_geolocation?(ip) do
      start_time = System.monotonic_time(:millisecond)

      # Run geolocation in a task with a hard timeout
      task = Task.async(fn -> fetch_ip_location(ip) end)

      case Task.yield(task, 600) || Task.shutdown(task, :brutal_kill) do
        {:ok, {:ok, location}} ->
          duration = System.monotonic_time(:millisecond) - start_time

          Logger.info(
            "IP geolocation: Successfully located #{ip} at #{location["lat"]}, #{location["lng"]} in #{duration}ms"
          )

          put_session(conn, :ip_geolocation, location)

        {:ok, {:error, reason}} ->
          duration = System.monotonic_time(:millisecond) - start_time
          Logger.warning("IP geolocation: Failed for #{ip} after #{duration}ms - #{inspect(reason)}")
          conn

        nil ->
          # Task timed out
          duration = System.monotonic_time(:millisecond) - start_time
          Logger.warning("IP geolocation: Task timeout for #{ip} after #{duration}ms")
          conn
      end
    else
      Logger.info("IP geolocation: Skipping private/local IP #{ip}")
      conn
    end
  end

  defp get_client_ip(conn) do
    # Check for Cloudflare header first (CF-Connecting-IP)
    cf_ip = get_req_header(conn, "cf-connecting-ip")

    # Then check for standard forwarded headers
    forwarded_for = get_req_header(conn, "x-forwarded-for")
    real_ip = get_req_header(conn, "x-real-ip")

    ip =
      case {cf_ip, forwarded_for, real_ip} do
        {[cf | _], _, _} ->
          # Cloudflare header takes precedence
          ip_from_cf = String.trim(cf)
          Logger.info("IP geolocation: Using Cloudflare CF-Connecting-IP: #{ip_from_cf}")
          ip_from_cf

        {[], [forwarded | _], _} ->
          # Take the first IP from the X-Forwarded-For header
          ip_from_header =
            forwarded
            |> String.split(",")
            |> List.first()
            |> String.trim()

          Logger.info("IP geolocation: Using forwarded IP from X-Forwarded-For header: #{ip_from_header}")
          ip_from_header

        {[], [], [real | _]} ->
          # Use X-Real-IP header
          ip_from_real = String.trim(real)
          Logger.info("IP geolocation: Using X-Real-IP header: #{ip_from_real}")
          ip_from_real

        {[], [], []} ->
          # Fall back to remote_ip
          ip_from_remote =
            case conn.remote_ip do
              {a, b, c, d} ->
                "#{a}.#{b}.#{c}.#{d}"

              {a, b, c, d, e, f, g, h} ->
                # Convert IPv6 tuple to proper IPv6 format
                {a, b, c, d, e, f, g, h}
                |> :inet.ntoa()
                |> to_string()

              _ ->
                nil
            end

          if ip_from_remote do
            Logger.info("IP geolocation: Using remote_ip: #{ip_from_remote}")
          end

          ip_from_remote
      end

    ip
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
    url = "http://ip-api.com/json/#{ip}"
    Logger.info("IP geolocation: Making HTTP request to #{url}")

    # Configure very short timeout to prevent blocking page load
    req_options = [
      # 500ms timeout - fail fast
      receive_timeout: 500,
      # No retries - we want fast failure
      retry: false,
      # Also limit connection pool wait time
      pool_timeout: 500
    ]

    case Req.get(url, req_options) do
      {:ok, %Req.Response{status: 200, body: body}} ->
        Logger.info("IP geolocation: Got 200 response with body: #{inspect(body)}")
        parse_ip_api_response(body)

      {:ok, response} ->
        Logger.warning("IP geolocation: Got non-200 response: #{response.status}")
        {:error, :invalid_response}

      {:error, reason} ->
        Logger.error("IP geolocation: HTTP request failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp parse_ip_api_response(body) when is_map(body) do
    case body do
      %{"status" => "success", "lat" => lat, "lon" => lon}
      when is_number(lat) and is_number(lon) ->
        if lat >= -90 and lat <= 90 and lon >= -180 and lon <= 180 do
          Logger.info("IP geolocation: Parsed coordinates lat=#{lat}, lon=#{lon}")
          {:ok, %{"lat" => lat / 1.0, "lng" => lon / 1.0}}
        else
          Logger.warning("IP geolocation: Invalid coordinates lat=#{lat}, lon=#{lon}")
          {:error, :invalid_coordinates}
        end

      %{"status" => "fail", "message" => message} ->
        Logger.warning("IP geolocation: API returned failure: #{message}")
        {:error, :api_failure}

      data ->
        Logger.warning("IP geolocation: Unexpected response format: #{inspect(data)}")
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
