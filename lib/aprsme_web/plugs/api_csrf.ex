defmodule AprsmeWeb.Plugs.ApiCSRF do
  @moduledoc """
  CSRF protection for API endpoints using X-Requested-With header or API tokens
  """
  import Phoenix.Controller
  import Plug.Conn

  def init(opts) do
    opts
  end

  def call(conn, _opts) do
    case get_req_header(conn, "content-type") do
      [content_type | _] when content_type in ["application/json", "application/json; charset=utf-8"] ->
        # For JSON requests, check for X-Requested-With header or valid API token
        check_csrf_protection(conn)

      _ ->
        # For non-JSON requests, proceed without CSRF check
        conn
    end
  end

  defp check_csrf_protection(conn) do
    case {get_req_header(conn, "x-requested-with"), get_req_header(conn, "authorization")} do
      {["XMLHttpRequest"], _} ->
        # Valid AJAX request
        conn

      {_, ["Bearer " <> _token]} ->
        # Has authorization header (API token) - would need validation in production
        conn

      {_, _} ->
        # Check for CSRF token in header
        case get_req_header(conn, "x-csrf-token") do
          [token] when byte_size(token) > 0 ->
            # Validate CSRF token
            validate_csrf_token(conn, token)

          _ ->
            reject_request(conn)
        end
    end
  end

  defp validate_csrf_token(conn, token) do
    case get_session(conn, "_csrf_token") do
      nil ->
        reject_request(conn)

      session_token ->
        if Plug.CSRFProtection.valid_state_and_csrf_token?(session_token, token) do
          conn
        else
          reject_request(conn)
        end
    end
  rescue
    _ ->
      reject_request(conn)
  end

  defp reject_request(conn) do
    conn
    |> put_status(:forbidden)
    |> json(%{
      error: "CSRF protection failed",
      message: "Include X-Requested-With: XMLHttpRequest header or valid CSRF token"
    })
    |> halt()
  end
end
