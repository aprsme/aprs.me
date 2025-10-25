defmodule AprsmeWeb.Plugs.LogFilter do
  @moduledoc """
  Log filter that excludes healthcheck and other noisy requests from Phoenix logs.

  Used by Plug.Telemetry to determine the log level for requests.
  Returns `false` for requests that shouldn't be logged (like K8s healthchecks),
  or `:info` for normal requests.
  """

  @doc """
  Determines the log level for a given request.

  Returns `false` to skip logging, or `:info` to log at info level.
  """
  def log_level(conn) do
    if should_skip_logging?(conn) do
      false
    else
      :info
    end
  end

  defp should_skip_logging?(conn) do
    conn.request_path in ["/health", "/ready", "/"]
  end
end
