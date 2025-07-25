defmodule Aprsme.SentryFilter do
  @moduledoc """
  Filters out certain errors from being sent to Sentry.

  This module helps reduce noise in Sentry by filtering out expected errors
  like malformed HTTP requests that are missing required headers.
  """

  require Logger

  @doc """
  Callback function for Sentry's before_send hook.

  Returns nil to prevent the event from being sent to Sentry,
  or returns the event to allow it to be sent.
  """
  def before_send(event) do
    if should_filter_event?(event) do
      # Log locally but don't send to Sentry
      Logger.debug("Filtered Sentry event: #{inspect_error(event)}")
      nil
    else
      event
    end
  end

  # Check if the event should be filtered out
  defp should_filter_event?(event) do
    # Access the first exception from the list if it exists
    first_exception =
      case event.exception do
        [first | _] -> first
        _ -> nil
      end

    error_type = if first_exception, do: first_exception.type
    error_message = if first_exception, do: first_exception.value

    cond do
      # Filter out Bandit errors for missing Host header
      error_type == "Bandit.HTTPError" and
          String.contains?(error_message || "", "No host header") ->
        true

      # Filter out other common bot/scanner errors
      error_type == "Bandit.HTTPError" and
          String.contains?(error_message || "", "Unable to obtain host and port") ->
        true

      # Filter out Phoenix.Router.NoRouteError for common bot paths
      error_type == "Phoenix.Router.NoRouteError" and
          is_bot_path?(event) ->
        true

      # Allow all other errors through
      true ->
        false
    end
  end

  # Check if the request path looks like a bot/scanner
  defp is_bot_path?(event) do
    request_path =
      case event.request do
        %{url: url} when is_binary(url) -> url
        _ -> ""
      end

    bot_patterns = [
      ~r/\.php$/i,
      ~r/\.asp$/i,
      ~r/\.aspx$/i,
      ~r/wp-admin/i,
      ~r/wp-login/i,
      ~r/wordpress/i,
      ~r/admin/i,
      ~r/\.env$/,
      ~r/\.git/,
      ~r/phpmyadmin/i,
      ~r/mysql/i,
      ~r/config\./i,
      ~r/\.xml$/i,
      ~r/sitemap/i,
      ~r/robots\.txt$/i
    ]

    Enum.any?(bot_patterns, &Regex.match?(&1, request_path))
  end

  # Extract a readable error description
  defp inspect_error(event) do
    # Access the first exception from the list if it exists
    {error_type, error_message} =
      case event.exception do
        [%{type: type, value: value} | _] -> {type || "Unknown", value || "No message"}
        _ -> {"Unknown", "No message"}
      end

    "#{error_type}: #{error_message}"
  end
end
