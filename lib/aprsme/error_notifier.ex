defmodule Aprsme.ErrorNotifier do
  @moduledoc """
  Sends email notifications when errors occur in production.

  Integrates with ErrorTracker to send emails only for the first occurrence
  of each unique error (based on error signature).
  """

  import Swoosh.Email

  require Logger

  @doc """
  Attaches telemetry handler for error occurrences.
  Called during application startup.
  """
  def attach do
    :telemetry.attach(
      "aprsme-error-email-notifier",
      [:error_tracker, :occurrence, :created],
      &__MODULE__.handle_error_occurrence/4,
      nil
    )
  end

  @doc """
  Telemetry handler for error occurrences.
  Sends email if this is the first occurrence and we're in production.
  """
  def handle_error_occurrence(_event, _measurements, metadata, _config) do
    occurrence = metadata.occurrence

    # Only send email for first occurrence in production
    if should_send_email?(occurrence) do
      send_error_email(occurrence)
    end

    :ok
  end

  defp should_send_email?(occurrence) do
    occurrence.occurrence_count == 1 && production?()
  end

  defp production? do
    Application.get_env(:aprsme, :env) == :prod
  end

  defp send_error_email(occurrence) do
    # Extract file and line information for the subject line
    first_line = get_first_stack_line(occurrence)
    file = if first_line, do: first_line.file, else: "unknown_file"
    line = if first_line, do: first_line.line, else: "?"
    error_name = String.slice(occurrence.reason, 0, 80) || "Unknown error"

    subject = "[APRS.me] Error: #{error_name} - #{file} - #{line}"

    email =
      new()
      |> to({"", "graham@mcintire.me"})
      |> from({"APRS.me Alerts", "alerts@aprs.me"})
      |> subject(subject)
      |> html_body(occurrence_email_html(occurrence))

    case Aprsme.Mailer.deliver(email) do
      {:ok, _} ->
        Logger.info("Error notification email sent successfully for error #{occurrence.error_id}")
        {:ok, "Email sent successfully"}

      {:error, reason} ->
        Logger.error("Failed to send error notification: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp occurrence_email_html(occurrence) do
    # Extract the first line from the stacktrace for the error location
    first_line = get_first_stack_line(occurrence)

    # Extract useful information from the stacktrace line
    error_location =
      if first_line do
        "#{first_line.module}.#{first_line.function} (#{first_line.file}:#{first_line.line})"
      else
        "Unknown location"
      end

    # Extract useful context information
    view = occurrence.context["live_view.view"] || "Unknown view"
    path = occurrence.context["request.path"] || "Unknown path"

    # Get the error URL
    error_url = "https://aprs.me/dev/error-tracker/errors/#{occurrence.error_id}"

    # Escape all user-controlled fields to prevent XSS
    escaped_error_id = occurrence.error_id |> Phoenix.HTML.html_escape() |> Phoenix.HTML.safe_to_string()

    escaped_reason =
      occurrence.reason |> String.slice(0..199) |> Phoenix.HTML.html_escape() |> Phoenix.HTML.safe_to_string()

    escaped_location = error_location |> Phoenix.HTML.html_escape() |> Phoenix.HTML.safe_to_string()
    escaped_view = view |> Phoenix.HTML.html_escape() |> Phoenix.HTML.safe_to_string()
    escaped_path = path |> Phoenix.HTML.html_escape() |> Phoenix.HTML.safe_to_string()
    escaped_url = error_url |> Phoenix.HTML.html_escape() |> Phoenix.HTML.safe_to_string()

    """
    <div style="max-width: 600px; margin: 0 auto; padding: 20px; font-family: system-ui, -apple-system, sans-serif;">
      <div style="background-color: white; border-radius: 8px; padding: 24px; box-shadow: 0 1px 3px 0 rgb(0 0 0 / 0.1);">
        <h1 style="color: #dc2626; font-size: 24px; font-weight: bold; margin-bottom: 16px;">
          New Error Detected
        </h1>
        <p style="color: #374151; font-size: 16px; line-height: 24px; margin-bottom: 24px;">
          ErrorTracker has detected a new error in production:
        </p>

        <div style="background-color: #f9fafb; border-radius: 6px; padding: 16px; margin-bottom: 24px;">
          <p><strong>Error ID:</strong> #{escaped_error_id}</p>
          <p><strong>Reason:</strong> #{escaped_reason}</p>
          <p><strong>Location:</strong> #{escaped_location}</p>
          <p><strong>View:</strong> #{escaped_view}</p>
          <p><strong>Request Path:</strong> #{escaped_path}</p>
          <p><strong>Time:</strong> #{format_time()}</p>
        </div>

        <p style="margin-bottom: 24px;">
          <a href="#{escaped_url}"
             style="display: inline-block; background-color: #dc2626; color: white; font-weight: 500;
                    padding: 8px 16px; border-radius: 4px; text-decoration: none;">
            View Error Details
          </a>
        </p>
      </div>
    </div>
    """
  end

  defp get_first_stack_line(occurrence) do
    case occurrence.stacktrace do
      %{lines: [first | _]} -> first
      _ -> nil
    end
  end

  defp format_time do
    DateTime.to_string(DateTime.utc_now())
  end
end
