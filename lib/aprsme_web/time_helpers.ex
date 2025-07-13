defmodule AprsmeWeb.TimeHelpers do
  @moduledoc """
  Shared helpers for formatting time and dates in the web layer.
  """
  use Gettext, backend: AprsmeWeb.Gettext

  @doc """
  Returns a human-readable string for how long ago the given DateTime was.
  """
  def time_ago_in_words(nil), do: gettext("unknown time") <> " " <> gettext("ago")

  def time_ago_in_words(%NaiveDateTime{} = naive_datetime) do
    # Convert NaiveDateTime to DateTime assuming UTC
    datetime = DateTime.from_naive!(naive_datetime, "Etc/UTC")
    time_ago_in_words(datetime)
  end

  def time_ago_in_words(datetime) do
    now = DateTime.utc_now()
    diff_seconds = DateTime.diff(now, datetime, :second)

    format_time_diff(diff_seconds) <> " " <> gettext("ago")
  end

  defp format_time_diff(seconds) when seconds < 60, do: gettext("less than a minute")
  defp format_time_diff(seconds) when seconds < 120, do: gettext("1 minute")
  defp format_time_diff(seconds) when seconds < 3600, do: gettext("%{count} minutes", count: div(seconds, 60))
  defp format_time_diff(seconds) when seconds < 7200, do: gettext("1 hour")
  defp format_time_diff(seconds) when seconds < 86_400, do: gettext("%{count} hours", count: div(seconds, 3600))
  defp format_time_diff(seconds) when seconds < 172_800, do: gettext("1 day")
  defp format_time_diff(seconds) when seconds < 2_592_000, do: gettext("%{count} days", count: div(seconds, 86_400))
  defp format_time_diff(seconds) when seconds < 5_184_000, do: gettext("1 month")

  defp format_time_diff(seconds) when seconds < 31_536_000, do: gettext("%{count} months", count: div(seconds, 2_592_000))

  defp format_time_diff(seconds) when seconds < 63_072_000, do: gettext("1 year")
  defp format_time_diff(seconds), do: gettext("%{count} years", count: div(seconds, 31_536_000))

  @doc """
  Converts various timestamp formats into a standard DateTime object.
  Handles ISO 8601 strings, Unix timestamps in milliseconds, and existing
  DateTime or NaiveDateTime structs.
  """
  def to_datetime(ts) do
    cond do
      is_binary(ts) ->
        case DateTime.from_iso8601(ts) do
          {:ok, dt, _} -> dt
          _ -> nil
        end

      is_integer(ts) ->
        DateTime.from_unix!(ts, :millisecond)

      match?(%DateTime{}, ts) ->
        ts

      match?(%NaiveDateTime{}, ts) ->
        DateTime.from_naive!(ts, "Etc/UTC")

      true ->
        nil
    end
  end
end
