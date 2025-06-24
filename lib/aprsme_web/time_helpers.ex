defmodule AprsmeWeb.TimeHelpers do
  @moduledoc """
  Shared helpers for formatting time and dates in the web layer.
  """

  @doc """
  Returns a human-readable string for how long ago the given DateTime was.
  """
  def time_ago_in_words(datetime) do
    now = DateTime.utc_now()
    diff_seconds = DateTime.diff(now, datetime, :second)

    format_time_diff(diff_seconds) <> " ago"
  end

  defp format_time_diff(seconds) when seconds < 60, do: "less than a minute"
  defp format_time_diff(seconds) when seconds < 120, do: "1 minute"
  defp format_time_diff(seconds) when seconds < 3600, do: "#{div(seconds, 60)} minutes"
  defp format_time_diff(seconds) when seconds < 7200, do: "1 hour"
  defp format_time_diff(seconds) when seconds < 86_400, do: "#{div(seconds, 3600)} hours"
  defp format_time_diff(seconds) when seconds < 172_800, do: "1 day"
  defp format_time_diff(seconds) when seconds < 2_592_000, do: "#{div(seconds, 86_400)} days"
  defp format_time_diff(seconds) when seconds < 5_184_000, do: "1 month"

  defp format_time_diff(seconds) when seconds < 31_536_000, do: "#{div(seconds, 2_592_000)} months"

  defp format_time_diff(seconds) when seconds < 63_072_000, do: "1 year"
  defp format_time_diff(seconds), do: "#{div(seconds, 31_536_000)} years"

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
