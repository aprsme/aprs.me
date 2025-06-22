defmodule AprsWeb.TimeHelpers do
  @moduledoc """
  Shared helpers for formatting time and dates in the web layer.
  """

  @doc """
  Returns a human-readable string for how long ago the given DateTime was.
  """
  def time_ago_in_words(datetime) do
    now = DateTime.utc_now()
    diff_seconds = DateTime.diff(now, datetime, :second)

    cond do
      diff_seconds < 60 -> "less than a minute"
      diff_seconds < 120 -> "1 minute"
      diff_seconds < 3600 -> "#{div(diff_seconds, 60)} minutes"
      diff_seconds < 7200 -> "1 hour"
      diff_seconds < 86_400 -> "#{div(diff_seconds, 3600)} hours"
      diff_seconds < 172_800 -> "1 day"
      diff_seconds < 2_592_000 -> "#{div(diff_seconds, 86_400)} days"
      diff_seconds < 5_184_000 -> "1 month"
      diff_seconds < 31_536_000 -> "#{div(diff_seconds, 2_592_000)} months"
      diff_seconds < 63_072_000 -> "1 year"
      true -> "#{div(diff_seconds, 31_536_000)} years"
    end <> " ago"
  end
end
