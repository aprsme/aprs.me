defmodule AprsmeWeb.TimeHelpersTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.TimeHelpers

  describe "time_ago_in_words/1" do
    test "returns 'less than a minute ago' for recent times" do
      now = DateTime.utc_now()
      assert TimeHelpers.time_ago_in_words(now) == "less than a minute ago"
    end

    test "returns '1 minute ago' for times a minute ago" do
      one_minute_ago = DateTime.add(DateTime.utc_now(), -61, :second)
      assert TimeHelpers.time_ago_in_words(one_minute_ago) == "1 minute ago"
    end

    test "returns 'X minutes ago' for times a few minutes ago" do
      five_minutes_ago = DateTime.add(DateTime.utc_now(), -300, :second)
      assert TimeHelpers.time_ago_in_words(five_minutes_ago) == "5 minutes ago"
    end

    test "returns '1 hour ago' for times an hour ago" do
      one_hour_ago = DateTime.add(DateTime.utc_now(), -3601, :second)
      assert TimeHelpers.time_ago_in_words(one_hour_ago) == "1 hour ago"
    end

    test "returns 'X hours ago' for times a few hours ago" do
      five_hours_ago = DateTime.add(DateTime.utc_now(), -18_000, :second)
      assert TimeHelpers.time_ago_in_words(five_hours_ago) == "5 hours ago"
    end

    test "returns '1 day ago' for times a day ago" do
      one_day_ago = DateTime.add(DateTime.utc_now(), -86_401, :second)
      assert TimeHelpers.time_ago_in_words(one_day_ago) == "1 day ago"
    end

    test "returns 'X days ago' for times a few days ago" do
      five_days_ago = DateTime.add(DateTime.utc_now(), -432_000, :second)
      assert TimeHelpers.time_ago_in_words(five_days_ago) == "5 days ago"
    end

    test "returns '1 month ago' for times a month ago" do
      one_month_ago = DateTime.add(DateTime.utc_now(), -2_592_001, :second)
      assert TimeHelpers.time_ago_in_words(one_month_ago) == "1 month ago"
    end

    test "returns 'X months ago' for times a few months ago" do
      five_months_ago = DateTime.add(DateTime.utc_now(), -12_960_000, :second)
      assert TimeHelpers.time_ago_in_words(five_months_ago) == "5 months ago"
    end

    test "returns '1 year ago' for times a year ago" do
      one_year_ago = DateTime.add(DateTime.utc_now(), -31_536_001, :second)
      assert TimeHelpers.time_ago_in_words(one_year_ago) == "1 year ago"
    end

    test "returns 'X years ago' for times a few years ago" do
      two_years_ago = DateTime.add(DateTime.utc_now(), -63_072_002, :second)
      assert TimeHelpers.time_ago_in_words(two_years_ago) == "2 years ago"
    end
  end

  describe "to_datetime/1" do
    test "converts an ISO 8601 string to a DateTime" do
      iso_string = "2023-01-01T12:00:00Z"
      {:ok, expected_dt, _} = DateTime.from_iso8601(iso_string)
      assert TimeHelpers.to_datetime(iso_string) == expected_dt
    end

    test "returns nil for an invalid ISO 8601 string" do
      assert TimeHelpers.to_datetime("invalid-datetime") == nil
    end

    test "converts a Unix timestamp in milliseconds to a DateTime" do
      timestamp_ms = 1_672_574_400_000
      expected_dt = DateTime.from_unix!(timestamp_ms, :millisecond)
      assert TimeHelpers.to_datetime(timestamp_ms) == expected_dt
    end

    test "returns the same DateTime if a DateTime is passed" do
      dt = DateTime.utc_now()
      assert TimeHelpers.to_datetime(dt) == dt
    end

    test "converts a NaiveDateTime to a DateTime" do
      ndt = NaiveDateTime.utc_now()
      expected_dt = DateTime.from_naive!(ndt, "Etc/UTC")
      assert TimeHelpers.to_datetime(ndt) == expected_dt
    end

    test "returns nil for other types" do
      assert TimeHelpers.to_datetime(%{}) == nil
    end
  end
end
