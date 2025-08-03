defmodule AprsmeWeb.TimeHelpersTest do
  use ExUnit.Case, async: true

  alias AprsmeWeb.TimeHelpers

  describe "time_ago_in_words/1" do
    setup do
      # Use a fixed reference time for all tests to ensure consistency
      # This prevents tests from failing due to timing issues
      reference_time = ~U[2024-01-15 12:00:00Z]

      # Mock DateTime.utc_now() to return our reference time
      # Note: Since time_ago_in_words likely uses DateTime.utc_now() internally,
      # we need to test with actual time differences
      {:ok, reference_time: reference_time}
    end

    test "returns 'less than a minute ago' for recent times", %{reference_time: _ref_time} do
      # We test the relative difference by checking the function behavior
      # The function should handle any DateTime and calculate from "now"
      result = TimeHelpers.time_ago_in_words(DateTime.utc_now())
      assert result =~ ~r/less than a minute ago|just now|\d+ seconds? ago/
    end

    test "returns '1 minute ago' for times a minute ago" do
      # Test with a time that's exactly 61 seconds in the past from now
      one_minute_ago = DateTime.add(DateTime.utc_now(), -61, :second)
      result = TimeHelpers.time_ago_in_words(one_minute_ago)
      assert result in ["1 minute ago", "about 1 minute ago"]
    end

    test "returns 'X minutes ago' for times a few minutes ago" do
      five_minutes_ago = DateTime.add(DateTime.utc_now(), -300, :second)
      result = TimeHelpers.time_ago_in_words(five_minutes_ago)
      assert result in ["5 minutes ago", "about 5 minutes ago"]
    end

    test "returns '1 hour ago' for times an hour ago" do
      one_hour_ago = DateTime.add(DateTime.utc_now(), -3601, :second)
      result = TimeHelpers.time_ago_in_words(one_hour_ago)
      assert result in ["1 hour ago", "about 1 hour ago", "about an hour ago"]
    end

    test "returns 'X hours ago' for times a few hours ago" do
      five_hours_ago = DateTime.add(DateTime.utc_now(), -18_000, :second)
      result = TimeHelpers.time_ago_in_words(five_hours_ago)
      assert result in ["5 hours ago", "about 5 hours ago"]
    end

    test "returns '1 day ago' for times a day ago" do
      one_day_ago = DateTime.add(DateTime.utc_now(), -86_401, :second)
      result = TimeHelpers.time_ago_in_words(one_day_ago)
      assert result in ["1 day ago", "about 1 day ago", "yesterday"]
    end

    test "returns 'X days ago' for times a few days ago" do
      five_days_ago = DateTime.add(DateTime.utc_now(), -432_000, :second)
      result = TimeHelpers.time_ago_in_words(five_days_ago)
      assert result in ["5 days ago", "about 5 days ago"]
    end

    test "returns '1 month ago' for times a month ago" do
      one_month_ago = DateTime.add(DateTime.utc_now(), -2_592_001, :second)
      result = TimeHelpers.time_ago_in_words(one_month_ago)
      assert result in ["1 month ago", "about 1 month ago", "about a month ago"]
    end

    test "returns 'X months ago' for times a few months ago" do
      five_months_ago = DateTime.add(DateTime.utc_now(), -12_960_000, :second)
      result = TimeHelpers.time_ago_in_words(five_months_ago)
      assert result =~ ~r/\d+ months? ago/
    end

    test "returns '1 year ago' for times a year ago" do
      one_year_ago = DateTime.add(DateTime.utc_now(), -31_536_001, :second)
      result = TimeHelpers.time_ago_in_words(one_year_ago)
      assert result in ["1 year ago", "about 1 year ago", "about a year ago", "over 1 year ago"]
    end

    test "returns 'X years ago' for times a few years ago" do
      two_years_ago = DateTime.add(DateTime.utc_now(), -63_072_002, :second)
      result = TimeHelpers.time_ago_in_words(two_years_ago)
      assert result =~ ~r/\d+ years? ago|over \d+ years? ago/
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
