defmodule AprsmeWeb.TimeUtils do
  @moduledoc """
  Common time calculation utilities to reduce duplication across the codebase.
  """

  @doc """
  Returns a DateTime that is the specified number of hours before now.
  """
  @spec hours_ago(number()) :: DateTime.t()
  def hours_ago(hours) when is_number(hours) do
    DateTime.add(DateTime.utc_now(), -hours * 3600, :second)
  end

  @doc """
  Returns a DateTime that is one hour before now.
  """
  @spec one_hour_ago() :: DateTime.t()
  def one_hour_ago, do: hours_ago(1)

  @doc """
  Returns a DateTime that is 24 hours (one day) before now.
  """
  @spec one_day_ago() :: DateTime.t()
  def one_day_ago, do: hours_ago(24)

  @doc """
  Returns a DateTime that is 48 hours (two days) before now.
  """
  @spec two_days_ago() :: DateTime.t()
  def two_days_ago, do: hours_ago(48)

  @doc """
  Returns a DateTime that is 7 days (one week) before now.
  """
  @spec one_week_ago() :: DateTime.t()
  def one_week_ago, do: hours_ago(24 * 7)

  @doc """
  Returns a DateTime that is the specified number of days before now.
  """
  @spec days_ago(number()) :: DateTime.t()
  def days_ago(days) when is_number(days) do
    DateTime.add(DateTime.utc_now(), -days, :day)
  end

  @type time_range_atom :: :last_hour | :last_day | :last_two_days | :last_week

  @doc """
  Returns a tuple of {start_time, end_time} for common time ranges.
  """
  @spec time_range(time_range_atom()) :: {DateTime.t(), DateTime.t()}
  def time_range(:last_hour), do: {one_hour_ago(), DateTime.utc_now()}
  def time_range(:last_day), do: {one_day_ago(), DateTime.utc_now()}
  def time_range(:last_two_days), do: {two_days_ago(), DateTime.utc_now()}
  def time_range(:last_week), do: {one_week_ago(), DateTime.utc_now()}

  @doc """
  Returns a default time range of 48 hours.
  """
  @spec default_time_range() :: {DateTime.t(), DateTime.t()}
  def default_time_range, do: time_range(:last_two_days)
end
