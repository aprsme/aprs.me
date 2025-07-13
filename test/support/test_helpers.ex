defmodule AprsmeWeb.TestHelpers do
  @moduledoc """
  Common test helper functions to reduce duplication across test files.
  """

  alias Aprsme.Packet
  alias Aprsme.Repo

  @doc """
  Creates a test packet with default values that can be overridden.
  """
  def create_test_packet(attrs \\ %{}) do
    default_attrs = %{
      sender: "TEST-1",
      base_callsign: "TEST",
      ssid: "1",
      lat: Decimal.new("33.0000"),
      lon: Decimal.new("-96.0000"),
      has_position: true,
      received_at: DateTime.truncate(DateTime.utc_now(), :second),
      data_type: "position"
    }

    attrs = Map.merge(default_attrs, attrs)

    Repo.insert(%Packet{
      sender: attrs.sender,
      base_callsign: attrs.base_callsign,
      ssid: attrs.ssid,
      lat: attrs.lat,
      lon: attrs.lon,
      has_position: attrs.has_position,
      received_at: attrs.received_at,
      data_type: attrs.data_type,
      symbol_table_id: Map.get(attrs, :symbol_table_id),
      symbol_code: Map.get(attrs, :symbol_code),
      temperature: Map.get(attrs, :temperature),
      humidity: Map.get(attrs, :humidity),
      wind_speed: Map.get(attrs, :wind_speed)
    })
  end

  @doc """
  Creates common test bounds for Texas area.
  """
  def texas_bounds do
    %{
      "north" => "33.0",
      "south" => "32.0",
      "east" => "-96.0",
      "west" => "-97.0"
    }
  end

  @doc """
  Creates common test bounds for a restrictive area.
  """
  def restrictive_bounds do
    %{
      "north" => "31.0",
      "south" => "30.0",
      "east" => "-95.0",
      "west" => "-96.0"
    }
  end

  @doc """
  Common time calculations used across tests.
  """
  def hours_ago(hours) when is_number(hours) do
    DateTime.add(DateTime.utc_now(), -hours * 3600, :second)
  end

  def minutes_ago(minutes) when is_number(minutes) do
    DateTime.add(DateTime.utc_now(), -minutes * 60, :second)
  end

  def days_ago(days) when is_number(days) do
    DateTime.add(DateTime.utc_now(), -days * 86_400, :second)
  end
end
