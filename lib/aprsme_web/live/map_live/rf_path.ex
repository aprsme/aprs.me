defmodule AprsmeWeb.MapLive.RfPath do
  @moduledoc """
  Handles RF path parsing and visualization for APRS packets.
  """

  alias Aprsme.Packets
  alias AprsmeWeb.MapLive.Utils

  @doc """
  Parse RF path string to extract digipeater/igate stations.
  """
  @spec parse_rf_path(binary()) :: list(binary())
  def parse_rf_path(path) when is_binary(path) do
    path
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.map(&extract_callsign_from_path_element/1)
    |> Enum.filter(&(&1 != ""))
    |> Enum.uniq()
  end

  def parse_rf_path(_), do: []

  @doc """
  Get positions for path stations from the database.
  """
  @spec get_path_station_positions(list(binary()), Phoenix.LiveView.Socket.t()) :: list(map())
  def get_path_station_positions(path_stations, _socket) do
    # Limit to prevent excessive database queries
    limited_stations = Enum.take(path_stations, 10)

    limited_stations
    |> Enum.map(&get_station_position/1)
    |> Enum.filter(& &1)
  end

  defp extract_callsign_from_path_element(element) do
    # Remove asterisk (used flag) and extract callsign
    element
    |> String.replace("*", "")
    |> String.trim()
    |> String.upcase()
    |> validate_path_callsign()
  end

  defp validate_path_callsign(callsign) do
    if Utils.valid_callsign?(callsign) do
      callsign
    else
      ""
    end
  end

  defp get_station_position(callsign) do
    case Packets.get_latest_packet_for_callsign(callsign) do
      %{lat: lat, lon: lon} when is_number(lat) and is_number(lon) ->
        %{
          callsign: callsign,
          lat: lat,
          lng: lon
        }

      _ ->
        nil
    end
  end
end
