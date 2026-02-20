defmodule AprsmeWeb.MapLive.RfPath do
  @moduledoc """
  Handles RF path parsing and visualization for APRS packets.
  """

  alias Aprsme.Packets
  alias AprsmeWeb.Live.Shared.ParamUtils

  # APRS aliases that are not real stations
  @path_aliases ~w(WIDE WIDE1 WIDE2 WIDE3 WIDE4 WIDE5 WIDE6 WIDE7
    RELAY TRACE ECHO HOP DMR)

  @doc """
  Parse RF path string to extract digipeater/igate stations.

  APRS-IS path format: [RF digipeaters...],qA[R/S/O/C],[igate]

  - Everything before the qA* element = RF path (digipeaters)
  - The element after qA* = the igate that received/gated the packet
  - TCPIP/NOGATE paths are Internet-only and return empty
  - WIDE*, RELAY, TRACE, etc. are aliases, not real stations
  """
  @spec parse_rf_path(binary()) :: list(binary())
  def parse_rf_path(""), do: []

  def parse_rf_path(path) when is_binary(path) do
    if String.contains?(path, "TCPIP") or String.contains?(path, "NOGATE") do
      []
    else
      elements = path |> String.split(",") |> Enum.map(&String.trim/1)

      {rf_elements, igate_elements} = split_at_q_construct(elements)

      (rf_elements ++ igate_elements)
      |> Enum.map(&extract_callsign_from_path_element/1)
      |> Enum.filter(&(&1 != ""))
      |> Enum.reject(&path_alias?/1)
      |> Enum.uniq()
    end
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

  defp split_at_q_construct(elements) do
    q_index = Enum.find_index(elements, &String.starts_with?(&1, "qA"))

    case q_index do
      nil ->
        # No q-construct found — treat all elements as RF path
        {elements, []}

      idx ->
        rf_elements = Enum.slice(elements, 0, idx)
        # The element after qA* is the igate
        igate_elements = Enum.slice(elements, (idx + 1)..-1//1)
        {rf_elements, igate_elements}
    end
  end

  defp path_alias?(callsign) do
    # Check base callsign (without SSID) against known aliases
    base = callsign |> String.split("-") |> hd()
    # Also filter out aliases with numeric suffixes like WIDE1, WIDE2, TRACE3, HOP7
    stripped = String.replace(base, ~r/\d+$/, "")

    base in @path_aliases or stripped in @path_aliases
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
    if ParamUtils.valid_callsign?(callsign) do
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
