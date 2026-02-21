defmodule AprsmeWeb.Live.Shared.PacketUtils do
  @moduledoc """
  Shared packet processing and memory management utilities.
  Used across multiple LiveView modules for consistent packet handling.
  """

  alias AprsmeWeb.Live.Shared.BoundsUtils

  @doc """
  Get unique callsign key from packet.
  """
  @spec get_callsign_key(map()) :: binary()
  def get_callsign_key(%{id: id}) when not is_nil(id), do: to_string(id)
  def get_callsign_key(%{"id" => id}) when not is_nil(id), do: to_string(id)
  def get_callsign_key(%{sender: sender}) when is_binary(sender), do: sender
  def get_callsign_key(%{"sender" => sender}) when is_binary(sender), do: sender
  def get_callsign_key(_packet), do: [:positive] |> System.unique_integer() |> to_string()

  @doc """
  Prune oldest packets from a map to enforce memory limits.
  """
  @spec prune_oldest_packets(map(), integer()) :: map()
  def prune_oldest_packets(packets_map, max_count) when map_size(packets_map) <= max_count do
    packets_map
  end

  def prune_oldest_packets(packets_map, max_count) do
    # Convert to list, sort by timestamp, take newest max_count packets
    packets_map
    |> Enum.to_list()
    |> Enum.sort_by(
      fn {_key, packet} ->
        case packet do
          %{received_at: %DateTime{} = dt} -> DateTime.to_unix(dt)
          %{"received_at" => timestamp} when is_integer(timestamp) -> timestamp
          _ -> 0
        end
      end,
      :desc
    )
    |> Enum.take(max_count)
    |> Map.new()
  end

  @doc """
  Parse trail duration with validation and bounds checking.
  """
  @spec parse_trail_duration(binary() | any()) :: integer()
  def parse_trail_duration(duration) when is_binary(duration) do
    case Integer.parse(duration) do
      {hours, ""} when hours in [1, 6, 12, 24, 48, 168] ->
        hours

      _ ->
        # Default to 1 hour if invalid
        1
    end
  end

  def parse_trail_duration(_), do: 1

  @doc """
  Parse historical hours with validation.
  """
  @spec parse_historical_hours(binary() | any()) :: integer()
  def parse_historical_hours(hours) when is_binary(hours) do
    case Integer.parse(hours) do
      {h, ""} when h in [1, 3, 6, 12, 24] ->
        h

      _ ->
        # Default to 1 hour if invalid
        1
    end
  end

  def parse_historical_hours(_), do: 1

  @doc """
  Filter packets by both time threshold and bounds.
  """
  @spec filter_packets_by_time_and_bounds(map(), map(), DateTime.t()) :: map()
  def filter_packets_by_time_and_bounds(packets, bounds, time_threshold) do
    packets
    |> Enum.filter(fn {_callsign, packet} ->
      BoundsUtils.within_bounds?(packet, bounds) &&
        packet_within_time_threshold?(packet, time_threshold)
    end)
    |> Map.new()
  end

  @doc """
  Check if packet contains weather data.
  """
  @spec has_weather_data?(map()) :: boolean()
  def has_weather_data?(packet) do
    Enum.any?(Aprsme.EncodingUtils.weather_fields(), fn field ->
      value = get_packet_field(packet, field, nil)
      not is_nil(value)
    end)
  end

  @doc """
  Check if packet is a weather packet based on its data.
  """
  @spec weather_packet?(map()) :: boolean()
  def weather_packet?(packet) do
    has_weather_data?(packet)
  end

  @doc """
  Get weather field value from packet with fallback.
  """
  @spec get_weather_field(map(), atom()) :: any()
  def get_weather_field(packet, field) do
    data_extended = Map.get(packet, :data_extended, Map.get(packet, "data_extended", %{})) || %{}

    Map.get(packet, field) ||
      Map.get(packet, to_string(field)) ||
      Map.get(data_extended, field) ||
      Map.get(data_extended, to_string(field))
  end

  @doc """
  Get packet field value with fallback and default.
  Checks both atom and string keys, and data_extended field.
  """
  @spec get_packet_field(map(), atom() | String.t(), any()) :: any()
  def get_packet_field(packet, field, default \\ nil) do
    data_extended = Map.get(packet, :data_extended, Map.get(packet, "data_extended", %{})) || %{}

    Map.get(packet, field) ||
      Map.get(packet, to_string(field)) ||
      Map.get(data_extended, field) ||
      Map.get(data_extended, to_string(field)) ||
      default
  end

  # Private helper functions

  @doc """
  Check if a packet is within the time threshold (not too old).
  """
  @spec packet_within_time_threshold?(struct(), any()) :: boolean()
  def packet_within_time_threshold?(%{received_at: received_at}, threshold) when not is_nil(received_at) do
    threshold_dt = convert_threshold_to_datetime(threshold)
    DateTime.compare(received_at, threshold_dt) in [:gt, :eq]
  end

  def packet_within_time_threshold?(_packet, _threshold), do: true

  defp convert_threshold_to_datetime(threshold) when is_integer(threshold) do
    # Assume seconds since epoch
    DateTime.from_unix!(threshold)
  end

  defp convert_threshold_to_datetime(threshold) when is_binary(threshold) do
    case DateTime.from_iso8601(threshold) do
      {:ok, dt, _} -> dt
      _ -> DateTime.utc_now()
    end
  end

  defp convert_threshold_to_datetime(%DateTime{} = threshold), do: threshold
  defp convert_threshold_to_datetime(_), do: DateTime.utc_now()

  @doc """
  Extracts symbol information from a packet with fallbacks.
  """
  @spec get_symbol_info(map()) :: {String.t(), String.t()}
  def get_symbol_info(packet) do
    AprsmeWeb.AprsSymbol.extract_from_packet(packet)
  end

  @doc """
  Extracts timestamp from a packet in ISO8601 format.
  """
  @spec get_timestamp(map()) :: String.t()
  def get_timestamp(packet) do
    received_at = Map.get(packet, :received_at) || Map.get(packet, "received_at")

    case received_at do
      nil ->
        ""

      %DateTime{} = dt ->
        DateTime.to_iso8601(dt)

      %NaiveDateTime{} = ndt ->
        ndt
        |> DateTime.from_naive!("Etc/UTC")
        |> DateTime.to_iso8601()

      timestamp when is_binary(timestamp) ->
        # If it's already a string, try to parse and reformat it
        case DateTime.from_iso8601(timestamp) do
          {:ok, dt, _} -> DateTime.to_iso8601(dt)
          _ -> timestamp
        end

      _ ->
        ""
    end
  end

  @doc """
  Generates a callsign string with SSID if present.
  """
  @spec generate_callsign(map()) :: String.t()
  def generate_callsign(packet) do
    base_callsign = get_packet_field(packet, :base_callsign, "")
    ssid = get_packet_field(packet, :ssid, nil)
    format_callsign_with_ssid(base_callsign, ssid)
  end

  @doc """
  Returns the display name for a packet.

  For APRS objects, returns the object_name.
  For APRS items, returns the item_name.
  Otherwise, falls back to the sender callsign.
  """
  @spec display_name(map()) :: String.t()
  def display_name(packet) do
    object_name = get_packet_field(packet, :object_name, nil)
    item_name = get_packet_field(packet, :item_name, nil)

    cond do
      non_empty_string?(object_name) -> String.trim(object_name)
      non_empty_string?(item_name) -> String.trim(item_name)
      true -> get_packet_field(packet, :sender, "")
    end
  end

  @doc """
  Get the map label for a packet.

  For map display, always show the sender callsign regardless of whether
  it's an object or item packet. This avoids confusion where the map label
  shows something different from what the user expects (like a balloon ID
  instead of the station callsign).

  If the packet is an object or item, you can append the object/item name
  in the popup or detail view.
  """
  @spec map_label(map()) :: String.t()
  def map_label(packet) do
    # Always return the sender for map labels
    get_packet_field(packet, :sender, "")
  end

  defp non_empty_string?(value) when is_binary(value), do: String.trim(value) != ""
  defp non_empty_string?(_), do: false

  defp format_callsign_with_ssid(base_callsign, nil), do: base_callsign
  defp format_callsign_with_ssid(base_callsign, ""), do: base_callsign
  defp format_callsign_with_ssid(base_callsign, ssid), do: "#{base_callsign}-#{ssid}"
end
