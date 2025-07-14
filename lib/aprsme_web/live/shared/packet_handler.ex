defmodule AprsmeWeb.Live.SharedPacketHandler do
  @moduledoc """
  Shared functionality for handling packet updates in LiveView modules.
  Provides common patterns for packet filtering, enrichment, and processing.
  """

  alias Aprsme.Callsign
  alias Aprsme.DeviceCache
  alias Aprsme.EncodingUtils

  @doc """
  Handles incoming packet updates with filtering and processing.

  ## Options
    * `:filter_fn` - Function to determine if packet should be processed (required)
    * `:process_fn` - Function to process matching packets and update socket (required)
    * `:enrich_packet` - Whether to enrich packet with device info (default: true)
  """
  def handle_packet_update(packet, socket, opts) do
    filter_fn = Keyword.fetch!(opts, :filter_fn)
    process_fn = Keyword.fetch!(opts, :process_fn)
    enrich_packet? = Keyword.get(opts, :enrich_packet, true)

    if filter_fn.(packet, socket) do
      enriched_packet =
        if enrich_packet? do
          packet
          |> sanitize_packet()
          |> enrich_with_device_info()
        else
          packet
        end

      process_fn.(enriched_packet, socket)
    else
      {:noreply, socket}
    end
  end

  @doc """
  Checks if packet sender matches the given callsign.
  """
  def packet_matches_callsign?(packet, callsign) do
    packet_sender = Map.get(packet, "sender") || Map.get(packet, :sender, "")
    Callsign.matches?(packet_sender, callsign)
  end

  @doc """
  Checks if packet contains weather data.
  """
  def has_weather_data?(packet) do
    Enum.any?(EncodingUtils.weather_fields(), fn field ->
      value = Map.get(packet, field) || Map.get(packet, to_string(field))
      not is_nil(value)
    end)
  end

  @doc """
  Sanitizes packet strings to handle encoding issues.
  """
  def sanitize_packet(packet) do
    EncodingUtils.sanitize_packet(packet)
  end

  @doc """
  Enriches packet with device information.
  """
  def enrich_with_device_info(packet) do
    device_identifier = get_device_identifier(packet)

    device = lookup_device_info(device_identifier)

    packet
    |> Map.put(:device_model, device && device.model)
    |> Map.put(:device_vendor, device && device.vendor)
    |> Map.put(:device_contact, device && device.contact)
    |> Map.put(:device_class, device && device.class)
  end

  @doc """
  Batch enrich multiple packets with device info for better performance.
  """
  def enrich_packets_with_device_info(packets) when is_list(packets) do
    # Get unique device identifiers from all packets
    device_identifiers =
      packets
      |> Enum.map(&get_device_identifier/1)
      |> Enum.reject(&is_nil_or_empty/1)
      |> Enum.uniq()

    # Batch lookup devices (DeviceCache is already optimized with caching)
    device_map =
      Map.new(device_identifiers, fn identifier -> {identifier, DeviceCache.lookup_device(identifier)} end)

    # Enrich packets using the cached device map
    Enum.map(packets, fn packet ->
      device_identifier = get_device_identifier(packet)
      device = Map.get(device_map, device_identifier)

      packet
      |> Map.put(:device_model, device && device.model)
      |> Map.put(:device_vendor, device && device.vendor)
      |> Map.put(:device_contact, device && device.contact)
      |> Map.put(:device_class, device && device.class)
    end)
  end

  defp get_device_identifier(packet) do
    Map.get(packet, :device_identifier) || Map.get(packet, "device_identifier")
  end

  defp lookup_device_info(device_identifier) do
    case device_identifier do
      nil -> nil
      "" -> nil
      identifier -> DeviceCache.lookup_device(identifier)
    end
  end

  defp is_nil_or_empty(nil), do: true
  defp is_nil_or_empty(""), do: true
  defp is_nil_or_empty(_), do: false

  @doc """
  Creates a filter function that checks both callsign and weather data.
  """
  def callsign_and_weather_filter(callsign) do
    fn packet, _socket ->
      packet_matches_callsign?(packet, callsign) and has_weather_data?(packet)
    end
  end

  @doc """
  Creates a filter function that only checks callsign.
  """
  def callsign_filter(callsign) do
    fn packet, _socket ->
      packet_matches_callsign?(packet, callsign)
    end
  end
end
