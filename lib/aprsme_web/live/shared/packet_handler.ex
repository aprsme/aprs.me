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
    device_identifier = Map.get(packet, :device_identifier) || Map.get(packet, "device_identifier")

    device =
      case device_identifier do
        nil -> nil
        "" -> nil
        identifier -> DeviceCache.lookup_device(identifier)
      end

    packet
    |> Map.put(:device_model, device && device.model)
    |> Map.put(:device_vendor, device && device.vendor)
    |> Map.put(:device_contact, device && device.contact)
    |> Map.put(:device_class, device && device.class)
  end

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
