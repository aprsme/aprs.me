defmodule AprsmeWeb.PacketsLive.CallsignView do
  @moduledoc false
  use AprsmeWeb, :live_view

  import Ecto.Query

  alias Aprsme.DeviceParser
  alias Aprsme.EncodingUtils
  alias Aprsme.Packet
  alias Aprsme.Repo
  alias AprsmeWeb.Endpoint

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    # Validate and normalize callsign
    normalized_callsign = String.upcase(String.trim(callsign))

    if valid_callsign?(normalized_callsign) do
      # Subscribe to live packet updates if connected
      if connected?(socket) do
        Endpoint.subscribe("aprs_messages")
      end

      # Get stored packets for this callsign (up to 100)
      stored_packets = get_stored_packets(normalized_callsign, 100)
      all_packets = stored_packets
      latest_packet = List.first(all_packets)
      {symbol_table_id, symbol_code} = extract_symbol_info(latest_packet)

      socket =
        socket
        |> assign(:callsign, normalized_callsign)
        |> assign(:packets, stored_packets)
        |> assign(:live_packets, [])
        |> assign(:all_packets, all_packets)
        |> assign(:latest_symbol_table_id, symbol_table_id)
        |> assign(:latest_symbol_code, symbol_code)
        |> assign(:error, nil)

      {:ok, socket}
    else
      socket =
        socket
        |> assign(:callsign, normalized_callsign)
        |> assign(:packets, [])
        |> assign(:live_packets, [])
        |> assign(:all_packets, [])
        |> assign(:latest_symbol_table_id, "/")
        |> assign(:latest_symbol_code, ">")
        |> assign(:error, "Invalid callsign format")

      {:ok, socket}
    end
  end

  @impl true
  def handle_info(%{event: "packet", payload: payload}, socket) do
    # Handle incoming live packets - only process if they match our callsign
    if packet_matches_callsign?(payload, socket.assigns.callsign) do
      process_matching_packet(payload, socket)
    else
      {:noreply, socket}
    end
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp process_matching_packet(payload, socket) do
    sanitized_payload = EncodingUtils.sanitize_packet(payload)
    enriched_payload = enrich_packet_with_device_identifier(sanitized_payload)
    enriched_payload = enrich_packet_with_device_info(enriched_payload)

    current_live = socket.assigns.live_packets
    current_stored = socket.assigns.packets

    {updated_stored, updated_live} = update_packet_lists(current_stored, current_live, enriched_payload)
    all_packets = get_all_packets_list(updated_stored, updated_live)
    latest_packet = List.first(all_packets)
    {symbol_table_id, symbol_code} = extract_symbol_info(latest_packet)

    socket =
      socket
      |> assign(:packets, updated_stored)
      |> assign(:live_packets, updated_live)
      |> assign(:all_packets, all_packets)
      |> assign(:latest_symbol_table_id, symbol_table_id)
      |> assign(:latest_symbol_code, symbol_code)

    {:noreply, socket}
  end

  defp enrich_packet_with_device_identifier(sanitized_payload) do
    device_identifier =
      Map.get(sanitized_payload, :device_identifier) ||
        Map.get(sanitized_payload, "device_identifier") ||
        DeviceParser.extract_device_identifier(sanitized_payload)

    canonical_identifier =
      if is_binary(device_identifier) do
        matched_device = Aprsme.DeviceIdentification.lookup_device_by_identifier(device_identifier)
        if matched_device, do: matched_device.identifier, else: device_identifier
      else
        device_identifier
      end

    Map.put(sanitized_payload, :device_identifier, canonical_identifier)
  end

  # Private helper functions

  # Get recent packets for this callsign from the database (all packets, not just position)
  # Returns the latest packets regardless of age, filtered by callsign
  defp get_stored_packets(callsign, limit) do
    # Create query for all packets (not just position packets) for this callsign
    query =
      from p in Packet,
        order_by: [desc: p.received_at],
        limit: ^limit

    # Apply callsign filter using sender field for exact matching
    filtered_query = from p in query, where: ilike(p.sender, ^callsign)

    filtered_query
    |> Repo.all()
    |> Enum.map(&EncodingUtils.sanitize_packet/1)
    |> Enum.map(&enrich_packet_with_device_info/1)
  rescue
    error ->
      require Logger

      Logger.error("Failed to fetch stored packets for callsign #{callsign}: #{inspect(error)}")
      []
  end

  defp packet_matches_callsign?(packet, target_callsign) do
    # Check exact match for sender field only
    sender = packet.sender || ""

    # Convert to uppercase for case-insensitive comparison
    sender_upper = String.upcase(sender)
    target_upper = String.upcase(target_callsign)

    # Exact match only
    sender_upper == target_upper
  end

  # Helper to get all packets (stored + live) in chronological order
  # Combines stored and live packets, sorts by timestamp (newest first), and limits to 100
  defp get_all_packets_list(stored, live) do
    # Combine and sort by received_at timestamp (newest first)
    (live ++ stored)
    |> Enum.sort_by(&get_timestamp_microseconds/1, :desc)
    # Ensure we never exceed 100 total
    |> Enum.take(100)
  end

  defp get_timestamp_microseconds(packet) do
    case packet.received_at do
      %DateTime{} = dt ->
        DateTime.to_unix(dt, :microsecond)

      dt when is_binary(dt) ->
        case DateTime.from_iso8601(dt) do
          {:ok, parsed_dt, _} -> DateTime.to_unix(parsed_dt, :microsecond)
          _ -> 0
        end

      _ ->
        0
    end
  end

  # Helper to update stored and live packet lists, keeping total <= 100
  defp update_packet_lists(current_stored, current_live, sanitized_payload) do
    total_count = length(current_live) + length(current_stored)

    cond do
      total_count >= 100 and current_stored == [] ->
        live_without_oldest = Enum.drop(current_live, -1)
        {current_stored, [sanitized_payload | live_without_oldest]}

      total_count >= 100 ->
        stored_without_oldest = Enum.drop(current_stored, -1)
        {stored_without_oldest, [sanitized_payload | current_live]}

      true ->
        {current_stored, [sanitized_payload | current_live]}
    end
  end

  # Validates if the callsign format is reasonable
  defp valid_callsign?(callsign) do
    # Basic validation for amateur radio callsign format
    # Should be 3-8 characters, can contain letters, numbers, and one hyphen for SSID
    case String.trim(callsign) do
      "" -> false
      cs when byte_size(cs) < 3 or byte_size(cs) > 15 -> false
      cs -> Regex.match?(~r/^[A-Z0-9]+(-[A-Z0-9]{1,2})?$/i, cs)
    end
  end

  # Helper to extract symbol table and code from a packet
  defp extract_symbol_info(nil), do: {"/", ">"}

  defp extract_symbol_info(packet) do
    data = Map.get(packet, :data_extended) || %{}
    table = Map.get(data, :symbol_table_id) || Map.get(data, "symbol_table_id") || "/"
    code = Map.get(data, :symbol_code) || Map.get(data, "symbol_code") || ">"
    {table, code}
  end

  defp enrich_packet_with_device_info(packet) do
    device_identifier = Map.get(packet, :device_identifier) || Map.get(packet, "device_identifier")

    device =
      if is_binary(device_identifier), do: Aprsme.DeviceIdentification.lookup_device_by_identifier(device_identifier)

    model = if device, do: device.model
    vendor = if device, do: device.vendor

    packet
    |> Map.put(:device_model, model)
    |> Map.put(:device_vendor, vendor)
  end
end
