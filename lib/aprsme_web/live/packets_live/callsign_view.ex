defmodule AprsmeWeb.PacketsLive.CallsignView do
  @moduledoc false
  use AprsmeWeb, :live_view

  import Ecto.Query

  alias Aprsme.Callsign
  alias Aprsme.DeviceParser
  alias Aprsme.EncodingUtils
  alias Aprsme.Packet
  alias Aprsme.Repo
  alias AprsmeWeb.Endpoint
  alias AprsmeWeb.Live.SharedPacketHandler

  @impl true
  def mount(%{"callsign" => callsign}, _session, socket) do
    # Validate and normalize callsign
    normalized_callsign = Callsign.normalize(callsign)

    if Callsign.valid?(normalized_callsign) do
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
    SharedPacketHandler.handle_packet_update(payload, socket,
      filter_fn: SharedPacketHandler.callsign_filter(socket.assigns.callsign),
      process_fn: &process_matching_packet/2
    )
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp process_matching_packet(enriched_payload, socket) do
    # Device identifier enrichment specific to this module
    enriched_payload = enrich_packet_with_device_identifier(enriched_payload)

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
    # Use functional index on upper(sender) for performance
    normalized_callsign = String.upcase(String.trim(callsign))
    filtered_query = from p in query, where: fragment("upper(?)", p.sender) == ^normalized_callsign

    filtered_query
    |> Repo.all()
    |> Enum.map(&EncodingUtils.sanitize_packet/1)
    |> SharedPacketHandler.enrich_packets_with_device_info()
  rescue
    error ->
      require Logger

      Logger.error("Failed to fetch stored packets for callsign #{callsign}: #{inspect(error)}")
      []
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

  # Helper to extract symbol table and code from a packet
  defp extract_symbol_info(nil), do: {"/", ">"}

  defp extract_symbol_info(packet) do
    data = Map.get(packet, :data_extended) || %{}
    table = Map.get(data, :symbol_table_id) || Map.get(data, "symbol_table_id") || "/"
    code = Map.get(data, :symbol_code) || Map.get(data, "symbol_code") || ">"
    {table, code}
  end
end
