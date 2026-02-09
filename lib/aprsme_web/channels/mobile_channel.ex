defmodule AprsmeWeb.MobileChannel do
  @moduledoc """
  Channel for mobile clients to receive real-time APRS packets filtered by geographic bounds.

  ## Usage

  1. Connect to socket: wss://aprs.me/mobile/websocket
  2. Join channel: mobile:packets
  3. Subscribe to bounds: push "subscribe_bounds" with bounds payload
  4. Receive packets: listen for "packet" events
  5. Update bounds: push "update_bounds" with new bounds

  ## Messages

  ### Client -> Server

  - "subscribe_bounds" - Subscribe to packets within geographic bounds
    Payload: %{north: float, south: float, east: float, west: float}

  - "update_bounds" - Update subscription bounds (e.g., when user pans/zooms map)
    Payload: %{north: float, south: float, east: float, west: float}

  - "unsubscribe" - Stop receiving packets

  - "search_callsign" - Search for callsigns matching a pattern
    Payload: %{query: string, limit: integer (optional)}

  - "subscribe_callsign" - Subscribe to live updates for a callsign pattern
    Payload: %{callsign: string, hours_back: integer (optional)}

  - "unsubscribe_callsign" - Stop receiving updates for tracked callsign

  ### Server -> Client

  - "packet" - New APRS packet within subscribed bounds or matching tracked callsign
    Payload: %{
      id: string,
      callsign: string,
      lat: float,
      lng: float,
      timestamp: string (ISO 8601),
      symbol_table_id: string,
      symbol_code: string,
      comment: string (optional),
      altitude: float (optional),
      speed: float (optional),
      course: integer (optional)
    }

  - "subscription_confirmed" - Bounds subscription successful
    Payload: %{bounds: map, message: string}
  """
  use AprsmeWeb, :channel

  alias AprsmeWeb.MapLive.MapHelpers

  require Logger

  @impl true
  def join("mobile:packets", _payload, socket) do
    Logger.info("Mobile client joined packets channel")
    {:ok, %{message: "Connected to APRS mobile channel"}, socket}
  end

  @impl true
  def handle_in(
        "subscribe_bounds",
        %{"north" => north, "south" => south, "east" => east, "west" => west} = payload,
        socket
      ) do
    Logger.info("Mobile websocket received subscribe_bounds: #{inspect(payload)}")

    bounds = %{
      north: ensure_float(north),
      south: ensure_float(south),
      east: ensure_float(east),
      west: ensure_float(west)
    }

    # Validate bounds
    case validate_bounds(bounds) do
      :ok ->
        # Generate unique client ID
        client_id = "mobile_#{:erlang.phash2(self())}"

        # Subscribe to StreamingPacketsPubSub with bounds
        Aprsme.StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

        # Store client info in socket
        socket =
          socket
          |> assign(:client_id, client_id)
          |> assign(:bounds, bounds)
          |> assign(:subscribed, true)

        Logger.info("Mobile client #{client_id} subscribed to bounds: #{inspect(bounds)}")

        # Load and send historical packets
        socket = load_historical_packets(socket, bounds, payload)

        {:reply, {:ok, %{bounds: bounds, message: "Subscribed to packet stream"}}, socket}

      {:error, reason} ->
        {:reply, {:error, %{message: reason}}, socket}
    end
  end

  @impl true
  def handle_in(
        "update_bounds",
        %{"north" => north, "south" => south, "east" => east, "west" => west} = payload,
        socket
      ) do
    Logger.info("Mobile websocket received update_bounds: #{inspect(payload)}")

    bounds = %{
      north: ensure_float(north),
      south: ensure_float(south),
      east: ensure_float(east),
      west: ensure_float(west)
    }

    # Check if subscribed
    if Map.get(socket.assigns, :subscribed, false) do
      # Validate bounds
      case validate_bounds(bounds) do
        :ok ->
          update_subscription_bounds(socket, bounds)

        {:error, reason} ->
          {:reply, {:error, %{message: reason}}, socket}
      end
    else
      {:reply, {:error, %{message: "Not subscribed. Call subscribe_bounds first."}}, socket}
    end
  end

  @impl true
  def handle_in("unsubscribe", payload, socket) do
    Logger.info("Mobile websocket received unsubscribe: #{inspect(payload)}")

    if socket.assigns[:subscribed] && socket.assigns[:bounds] do
      Aprsme.StreamingPacketsPubSub.unsubscribe(self())

      socket =
        socket
        |> assign(:subscribed, false)
        |> assign(:bounds, nil)

      Logger.info("Mobile client #{socket.assigns[:client_id]} unsubscribed from packet stream")

      {:reply, {:ok, %{message: "Unsubscribed from packet stream"}}, socket}
    else
      {:reply, {:ok, %{message: "Not subscribed"}}, socket}
    end
  end

  @impl true
  def handle_in("search_callsign", %{"query" => query} = payload, socket) do
    Logger.info("Mobile websocket received search_callsign: #{inspect(payload)}")

    # Trim whitespace and control characters from query
    query = String.trim(query)

    limit = Map.get(payload, "limit", 50)
    limit = min(limit, 500)

    results = search_callsign(query, limit)

    {:reply, {:ok, %{results: results, count: length(results)}}, socket}
  end

  @impl true
  def handle_in("subscribe_callsign", %{"callsign" => callsign} = payload, socket) do
    Logger.info("Mobile websocket received subscribe_callsign: #{inspect(payload)}")

    hours_back = Map.get(payload, "hours_back", 24)
    # Max 1 week
    hours_back = min(hours_back, 168)

    # Normalize callsign - trim whitespace and convert to uppercase
    callsign = callsign |> String.trim() |> String.upcase()

    # Load historical packets for this callsign
    socket = load_callsign_history(socket, callsign, hours_back)

    # Store tracked callsign in socket
    socket = assign(socket, :tracked_callsign, callsign)

    {:reply, {:ok, %{callsign: callsign, message: "Subscribed to callsign updates"}}, socket}
  end

  @impl true
  def handle_in("unsubscribe_callsign", payload, socket) do
    Logger.info("Mobile websocket received unsubscribe_callsign: #{inspect(payload)}")

    if socket.assigns[:tracked_callsign] do
      callsign = socket.assigns.tracked_callsign
      socket = assign(socket, :tracked_callsign, nil)

      Logger.info("Mobile client #{socket.assigns[:client_id]} unsubscribed from callsign: #{callsign}")

      {:reply, {:ok, %{message: "Unsubscribed from callsign updates"}}, socket}
    else
      {:reply, {:ok, %{message: "Not tracking any callsign"}}, socket}
    end
  end

  @impl true
  def handle_info({:streaming_packet, packet}, socket) do
    # Check if packet matches tracked callsign (if any)
    should_send =
      if tracked_callsign = socket.assigns[:tracked_callsign] do
        packet_callsign = get_field(packet, :sender) || get_field(packet, :base_callsign) || ""
        callsign_matches?(packet_callsign, tracked_callsign)
      else
        # If not tracking a callsign, send all packets (geographic filtering already applied)
        true
      end

    if should_send do
      # Convert packet to mobile-friendly format
      packet_data = build_mobile_packet(packet)

      # Push packet to mobile client
      push(socket, "packet", packet_data)
    end

    {:noreply, socket}
  end

  # Clean up on terminate
  @impl true
  def terminate(_reason, socket) do
    if socket.assigns[:subscribed] && socket.assigns[:bounds] do
      Aprsme.StreamingPacketsPubSub.unsubscribe(self())
    end

    :ok
  end

  # Private functions

  defp validate_bounds(%{north: north, south: south, east: east, west: west}) do
    cond do
      not all_numeric?(north, south, east, west) ->
        {:error, "All bounds must be numeric"}

      not valid_latitudes?(north, south) ->
        {:error, "Latitude must be between -90 and 90"}

      not valid_longitudes?(east, west) ->
        {:error, "Longitude must be between -180 and 180"}

      north <= south ->
        {:error, "North must be greater than south"}

      true ->
        :ok
    end
  end

  defp all_numeric?(north, south, east, west) do
    is_number(north) and is_number(south) and is_number(east) and is_number(west)
  end

  defp valid_latitudes?(north, south) do
    north >= -90 and north <= 90 and south >= -90 and south <= 90
  end

  defp valid_longitudes?(east, west) do
    east >= -180 and east <= 180 and west >= -180 and west <= 180
  end

  defp ensure_float(value) when is_integer(value), do: value * 1.0
  defp ensure_float(value) when is_float(value), do: value
  defp ensure_float(value) when is_binary(value), do: String.to_float(value)
  defp ensure_float(value), do: value

  defp build_mobile_packet(packet) do
    # Extract coordinates
    {lat, lon, _data_extended} = MapHelpers.get_coordinates(packet)

    # Build minimal packet data for mobile
    %{
      id: get_field(packet, :id),
      callsign: get_field(packet, :sender) || get_field(packet, :base_callsign),
      lat: to_float(lat),
      lng: to_float(lon),
      timestamp: packet |> get_field(:received_at) |> format_timestamp(),
      symbol_table_id: get_field(packet, :symbol_table_id, "/"),
      symbol_code: get_field(packet, :symbol_code, ">"),
      comment: get_field(packet, :comment),
      altitude: get_field(packet, :altitude),
      speed: get_field(packet, :speed),
      course: get_field(packet, :course),
      path: get_field(packet, :path)
    }
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()
  end

  defp get_field(packet, field, default \\ nil) when is_map(packet) do
    Map.get(packet, field) || Map.get(packet, to_string(field), default)
  end

  defp to_float(nil), do: nil
  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_integer(value), do: value * 1.0

  defp to_float(value) when is_binary(value) do
    case Float.parse(value) do
      {float, _} -> float
      :error -> nil
    end
  end

  defp to_float(%Decimal{} = value), do: Decimal.to_float(value)
  defp to_float(_), do: nil

  defp format_timestamp(nil), do: nil

  defp format_timestamp(%DateTime{} = dt) do
    DateTime.to_iso8601(dt)
  end

  defp format_timestamp(%NaiveDateTime{} = ndt) do
    ndt
    |> DateTime.from_naive!("Etc/UTC")
    |> DateTime.to_iso8601()
  end

  defp format_timestamp(_), do: nil

  defp load_historical_packets(socket, bounds, payload) do
    # Get optional parameters from payload
    limit = Map.get(payload, "limit", 1000)
    hours_back = Map.get(payload, "hours_back", 1)

    # Ensure reasonable limits
    limit = min(limit, 5000)
    hours_back = min(hours_back, 24)

    # Query historical packets within bounds
    bounds_list = [
      bounds.west,
      bounds.south,
      bounds.east,
      bounds.north
    ]

    params = %{
      bounds: bounds_list,
      limit: limit,
      offset: 0,
      hours_back: hours_back
    }

    Logger.debug("Mobile client loading historical packets with params: #{inspect(params)}")

    packets =
      try do
        Aprsme.Packets.get_recent_packets(params)
      rescue
        error ->
          Logger.error("Error loading historical packets for mobile client: #{inspect(error)}")
          []
      end

    Logger.info("Loaded #{length(packets)} historical packets for mobile client")

    # Send historical packets to client
    if packets != [] do
      # Convert packets to mobile format and send them
      Enum.each(packets, fn packet ->
        packet_data = build_mobile_packet(packet)
        push(socket, "packet", packet_data)
      end)
    end

    socket
  end

  defp search_callsign(query, limit) do
    import Ecto.Query

    # Normalize query to uppercase
    query = String.upcase(query)

    Logger.info("Searching for callsign: #{query}")

    # Build search pattern - support wildcard with *
    pattern =
      if String.contains?(query, "*") do
        # Convert * to SQL % wildcard
        String.replace(query, "*", "%")
      else
        # If no wildcard, search for exact match or with SSID
        "#{query}%"
      end

    Logger.info("Search pattern: #{pattern}")

    # Query for matching callsigns - optimized version
    # Use a subquery to get only the most recent packet per sender
    results =
      try do
        subquery =
          from p in Aprsme.Packet,
            where: ilike(p.sender, ^pattern),
            distinct: p.sender,
            select: %{
              callsign: p.sender,
              base_callsign: p.base_callsign,
              last_seen: max(p.received_at),
              # Use first_value for lat/lon from most recent packet
              lat: fragment("(array_agg(? ORDER BY ? DESC))[1]", p.lat, p.received_at),
              lon: fragment("(array_agg(? ORDER BY ? DESC))[1]", p.lon, p.received_at)
            },
            group_by: [p.sender, p.base_callsign],
            order_by: [desc: max(p.received_at)],
            limit: ^limit

        results = Aprsme.Repo.all(subquery)
        Logger.info("Search returned #{length(results)} results: #{inspect(results)}")
        results
      rescue
        error ->
          Logger.error("Error searching callsigns: #{inspect(error)}")
          []
      end

    results
  end

  defp load_callsign_history(socket, callsign, hours_back) do
    import Ecto.Query

    # Build pattern for callsign matching (supports wildcards)
    pattern =
      if String.contains?(callsign, "*") do
        String.replace(callsign, "*", "%")
      else
        # Exact callsign match
        callsign
      end

    # Calculate time window
    since = DateTime.add(DateTime.utc_now(), -hours_back * 3600, :second)

    # Query historical packets
    packets =
      try do
        Aprsme.Repo.all(
          from p in Aprsme.Packet,
            where: p.received_at >= ^since,
            where: ilike(p.sender, ^pattern) or ilike(p.base_callsign, ^pattern),
            order_by: [asc: p.received_at],
            limit: 1000
        )
      rescue
        error ->
          Logger.error("Error loading callsign history: #{inspect(error)}")
          []
      end

    Logger.info("Loaded #{length(packets)} historical packets for callsign #{callsign}")

    # Send historical packets to client
    if packets != [] do
      Enum.each(packets, fn packet ->
        packet_data = build_mobile_packet(packet)
        push(socket, "packet", packet_data)
      end)
    end

    socket
  end

  defp callsign_matches?(packet_callsign, tracked_callsign) do
    # Normalize both to uppercase
    packet_callsign = String.upcase(packet_callsign)
    tracked_callsign = String.upcase(tracked_callsign)

    cond do
      # Exact match
      packet_callsign == tracked_callsign ->
        true

      # Wildcard match (e.g., "W5ISP*" matches "W5ISP-9")
      String.contains?(tracked_callsign, "*") ->
        pattern = String.replace(tracked_callsign, "*", "")
        String.starts_with?(packet_callsign, pattern)

      # Base callsign match (e.g., "W5ISP" matches "W5ISP-9")
      String.starts_with?(packet_callsign, tracked_callsign <> "-") ->
        true

      # No match
      true ->
        false
    end
  end

  defp update_subscription_bounds(socket, bounds) do
    # Unsubscribe from old bounds if they exist
    if socket.assigns[:bounds] do
      Aprsme.StreamingPacketsPubSub.unsubscribe(self())
    end

    # Subscribe to new bounds
    Aprsme.StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

    socket = assign(socket, :bounds, bounds)

    Logger.debug("Mobile client #{socket.assigns.client_id} updated bounds: #{inspect(bounds)}")

    {:reply, {:ok, %{bounds: bounds, message: "Bounds updated"}}, socket}
  end
end
