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

  ### Server -> Client

  - "packet" - New APRS packet within subscribed bounds
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

  require Logger

  @impl true
  def join("mobile:packets", _payload, socket) do
    Logger.info("Mobile client joined packets channel")
    {:ok, %{message: "Connected to APRS mobile channel"}, socket}
  end

  @impl true
  def handle_in("subscribe_bounds", %{"north" => north, "south" => south, "east" => east, "west" => west}, socket) do
    bounds = %{
      north: ensure_float(north),
      south: ensure_float(south),
      east: ensure_float(east),
      west: ensure_float(west)
    }

    # Validate bounds
    with :ok <- validate_bounds(bounds) do
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

      {:reply, {:ok, %{bounds: bounds, message: "Subscribed to packet stream"}}, socket}
    else
      {:error, reason} ->
        {:reply, {:error, %{message: reason}}, socket}
    end
  end

  @impl true
  def handle_in("update_bounds", %{"north" => north, "south" => south, "east" => east, "west" => west}, socket) do
    bounds = %{
      north: ensure_float(north),
      south: ensure_float(south),
      east: ensure_float(east),
      west: ensure_float(west)
    }

    # Validate bounds
    with :ok <- validate_bounds(bounds),
         true <- socket.assigns[:subscribed] do
      # Unsubscribe from old bounds
      if socket.assigns[:bounds] do
        Aprsme.StreamingPacketsPubSub.unsubscribe_from_bounds(self(), socket.assigns.bounds)
      end

      # Subscribe to new bounds
      Aprsme.StreamingPacketsPubSub.subscribe_to_bounds(self(), bounds)

      socket = assign(socket, :bounds, bounds)

      Logger.debug("Mobile client #{socket.assigns.client_id} updated bounds: #{inspect(bounds)}")

      {:reply, {:ok, %{bounds: bounds, message: "Bounds updated"}}, socket}
    else
      {:error, reason} ->
        {:reply, {:error, %{message: reason}}, socket}

      false ->
        {:reply, {:error, %{message: "Not subscribed. Call subscribe_bounds first."}}, socket}
    end
  end

  @impl true
  def handle_in("unsubscribe", _payload, socket) do
    if socket.assigns[:subscribed] && socket.assigns[:bounds] do
      Aprsme.StreamingPacketsPubSub.unsubscribe_from_bounds(self(), socket.assigns.bounds)

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
  def handle_info({:streaming_packet, packet}, socket) do
    # Convert packet to mobile-friendly format
    packet_data = build_mobile_packet(packet)

    # Push packet to mobile client
    push(socket, "packet", packet_data)

    {:noreply, socket}
  end

  # Clean up on terminate
  @impl true
  def terminate(_reason, socket) do
    if socket.assigns[:subscribed] && socket.assigns[:bounds] do
      Aprsme.StreamingPacketsPubSub.unsubscribe_from_bounds(self(), socket.assigns.bounds)
    end

    :ok
  end

  # Private functions

  defp validate_bounds(%{north: north, south: south, east: east, west: west}) do
    cond do
      not is_number(north) or not is_number(south) or not is_number(east) or not is_number(west) ->
        {:error, "All bounds must be numeric"}

      north < -90 or north > 90 or south < -90 or south > 90 ->
        {:error, "Latitude must be between -90 and 90"}

      east < -180 or east > 180 or west < -180 or west > 180 ->
        {:error, "Longitude must be between -180 and 180"}

      north <= south ->
        {:error, "North must be greater than south"}

      true ->
        :ok
    end
  end

  defp ensure_float(value) when is_integer(value), do: value * 1.0
  defp ensure_float(value) when is_float(value), do: value
  defp ensure_float(value) when is_binary(value), do: String.to_float(value)
  defp ensure_float(value), do: value

  defp build_mobile_packet(packet) do
    # Extract coordinates
    {lat, lon, _data_extended} = AprsmeWeb.MapLive.MapHelpers.get_coordinates(packet)

    # Build minimal packet data for mobile
    %{
      id: get_field(packet, :id),
      callsign: get_field(packet, :sender) || get_field(packet, :base_callsign),
      lat: to_float(lat),
      lng: to_float(lon),
      timestamp: get_field(packet, :received_at) |> format_timestamp(),
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
end
