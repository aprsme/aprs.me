defmodule AprsmeWeb.PacketsLive.Index do
  @moduledoc false
  use AprsmeWeb, :live_view
  use Gettext, backend: AprsmeWeb.Gettext

  alias Aprsme.EncodingUtils
  alias AprsmeWeb.Endpoint

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Endpoint.subscribe("aprs_messages")
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")
    end

    {:ok, assign(socket, :packets, [])}
  end

  @impl true
  def handle_info(%{event: "packet", payload: payload}, socket) do
    # Sanitize the packet to prevent JSON encoding errors
    sanitized_payload = EncodingUtils.sanitize_packet(payload)
    packets = Enum.take([sanitized_payload | socket.assigns.packets], 100)
    socket = assign(socket, :packets, packets)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:postgres_packet, payload}, socket) do
    sanitized_payload = EncodingUtils.sanitize_packet(payload)
    packets = Enum.take([sanitized_payload | socket.assigns.packets], 100)
    socket = assign(socket, :packets, packets)
    {:noreply, socket}
  end

  @doc """
  Extract a coordinate (:lat or :lon) from a packet, checking multiple sources:
  1. Direct :lat/:lon keys
  2. Packet struct with PostGIS location
  3. data_extended map with :latitude/:longitude keys
  """
  def extract_coordinate(packet, which) when which in [:lat, :lon] do
    direct_key = which
    extended_key = if which == :lat, do: :latitude, else: :longitude

    Map.get(packet, direct_key) ||
      extract_from_location(packet, which) ||
      extract_from_data_extended(packet, extended_key)
  end

  @doc """
  Format a coordinate value for display with up to 6 decimal places.
  """
  def format_coordinate(nil), do: ""

  def format_coordinate(value) when is_float(value) do
    "~.6f" |> :io_lib.format([value]) |> List.to_string()
  end

  def format_coordinate(value) when is_binary(value) do
    Regex.replace(~r/(\d+\.\d{1,6})\d*/, value, "\\1")
  end

  def format_coordinate(value), do: to_string(value)

  defp extract_from_location(%Aprsme.Packet{location: %Geo.Point{coordinates: {_lon, lat}}}, :lat), do: lat
  defp extract_from_location(%Aprsme.Packet{location: %Geo.Point{coordinates: {lon, _lat}}}, :lon), do: lon
  defp extract_from_location(_, _), do: nil

  defp extract_from_data_extended(packet, key) do
    case Map.get(packet, :data_extended) do
      %{} = data -> Map.get(data, key) || Map.get(data, to_string(key))
      _ -> nil
    end
  end
end
