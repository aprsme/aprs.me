defmodule AprsmeWeb.MapLive.Navigation do
  @moduledoc """
  Handles geolocation, callsign tracking, and map navigation functionality.
  """

  import Phoenix.Component, only: [assign: 3]

  alias Aprsme.Packets
  alias AprsmeWeb.MapLive.UrlParams
  alias AprsmeWeb.MapLive.Utils
  alias Phoenix.LiveView
  alias Phoenix.LiveView.Socket

  @doc """
  Determine map location based on URL parameters and IP geolocation.
  Returns {map_center, zoom, should_skip_initial_url_update}.
  """
  @spec determine_map_location(map(), map()) :: {map(), integer(), boolean()}
  def determine_map_location(params, session) do
    {url_center, url_zoom} = UrlParams.parse_map_params(params)
    has_explicit_url_params = UrlParams.has_explicit_url_params?(params)

    case session["ip_geolocation"] do
      %{"lat" => lat, "lng" => lng} when is_number(lat) and is_number(lng) ->
        if has_explicit_url_params do
          {url_center, url_zoom, false}
        else
          {%{lat: lat, lng: lng}, 11, true}
        end

      _ ->
        {url_center, url_zoom, !has_explicit_url_params}
    end
  end

  @doc """
  Handle callsign tracking by finding the latest packet for the callsign.
  Returns {final_map_center, final_map_zoom}.
  """
  @spec handle_callsign_tracking(binary(), map(), integer(), boolean()) :: {map(), integer()}
  def handle_callsign_tracking(tracked_callsign, map_center, map_zoom, has_explicit_url_params) do
    if tracked_callsign != "" and not has_explicit_url_params do
      case Packets.get_latest_packet_for_callsign(tracked_callsign) do
        %{lat: lat, lon: lon} when is_number(lat) and is_number(lon) ->
          {%{lat: lat, lng: lon}, 12}

        _ ->
          {map_center, map_zoom}
      end
    else
      {map_center, map_zoom}
    end
  end

  @doc """
  Handle callsign search with validation.
  """
  @spec handle_callsign_search(binary(), Socket.t()) :: {:noreply, Socket.t()}
  def handle_callsign_search("", socket), do: {:noreply, socket}

  def handle_callsign_search(callsign, socket) do
    if Utils.valid_callsign?(callsign) do
      # Navigate to the new URL structure with the callsign as a path param
      {:noreply, LiveView.push_navigate(socket, to: "/#{callsign}")}
    else
      {:noreply, LiveView.put_flash(socket, :error, "Invalid callsign format")}
    end
  end

  @doc """
  Update map center and zoom to specific location.
  """
  @spec update_and_zoom_to_location(Socket.t(), float(), float(), integer()) :: Socket.t()
  def update_and_zoom_to_location(socket, lat, lng, zoom) do
    socket
    |> assign(:map_center, %{lat: lat, lng: lng})
    |> assign(:map_zoom, zoom)
    |> LiveView.push_event("zoom_to_location", %{lat: lat, lng: lng, zoom: zoom})
  end

  @doc """
  Zoom to current location stored in socket.
  """
  @spec zoom_to_current_location(Socket.t()) :: Socket.t()
  def zoom_to_current_location(socket) do
    LiveView.push_event(socket, "zoom_to_location", %{
      lat: socket.assigns.map_center.lat,
      lng: socket.assigns.map_center.lng,
      zoom: socket.assigns.map_zoom
    })
  end
end
