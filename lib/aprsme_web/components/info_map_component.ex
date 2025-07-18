defmodule AprsmeWeb.Components.InfoMapComponent do
  @moduledoc """
  A simple map component for showing a single station's location on the info page.
  """
  use Phoenix.Component

  @doc """
  Renders a small map showing a single station's position.
  """
  attr :id, :string, default: "info-map"
  attr :lat, :float, required: true
  attr :lon, :float, required: true
  attr :callsign, :string, required: true
  attr :symbol_html, :string, default: nil
  attr :height, :string, default: "300px"
  attr :zoom, :integer, default: 13

  def info_map(assigns) do
    ~H"""
    <div
      id={@id}
      class="info-map-container rounded-lg overflow-hidden shadow-md"
      style={"height: #{@height};"}
      phx-hook="InfoMap"
      data-lat={@lat}
      data-lon={@lon}
      data-zoom={@zoom}
      data-callsign={@callsign}
      data-symbol-html={@symbol_html}
    >
      <div class="h-full w-full bg-base-200 flex items-center justify-center">
        <span class="loading loading-spinner loading-lg"></span>
      </div>
    </div>
    """
  end
end
