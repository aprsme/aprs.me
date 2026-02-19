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
      class="info-map-container overflow-hidden h-full"
      style={"height: #{@height}; min-height: 320px;"}
      phx-hook="InfoMap"
      data-lat={@lat}
      data-lon={@lon}
      data-zoom={@zoom}
      data-callsign={@callsign}
      data-symbol-html={@symbol_html}
    >
      <div id={"#{@id}-loading"} class="h-full w-full bg-gray-100 dark:bg-gray-800 flex items-center justify-center">
        <svg
          class="animate-spin h-8 w-8 text-gray-400 dark:text-gray-500"
          xmlns="http://www.w3.org/2000/svg"
          fill="none"
          viewBox="0 0 24 24"
        >
          <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4" />
          <path
            class="opacity-75"
            fill="currentColor"
            d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
          />
        </svg>
      </div>
    </div>
    """
  end
end
