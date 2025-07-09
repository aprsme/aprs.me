defmodule AprsmeWeb.SymbolRenderer do
  @moduledoc """
  Server-side APRS symbol rendering component.

  This module handles the rendering of APRS symbols using the hessu/aprs-symbols sprite files.
  It provides a centralized way to render symbols that can be used across all LiveView pages.
  """

  use Phoenix.Component

  @doc """
  Renders an APRS symbol with the correct sprite positioning.

  ## Examples

      <.symbol symbol_table="/" symbol_code="_" size={32} callsign="W1AW" />
      <.symbol symbol_table="D" symbol_code="&" size={64} />
  """
  attr :symbol_table, :string, required: true, doc: "APRS symbol table identifier (/, \\, or overlay)"
  attr :symbol_code, :string, required: true, doc: "APRS symbol code character"
  attr :size, :integer, default: 32, doc: "Display size in pixels"
  attr :callsign, :string, default: nil, doc: "Optional callsign to display next to symbol"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :title, :string, default: nil, doc: "Optional title/tooltip text"

  def symbol(assigns) do
    # Get the sprite file and position for this symbol
    sprite_info = get_sprite_info(assigns.symbol_table, assigns.symbol_code)

    assigns =
      assign(assigns,
        sprite_file: sprite_info.sprite_file,
        background_position: sprite_info.background_position,
        background_size: sprite_info.background_size,
        symbol_title: assigns.title || "#{assigns.symbol_table}#{assigns.symbol_code}"
      )

    ~H"""
    <div
      class={["aprs-symbol-container", @class]}
      style={"position: relative; display: inline-block; width: #{@size}px; height: #{@size}px;"}
    >
      <div
        class="aprs-symbol"
        style={"
          width: #{@size}px;
          height: #{@size}px;
          background-image: url(#{@sprite_file});
          background-position: #{@background_position};
          background-size: #{@background_size};
          background-repeat: no-repeat;
          image-rendering: pixelated;
        "}
        title={@symbol_title}
      >
      </div>
      <%= if @callsign do %>
        <div
          class="aprs-callsign-label"
          style="
            position: absolute;
            left: #{@size + 4}px;
            top: 50%;
            transform: translateY(-50%);
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            font-size: 11px;
            font-weight: 600;
            color: #1e40af;
            background-color: rgba(255, 255, 255, 0.9);
            padding: 1px 4px;
            border-radius: 3px;
            white-space: nowrap;
            border: 1px solid rgba(30, 64, 175, 0.3);
            box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
            pointer-events: none;
            z-index: 1000;
          "
        >
          {@callsign}
        </div>
      <% end %>
    </div>
    """
  end

  @doc """
  Gets sprite information for a given symbol table and code.
  Returns a map with sprite_file, background_position, and background_size.
  """
  def get_sprite_info(symbol_table, symbol_code) do
    AprsmeWeb.AprsSymbol.get_sprite_info(symbol_table, symbol_code)
  end


  @doc """
  Renders an APRS symbol for use in Leaflet markers.
  Returns HTML string that can be used as marker content.
  """
  def render_marker_symbol(symbol_table, symbol_code, callsign \\ nil, size \\ 32) do
    AprsmeWeb.AprsSymbol.render_marker_html(symbol_table, symbol_code, callsign, size)
  end
end
