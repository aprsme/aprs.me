defmodule AprsmeWeb.AprsSymbol do
  @moduledoc """
  Shared library for APRS symbol handling and rendering.

  This module provides centralized functions for:
  - Symbol table and code normalization
  - Sprite file mapping
  - Symbol positioning calculations
  - HTML generation for symbols

  All APRS symbol logic should use this module to ensure consistency
  across the application.
  """

  @doc """
  Gets sprite information for a given symbol table and code.
  Returns a map with sprite_file, background_position, and background_size.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.get_sprite_info("/", "_")
      %{
        sprite_file: "/aprs-symbols/aprs-symbols-128-0@2x.png",
        background_position: "-352px -32px",
        background_size: "512px 192px"
      }
  """
  def get_sprite_info(symbol_table, symbol_code) do
    # For overlay symbols (A-Z, 0-9), display the base symbol with overlay
    if symbol_table && String.match?(symbol_table, ~r/^[A-Z0-9]$/) do
      # Use the base symbol from the overlay table, not the overlay character itself
      get_overlay_base_symbol_info(symbol_code)
    else
      # Normal symbol table processing
      symbol_table = normalize_symbol_table(symbol_table)
      symbol_code = normalize_symbol_code(symbol_code)

      # Map symbol table to sprite file ID
      table_id = get_table_id(symbol_table)

      sprite_file = "/aprs-symbols/aprs-symbols-128-#{table_id}@2x.png"

      # Get symbol position using ASCII-based calculation
      symbol_code_ord =
        symbol_code
        |> String.to_charlist()
        |> List.first()
        |> then(fn c -> if is_integer(c), do: c, else: 63 end)

      index = symbol_code_ord - 33
      safe_index = max(0, min(index, 93))

      # Calculate positioning for 16-column grid
      column = rem(safe_index, 16)
      row = div(safe_index, 16)
      x = -column * 128
      y = -row * 128

      %{
        sprite_file: sprite_file,
        background_position: "#{x / 4}px #{y / 4}px",
        background_size: "512px 192px"
      }
    end
  end

  @doc """
  Gets sprite information for overlay symbols (A-Z, 0-9).
  These symbols display the base symbol from the overlay table.
  """
  def get_overlay_base_symbol_info(base_symbol_code) do
    # Some overlay base symbols are in the alternate table (1), others in overlay table (2)
    # Check which table to use based on the symbol code
    table_id = get_overlay_base_table_id(base_symbol_code)
    sprite_file = "/aprs-symbols/aprs-symbols-128-#{table_id}@2x.png"

    # Get position of the base symbol in the appropriate table
    base_symbol_ord =
      base_symbol_code
      |> String.to_charlist()
      |> List.first()
      |> then(fn c -> if is_integer(c), do: c, else: 63 end)

    index = base_symbol_ord - 33
    safe_index = max(0, min(index, 93))

    # Calculate positioning for 16-column grid
    column = rem(safe_index, 16)
    row = div(safe_index, 16)
    x = -column * 128
    y = -row * 128

    %{
      sprite_file: sprite_file,
      background_position: "#{x / 4}px #{y / 4}px",
      background_size: "512px 192px"
    }
  end

  @doc """
  Determines which sprite table to use for overlay base symbols.
  Some symbols are in the alternate table (1), others in overlay table (2).
  """
  def get_overlay_base_table_id(base_symbol_code) do
    # Map symbols to the correct sprite table based on APRS specification
    # Most overlay symbols are in the alternate table (1)
    case base_symbol_code do
      # Digipeater symbols are often in the alternate table (1) and have colored backgrounds
      # Digipeater - green star background
      "#" -> "1"
      # Diamond shape - APRS overlay symbol (alternate table)
      "a" -> "1"
      # Square shape - APRS overlay symbol (alternate table)
      "A" -> "1"
      # Diamond shape - alternate table
      "&" -> "1"
      # Arrow symbols
      ">" -> "1"
      "<" -> "1"
      "^" -> "1"
      "v" -> "1"
      # Black square background - alternate table
      "i" -> "1"
      # Most other symbols that can be overlaid are in the alternate table
      _ -> "1"
    end
  end

  @doc """
  Gets sprite information for overlay characters (A-Z, 0-9).
  These are rendered from the overlay table.
  """
  def get_overlay_character_sprite_info(overlay_char) do
    # Use overlay table (table 2) for the overlay character
    sprite_file = "/aprs-symbols/aprs-symbols-128-2@2x.png"

    # Get position of the overlay character in the overlay table
    overlay_char_ord =
      overlay_char
      |> String.to_charlist()
      |> List.first()
      |> then(fn c -> if is_integer(c), do: c, else: 63 end)

    index = overlay_char_ord - 33
    safe_index = max(0, min(index, 93))

    # Calculate positioning for 16-column grid
    column = rem(safe_index, 16)
    row = div(safe_index, 16)
    x = -column * 128
    y = -row * 128

    %{
      sprite_file: sprite_file,
      background_position: "#{x / 4}px #{y / 4}px",
      background_size: "512px 192px"
    }
  end

  @doc """
  Normalizes a symbol table identifier.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.normalize_symbol_table("/")
      "/"
      
      iex> AprsmeWeb.AprsSymbol.normalize_symbol_table("A")
      "]"
      
      iex> AprsmeWeb.AprsSymbol.normalize_symbol_table("invalid")
      "/"
  """
  def normalize_symbol_table(symbol_table) do
    cond do
      symbol_table in ["/", "\\", "]"] -> symbol_table
      symbol_table && String.match?(symbol_table, ~r/^[A-Z0-9]$/) -> "]"
      true -> "/"
    end
  end

  @doc """
  Normalizes a symbol code.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.normalize_symbol_code("_")
      "_"
      
      iex> AprsmeWeb.AprsSymbol.normalize_symbol_code(nil)
      ">"
      
      iex> AprsmeWeb.AprsSymbol.normalize_symbol_code("")
      ">"
  """
  def normalize_symbol_code(symbol_code) do
    if symbol_code && symbol_code != "", do: symbol_code, else: ">"
  end

  @doc """
  Maps a symbol table to its sprite file ID.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.get_table_id("/")
      "0"
      
      iex> AprsmeWeb.AprsSymbol.get_table_id("\\")
      "1"
      
      iex> AprsmeWeb.AprsSymbol.get_table_id("]")
      "2"
  """
  def get_table_id(symbol_table) do
    case symbol_table do
      # Primary table
      "/" -> "0"
      # Alternate table
      "\\" -> "1"
      # Overlay table (A-Z, 0-9)
      "]" -> "2"
      # Default to primary table
      _ -> "0"
    end
  end

  @doc """
  Renders an APRS symbol as HTML for use in Leaflet markers.
  Returns HTML string that can be used as marker content.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.render_marker_html("/", "_", "W1AW")
      "<div style=\"position: relative; width: 32px; height: 32px; display: flex; align-items: center;\">..."
  """
  def render_marker_html(symbol_table, symbol_code, callsign \\ nil, size \\ 32) do
    sprite_info = get_sprite_info(symbol_table, symbol_code)

    # Check if this is an overlay symbol
    is_overlay = symbol_table && String.match?(symbol_table, ~r/^[A-Z0-9]$/)

    symbol_html =
      if is_overlay do
        # For overlay symbols, we need both the base symbol background and the overlay character
        overlay_sprite_info = get_overlay_character_sprite_info(symbol_table)

        """
        <div style="
          position: relative;
          width: #{size}px;
          height: #{size}px;
          background-image: url(#{overlay_sprite_info.sprite_file}), url(#{sprite_info.sprite_file});
          background-position: #{overlay_sprite_info.background_position}, #{sprite_info.background_position};
          background-size: #{overlay_sprite_info.background_size}, #{sprite_info.background_size};
          background-repeat: no-repeat, no-repeat;
          image-rendering: pixelated;
        " title="#{symbol_table}#{symbol_code}">
        </div>
        """
      else
        """
        <div style="
          width: #{size}px;
          height: #{size}px;
          background-image: url(#{sprite_info.sprite_file});
          background-position: #{sprite_info.background_position};
          background-size: #{sprite_info.background_size};
          background-repeat: no-repeat;
          image-rendering: pixelated;
        " title="#{symbol_table}#{symbol_code}"></div>
        """
      end

    if callsign do
      """
      <div style="position: relative; width: #{size}px; height: #{size}px; display: flex; align-items: center;">
        #{symbol_html}
        <div style="
          position: absolute;
          left: #{size + 4}px;
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
          pointer-events: auto;
          z-index: 1000;
        ">#{callsign}</div>
      </div>
      """
    else
      symbol_html
    end
  end

  @doc """
  Renders an APRS symbol as a style string for use in templates.
  Returns a CSS style string that can be used directly in HTML.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.render_style("/", "_", 32)
      "width: 32px; height: 32px; background-image: url(/aprs-symbols/aprs-symbols-128-0@2x.png); ..."
  """
  def render_style(symbol_table, symbol_code, size \\ 32) do
    sprite_info = get_sprite_info(symbol_table, symbol_code)

    "width: #{size}px; height: #{size}px; background-image: url(#{sprite_info.sprite_file}); background-position: #{sprite_info.background_position}; background-size: #{sprite_info.background_size}; background-repeat: no-repeat; image-rendering: pixelated; opacity: 1.0; display: inline-block; vertical-align: middle; margin-bottom: -6px;"
  end

  @doc """
  Extracts symbol information from a packet with fallbacks.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.extract_from_packet(%{symbol_table_id: "/", symbol_code: "_"})
      {"/", "_"}
      
      iex> AprsmeWeb.AprsSymbol.extract_from_packet(%{})
      {"/", ">"}
  """
  def extract_from_packet(packet) do
    symbol_table_id = get_packet_field(packet, :symbol_table_id, "/")
    symbol_code = get_packet_field(packet, :symbol_code, ">")

    {symbol_table_id, symbol_code}
  end

  # Helper function to safely extract a value from a packet or data_extended map
  defp get_packet_field(packet, field, default) do
    data_extended = Map.get(packet, :data_extended, Map.get(packet, "data_extended", %{})) || %{}

    Map.get(packet, field) ||
      Map.get(packet, to_string(field)) ||
      Map.get(data_extended, field) ||
      Map.get(data_extended, to_string(field)) ||
      default
  end
end
