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
  @spec get_sprite_info(String.t() | nil, String.t() | nil) :: %{
          sprite_file: String.t(),
          background_position: String.t(),
          background_size: String.t()
        }
  def get_sprite_info(symbol_table, symbol_code) do
    compute_sprite_info(overlay_symbol?(symbol_table), symbol_table, symbol_code)
  end

  defp overlay_symbol?(table) when is_binary(table), do: String.match?(table, ~r/^[A-Z0-9]$/)
  defp overlay_symbol?(_), do: false

  defp compute_sprite_info(true, _symbol_table, symbol_code) do
    get_overlay_base_symbol_info(symbol_code)
  end

  defp compute_sprite_info(false, symbol_table, symbol_code) do
    symbol_table = normalize_symbol_table(symbol_table)
    symbol_code = normalize_symbol_code(symbol_code)

    table_id = get_table_id(symbol_table)
    sprite_file = "/aprs-symbols/aprs-symbols-128-#{table_id}@2x.png"

    symbol_code_ord = get_symbol_code_ord(symbol_code)
    index = symbol_code_ord - 33
    safe_index = max(0, min(index, 93))

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
  Gets sprite information for overlay symbols (A-Z, 0-9).
  These symbols display the base symbol from the overlay table.
  """
  @spec get_overlay_base_symbol_info(String.t()) :: %{
          sprite_file: String.t(),
          background_position: String.t(),
          background_size: String.t()
        }
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
  @spec get_overlay_base_table_id(String.t()) :: String.t()
  def get_overlay_base_table_id(_base_symbol_code) do
    # All overlay symbols are in the alternate table (1) per APRS specification
    # This includes digipeaters, diamonds, squares, arrows, etc.
    "1"
  end

  @doc """
  Gets sprite information for overlay characters (A-Z, 0-9).
  These are rendered from the overlay table.
  """
  @spec get_overlay_character_sprite_info(String.t()) :: %{
          sprite_file: String.t(),
          background_position: String.t(),
          background_size: String.t()
        }
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
  @spec normalize_symbol_table(String.t() | nil) :: String.t()
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
  @spec normalize_symbol_code(String.t() | nil) :: String.t()
  def normalize_symbol_code(nil), do: ">"
  def normalize_symbol_code(""), do: ">"
  def normalize_symbol_code(symbol_code), do: symbol_code

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
  @spec get_table_id(String.t()) :: String.t()
  def get_table_id("/"), do: "0"
  def get_table_id("\\"), do: "1"
  def get_table_id("]"), do: "2"
  def get_table_id(_), do: "0"

  @doc """
  Renders an APRS symbol as HTML for use in Leaflet markers.
  Returns HTML string that can be used as marker content.

  ## Examples

      iex> AprsmeWeb.AprsSymbol.render_marker_html("/", "_", "W1AW")
      "<div style=\"position: relative; width: 32px; height: 32px; display: flex; align-items: center;\">..."
  """
  @spec render_marker_html(String.t() | nil, String.t() | nil, String.t() | nil, integer()) :: String.t()
  def render_marker_html(symbol_table, symbol_code, callsign \\ nil, size \\ 32) do
    # For symbols without callsigns, use Cachex for better caching
    if is_nil(callsign) do
      cache_key = "symbol_html:#{symbol_table}:#{symbol_code}:#{size}"

      case Aprsme.Cache.get(:symbol_cache, cache_key) do
        {:ok, html} when not is_nil(html) ->
          html

        _ ->
          html = generate_marker_html(symbol_table, symbol_code, nil, size)
          # Cache for 1 hour since symbols don't change
          Aprsme.Cache.put(:symbol_cache, cache_key, html, ttl: Aprsme.Cache.to_timeout(hour: 1))
          html
      end
    else
      # For symbols with callsigns, generate directly (callsigns are dynamic)
      generate_marker_html(symbol_table, symbol_code, callsign, size)
    end
  end

  @spec generate_marker_html(String.t() | nil, String.t() | nil, String.t() | nil, integer()) :: String.t()
  defp generate_marker_html(symbol_table, symbol_code, callsign, size) do
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
  @spec render_style(String.t() | nil, String.t() | nil, integer()) :: String.t()
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
  @spec extract_from_packet(map()) :: {String.t(), String.t()}
  def extract_from_packet(packet) do
    symbol_table_id = get_packet_field(packet, :symbol_table_id, "/")
    symbol_code = get_packet_field(packet, :symbol_code, ">")

    {symbol_table_id, symbol_code}
  end

  # Helper function to safely extract a value from a packet or data_extended map
  @spec get_packet_field(map(), atom(), any()) :: any()
  defp get_packet_field(packet, field, default) do
    data_extended = Map.get(packet, :data_extended, Map.get(packet, "data_extended", %{})) || %{}

    Map.get(packet, field) ||
      Map.get(packet, to_string(field)) ||
      Map.get(data_extended, field) ||
      Map.get(data_extended, to_string(field)) ||
      default
  end

  defp get_symbol_code_ord(symbol_code) do
    c = symbol_code |> String.to_charlist() |> List.first()
    if is_integer(c), do: c, else: 63
  end
end
