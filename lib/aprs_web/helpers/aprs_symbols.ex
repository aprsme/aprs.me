defmodule AprsWeb.Helpers.AprsSymbols do
  @moduledoc """
  Helper functions for working with APRS symbols.

  APRS symbols are organized in sprite sheets:
  - Primary table (symbol_table_id = "/"): aprs-symbols-24-0.png
  - Secondary table (symbol_table_id = "\\"): aprs-symbols-24-1.png
  - Overlay characters: aprs-symbols-24-2.png

  Each sprite sheet is a 16x6 grid (96 symbols total).
  Symbol positioning is based on ASCII code of the symbol_code.
  """

  @doc """
  Get the sprite sheet filename for a given symbol table ID.

  ## Examples

      iex> AprsWeb.Helpers.AprsSymbols.get_sprite_filename("/")
      "aprs-symbols-24-0.png"

      iex> AprsWeb.Helpers.AprsSymbols.get_sprite_filename("\\")
      "aprs-symbols-24-1.png"
  """
  @spec get_sprite_filename(String.t()) :: String.t()
  def get_sprite_filename(symbol_table_id) do
    case symbol_table_id do
      "/" -> "aprs-symbols-24-0.png"
      "\\" -> "aprs-symbols-24-1.png"
      # Default to primary table
      _ -> "aprs-symbols-24-0.png"
    end
  end

  @doc """
  Get the high-resolution sprite sheet filename for retina displays.
  """
  @spec get_sprite_filename_2x(String.t()) :: String.t()
  def get_sprite_filename_2x(symbol_table_id) do
    case symbol_table_id do
      "/" -> "aprs-symbols-24-0@2x.png"
      "\\" -> "aprs-symbols-24-1@2x.png"
      _ -> "aprs-symbols-24-0@2x.png"
    end
  end

  @doc """
  Calculate the CSS background position for a symbol in the sprite sheet.

  The sprite sheet is organized as a 16x6 grid.
  ASCII codes 32-127 map to positions 0-95.

  ## Examples

      iex> AprsWeb.Helpers.AprsSymbols.get_symbol_position(">")
      {-1440, 0}  # ASCII 62, position 30 -> column 14, row 1

      iex> AprsWeb.Helpers.AprsSymbols.get_symbol_position("!")
      {-216, 0}   # ASCII 33, position 1 -> column 1, row 0
  """
  @spec get_symbol_position(String.t() | integer()) :: {integer(), integer()}
  def get_symbol_position(symbol_code) when is_binary(symbol_code) do
    # Get first character if string
    char_code = symbol_code |> String.first() |> String.to_charlist() |> List.first()
    get_symbol_position_by_ascii(char_code)
  end

  def get_symbol_position(symbol_code) when is_integer(symbol_code) do
    get_symbol_position_by_ascii(symbol_code)
  end

  defp get_symbol_position_by_ascii(ascii_code) do
    # ASCII 32 (space) is position 0, ASCII 127 (DEL) is position 95
    position = ascii_code - 32

    # Clamp position to valid range 0-95
    position = max(0, min(95, position))

    # Calculate row and column (16 symbols per row)
    col = rem(position, 16)
    row = div(position, 16)

    # Each symbol is 24x24 pixels
    x = -col * 24
    y = -row * 24

    {x, y}
  end

  @doc """
  Generate CSS style for displaying an APRS symbol using sprite positioning.

  ## Examples

      iex> AprsWeb.Helpers.AprsSymbols.symbol_css_style("/", ">")
      "background-image: url('/aprs-symbols/aprs-symbols-24-0.png'); background-position: -1440px 0px; width: 24px; height: 24px;"
  """
  @spec symbol_css_style(String.t(), String.t() | integer()) :: String.t()
  def symbol_css_style(symbol_table_id, symbol_code) do
    sprite_file = get_sprite_filename(symbol_table_id)
    {x, y} = get_symbol_position(symbol_code)

    "background-image: url('/aprs-symbols/#{sprite_file}'); " <>
      "background-position: #{x}px #{y}px; " <>
      "width: 24px; height: 24px; " <>
      "background-repeat: no-repeat;"
  end

  @doc """
  Generate HTML for an APRS symbol with proper sprite positioning.
  """
  @spec symbol_html(String.t(), String.t() | integer(), keyword()) :: {:safe, String.t()}
  def symbol_html(symbol_table_id, symbol_code, opts \\ []) do
    css_class = Keyword.get(opts, :class, "aprs-symbol")
    extra_style = Keyword.get(opts, :style, "")

    base_style = symbol_css_style(symbol_table_id, symbol_code)
    full_style = if extra_style == "", do: base_style, else: base_style <> " " <> extra_style

    Phoenix.HTML.raw(~s(<div class="#{css_class}" style="#{full_style}"></div>))
  end

  @doc """
  Get a data URL for the symbol that can be used in JavaScript/Leaflet.
  This creates a small canvas with just the symbol extracted from the sprite.
  """
  @spec get_symbol_data_attributes(String.t(), String.t() | integer()) :: map()
  def get_symbol_data_attributes(symbol_table_id, symbol_code) do
    sprite_file = get_sprite_filename(symbol_table_id)
    sprite_file_2x = get_sprite_filename_2x(symbol_table_id)
    {x, y} = get_symbol_position(symbol_code)

    %{
      "data-sprite" => sprite_file,
      "data-sprite-2x" => sprite_file_2x,
      "data-pos-x" => x,
      "data-pos-y" => y,
      "data-symbol-table" => symbol_table_id,
      "data-symbol-code" => symbol_code
    }
  end

  @doc """
  Get the default symbol for unknown or invalid symbols.
  """
  @spec default_symbol() :: {String.t(), String.t()}
  def default_symbol do
    # Car icon as default
    {"/", ">"}
  end

  @doc """
  Validate if a symbol table ID and code combination is valid.
  """
  @spec valid_symbol?(any(), any()) :: boolean()
  def valid_symbol?(symbol_table_id, symbol_code) do
    case {symbol_table_id, symbol_code} do
      {table, code} when table in ["/", "\\"] and is_binary(code) and byte_size(code) > 0 ->
        char_code = code |> String.first() |> String.to_charlist() |> List.first()
        char_code >= 32 and char_code <= 127

      _ ->
        false
    end
  end

  @doc """
  Get a human-readable description for common APRS symbols.
  This is a subset of common symbols - for a complete list, you'd want to
  reference the official APRS symbol specification.
  """
  @spec symbol_description(String.t(), String.t()) :: String.t()
  def symbol_description(symbol_table_id, symbol_code) do
    case {symbol_table_id, symbol_code} do
      # Primary Table (/)
      {"/", "!"} -> "Police/Sheriff"
      {"/", "\""} -> "Reserved"
      {"/", "#"} -> "Digipeater"
      {"/", "$"} -> "Phone"
      {"/", "%"} -> "DX Cluster"
      {"/", "&"} -> "HF Gateway"
      {"/", "'"} -> "Small Aircraft"
      {"/", "("} -> "Mobile Satellite Station"
      {"/", ")"} -> "Wheelchair"
      {"/", "*"} -> "Snowmobile"
      {"/", "+"} -> "Red Cross"
      {"/", ","} -> "Boy Scout"
      {"/", "-"} -> "House"
      {"/", "."} -> "X"
      {"/", "/"} -> "Position"
      {"/", "0"} -> "Circle"
      {"/", "1"} -> "Circle"
      {"/", "2"} -> "Circle"
      {"/", "3"} -> "Circle"
      {"/", "4"} -> "Circle"
      {"/", "5"} -> "Circle"
      {"/", "6"} -> "Circle"
      {"/", "7"} -> "Circle"
      {"/", "8"} -> "Circle"
      {"/", "9"} -> "Circle"
      {"/", ":"} -> "Fire Department"
      {"/", ";"} -> "Campground"
      {"/", "<"} -> "Motorcycle"
      {"/", "="} -> "Rail Engine"
      {"/", ">"} -> "Car"
      {"/", "?"} -> "File Server"
      {"/", "@"} -> "HC Future"
      {"/", "A"} -> "Aid Station"
      {"/", "B"} -> "BBS"
      {"/", "C"} -> "Canoe"
      {"/", "D"} -> "Reserved"
      {"/", "E"} -> "Eyeball"
      {"/", "F"} -> "Tractor"
      {"/", "G"} -> "Grid Square"
      {"/", "H"} -> "Hotel"
      {"/", "I"} -> "TCP/IP"
      {"/", "J"} -> "Phone"
      {"/", "K"} -> "School"
      {"/", "L"} -> "PC User"
      {"/", "M"} -> "MacAPRS"
      {"/", "N"} -> "NTS Station"
      {"/", "O"} -> "Balloon"
      {"/", "P"} -> "Police"
      {"/", "Q"} -> "TBD"
      {"/", "R"} -> "Recreational Vehicle"
      {"/", "S"} -> "Shuttle"
      {"/", "T"} -> "SSTV"
      {"/", "U"} -> "Bus"
      {"/", "V"} -> "ATV"
      {"/", "W"} -> "National Weather Service"
      {"/", "X"} -> "Helo"
      {"/", "Y"} -> "Yacht"
      {"/", "Z"} -> "WinAPRS"
      {"/", "["} -> "Jogger"
      {"/", "\\"} -> "Triangle"
      {"/", "]"} -> "PBBS"
      {"/", "^"} -> "Aircraft"
      {"/", "_"} -> "Weather Station"
      {"/", "`"} -> "Dish Antenna"
      {"/", "a"} -> "Ambulance"
      {"/", "b"} -> "Bike"
      {"/", "c"} -> "Incident Command Post"
      {"/", "d"} -> "Fire Depts"
      {"/", "e"} -> "Horse"
      {"/", "f"} -> "Fire Truck"
      {"/", "g"} -> "Glider"
      {"/", "h"} -> "Hospital"
      {"/", "i"} -> "IOTA"
      {"/", "j"} -> "Jeep"
      {"/", "k"} -> "Truck"
      {"/", "l"} -> "Laptop"
      {"/", "m"} -> "Mic-E"
      {"/", "n"} -> "Node"
      {"/", "o"} -> "EOC"
      {"/", "p"} -> "Dog"
      {"/", "q"} -> "Grid"
      {"/", "r"} -> "Repeater"
      {"/", "s"} -> "Ship"
      {"/", "t"} -> "Truck Stop"
      {"/", "u"} -> "Truck"
      {"/", "v"} -> "Van"
      {"/", "w"} -> "Water Station"
      {"/", "x"} -> "X-APRS"
      {"/", "y"} -> "Yagi"
      {"/", "z"} -> "Shelter"
      # Secondary Table (\)
      {"\\", "!"} -> "Emergency"
      {"\\", "\""} -> "Reserved"
      {"\\", "#"} -> "Digipeater"
      {"\\", "$"} -> "Bank"
      {"\\", "%"} -> "Reserved"
      {"\\", "&"} -> "Reserved"
      {"\\", "'"} -> "Reserved"
      {"\\", "("} -> "Reserved"
      {"\\", ")"} -> "Reserved"
      {"\\", "*"} -> "Reserved"
      {"\\", "+"} -> "Reserved"
      {"\\", ","} -> "Reserved"
      {"\\", "-"} -> "Reserved"
      {"\\", "."} -> "Reserved"
      {"\\", "/"} -> "Triangle"
      {"\\", "0"} -> "Reserved"
      {"\\", "1"} -> "Reserved"
      {"\\", "2"} -> "Reserved"
      {"\\", "3"} -> "Reserved"
      {"\\", "4"} -> "Reserved"
      {"\\", "5"} -> "Reserved"
      {"\\", "6"} -> "Reserved"
      {"\\", "7"} -> "Reserved"
      {"\\", "8"} -> "Reserved"
      {"\\", "9"} -> "Reserved"
      {"\\", ":"} -> "Reserved"
      {"\\", ";"} -> "Reserved"
      {"\\", "<"} -> "Reserved"
      {"\\", "="} -> "Reserved"
      {"\\", ">"} -> "Car (alternate)"
      {"\\", "?"} -> "Reserved"
      {"\\", "@"} -> "Reserved"
      {"\\", "A"} -> "Reserved"
      {"\\", "B"} -> "Reserved"
      {"\\", "C"} -> "Reserved"
      {"\\", "D"} -> "Reserved"
      {"\\", "E"} -> "Reserved"
      {"\\", "F"} -> "Reserved"
      {"\\", "G"} -> "Reserved"
      {"\\", "H"} -> "Reserved"
      {"\\", "I"} -> "Reserved"
      {"\\", "J"} -> "Reserved"
      {"\\", "K"} -> "Reserved"
      {"\\", "L"} -> "Reserved"
      {"\\", "M"} -> "Reserved"
      {"\\", "N"} -> "Reserved"
      {"\\", "O"} -> "Reserved"
      {"\\", "P"} -> "Reserved"
      {"\\", "Q"} -> "Reserved"
      {"\\", "R"} -> "Reserved"
      {"\\", "S"} -> "Reserved"
      {"\\", "T"} -> "Reserved"
      {"\\", "U"} -> "Reserved"
      {"\\", "V"} -> "Reserved"
      {"\\", "W"} -> "Reserved"
      {"\\", "X"} -> "Reserved"
      {"\\", "Y"} -> "Reserved"
      {"\\", "Z"} -> "Reserved"
      {"\\", "["} -> "Reserved"
      {"\\", "\\"} -> "Reserved"
      {"\\", "]"} -> "Reserved"
      {"\\", "^"} -> "Reserved"
      {"\\", "_"} -> "Reserved"
      {"\\", "`"} -> "Reserved"
      {"\\", "a"} -> "Reserved"
      {"\\", "b"} -> "Reserved"
      {"\\", "c"} -> "Reserved"
      {"\\", "d"} -> "Reserved"
      {"\\", "e"} -> "Reserved"
      {"\\", "f"} -> "Reserved"
      {"\\", "g"} -> "Reserved"
      {"\\", "h"} -> "Reserved"
      {"\\", "i"} -> "Reserved"
      {"\\", "j"} -> "Reserved"
      {"\\", "k"} -> "Reserved"
      {"\\", "l"} -> "Reserved"
      {"\\", "m"} -> "Reserved"
      {"\\", "n"} -> "Reserved"
      {"\\", "o"} -> "Reserved"
      {"\\", "p"} -> "Reserved"
      {"\\", "q"} -> "Reserved"
      {"\\", "r"} -> "Reserved"
      {"\\", "s"} -> "Reserved"
      {"\\", "t"} -> "Reserved"
      {"\\", "u"} -> "Reserved"
      {"\\", "v"} -> "Reserved"
      {"\\", "w"} -> "Reserved"
      {"\\", "x"} -> "Reserved"
      {"\\", "y"} -> "Reserved"
      {"\\", "z"} -> "Reserved"
      # Unknown symbol
      _ -> "Unknown symbol"
    end
  end
end
