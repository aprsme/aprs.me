# APRS Symbol Implementation

This document describes the implementation of APRS icons/symbols in the LiveView-based map system, replacing the generic colored dots with proper APRS symbols.

## Overview

The APRS.me application now displays proper APRS symbols instead of generic dots for packet markers on the map. This implementation uses the high-resolution APRS symbol set from [hessu/aprs-symbols](https://github.com/hessu/aprs-symbols).

## Map Versions

The application now has three map implementations:

- `/` - **Default LiveView Map** - Minimal LiveView-based map with APRS symbols (recommended)
- `/enhanced` - **Enhanced LiveView Map** - Feature-rich LiveView map with additional controls
- `/old` - **Legacy Map** - Original JavaScript-heavy implementation (deprecated)

## APRS Symbol System

### Symbol Tables

APRS uses two main symbol tables:

- **Primary Table** (`/`): `aprs-symbols-24-0.png` - Standard symbols
- **Secondary Table** (`\`): `aprs-symbols-24-1.png` - Alternate symbols
- **Overlay Characters**: `aprs-symbols-24-2.png` - Overlay digits and letters

### Symbol Structure

Each symbol is identified by:
- **Symbol Table ID**: `/` (primary) or `\` (secondary)
- **Symbol Code**: ASCII character (33-126) representing the symbol position

### Symbol Positioning

Symbols are arranged in a 16×6 grid (96 symbols total) in each sprite sheet:
- Position = ASCII code - 32
- Column = Position % 16
- Row = Position ÷ 16
- Each symbol is 24×24 pixels

## Implementation Details

### Backend Components

#### AprsWeb.Helpers.AprsSymbols

Helper module for APRS symbol operations:

```elixir
# Get sprite sheet filename
AprsSymbols.get_sprite_filename("/")  # → "aprs-symbols-24-0.png"

# Calculate symbol position
AprsSymbols.get_symbol_position(">")  # → {-336, -24}

# Generate CSS for symbol display
AprsSymbols.symbol_css_style("/", ">")

# Get human-readable description
AprsSymbols.symbol_description("/", ">")  # → "Car"
```

#### Packet Processing

Modified `build_packet_data/1` in `MapLive.Index`:
- Extracts symbol information from packet data
- Validates symbols and provides defaults
- Includes coordinates and symbol metadata for frontend

#### Struct Conversion

Enhanced `struct_to_map/1` in `Aprs.Is`:
- Recursively converts nested structs to maps
- Preserves type information for proper data extraction
- Handles MicE packets specially

### Frontend Components

#### JavaScript Updates

Updated `minimal_map.js`:
- Added handlers for `new_packet` and `historical_packet` events
- Modified `createMarkerIcon()` to use sprite-based symbols
- Added popup content generation with symbol descriptions

#### Symbol Rendering

Symbols are displayed using CSS sprites:
- Background image points to appropriate sprite sheet
- Background position calculated from symbol code
- High-DPI support with @2x variants
- Opacity adjustment for historical markers

### Asset Files

Downloaded APRS symbol files in `priv/static/aprs-symbols/`:
- `aprs-symbols-24-0.png` - Primary table (24×24)
- `aprs-symbols-24-1.png` - Secondary table (24×24)
- `aprs-symbols-24-0@2x.png` - Primary table (48×48, retina)
- `aprs-symbols-24-1@2x.png` - Secondary table (48×48, retina)
- `aprs-symbols-24-2.png` - Overlay characters (24×24)
- `aprs-symbols-24-2@2x.png` - Overlay characters (48×48, retina)

## Common APRS Symbols

| Symbol Code | Table | Description |
|-------------|-------|-------------|
| `>`         | `/`   | Car         |
| `k`         | `/`   | Truck       |
| `j`         | `/`   | Jeep        |
| `f`         | `/`   | Fire truck  |
| `a`         | `/`   | Ambulance   |
| `b`         | `/`   | Bike        |
| `^`         | `/`   | Aircraft    |
| `s`         | `/`   | Ship        |
| `-`         | `/`   | House       |
| `r`         | `/`   | Repeater    |
| `_`         | `/`   | Weather     |

## Configuration

### Default Symbols

- **Unknown packets**: Car symbol (`/`, `>`)
- **MicE packets**: Car symbol (`/`, `>`)
- **Invalid symbols**: Fall back to car symbol

### Styling

CSS classes for customization:
- `.aprs-marker` - Base marker styling
- `.historical-marker` - Historical packet opacity
- `.aprs-popup` - Popup content styling
- `.aprs-callsign` - Callsign display
- `.aprs-symbol-info` - Symbol description
- `.aprs-comment` - Packet comment
- `.aprs-coords` - Coordinate display

## High-DPI Support

Automatic retina display support using CSS media queries:
```css
@media (-webkit-min-device-pixel-ratio: 2), (min-resolution: 192dpi) {
  .aprs-marker div[style*="aprs-symbols-24-0.png"] {
    background-image: url('/aprs-symbols/aprs-symbols-24-0@2x.png') !important;
    background-size: 384px 144px !important;
  }
}
```

## Navigation

Added floating navigation headers to all map versions:
- Links between different map implementations
- Access to status and packet pages
- Consistent UI across all versions

## Symbol Credits

APRS symbols from [hessu/aprs-symbols](https://github.com/hessu/aprs-symbols):
- Created by Heikki Hannikainen OH7LZB
- High-resolution vector-based symbol set
- Released to APRS community for free use
- Compatible with Updated APRS Symbol Set (Rev H)

## Future Enhancements

Potential improvements:
- Overlay character support for numbered/lettered symbols
- Symbol rotation based on course/heading
- Custom symbol upload functionality
- Symbol animation for moving objects
- Symbol filtering and categorization