# Trail Line Visualization

## Overview

When tracking a specific callsign, the map displays a trail line (polyline) connecting all position points chronologically at zoom levels ≤ 8, instead of showing a heat map.

## User Experience

### Viewing Trail Lines

1. Search for and track a callsign using the search box
2. Zoom out to level 8 or below
3. A blue trail line appears connecting all positions in chronological order
4. The trail line respects the "Trail Duration" setting (1 hour, 6 hours, etc.)

### Zoom Behavior

- **Zoom ≤ 8 + Tracking:** Trail line visible
- **Zoom > 8 + Tracking:** Individual markers visible
- **Zoom ≤ 8 + No tracking:** Heat map visible
- **Zoom > 8 + No tracking:** Individual markers visible

## Technical Details

### Backend

- `DisplayManager.send_trail_line_for_tracked_callsign/1` - Generates trail line data
- `DisplayManager.handle_zoom_threshold_crossing/2` - Handles mode switching
- Trail line data filtered by `packet_age_threshold` (trail duration setting)
- Points sorted chronologically by `received_at` timestamp

### Frontend

- `MapHook.trailLine` - Leaflet Polyline instance
- Event: `show_trail_line` - Display trail line
- Event: `clear_trail_line` - Remove trail line
- Trail line cleared when switching to heat map or markers

### Styling

- Color: `#3B82F6` (blue-500)
- Weight: 3 pixels
- Opacity: 0.8
- Line Cap/Join: round
- Interactive: true

## Continuous Path Connection

Trail lines connect **all points regardless of distance** between them. This is important for:

- High-altitude balloons traveling long distances
- Aircraft on cross-country flights
- Stations with intermittent coverage gaps
- Showing complete journey paths

No distance-based filtering or gap detection is applied. The trail always shows the complete chronological path within the trail duration.

## Future Enhancements

- Direction arrows along the trail
- Time-based gradient coloring (old → new)
- Click on trail to show packet details
- Line simplification for very long trails
- Multiple callsign tracking with different colors
