# Trail Line Display for Tracked Callsigns at Low Zoom

## Problem Statement

When tracking a specific callsign (e.g., W5ISP-9), the map currently displays aggregated heat map data at zoom levels ≤ 8. This makes it impossible to see the individual path points and trajectory of the tracked station when zoomed out. Users need to see the complete path of a tracked callsign regardless of zoom level.

## Solution Overview

Implement a trail line visualization that displays when tracking a specific callsign at low zoom levels (≤ 8). Instead of showing a heat map, the system will draw a polyline connecting all the station's position points in chronological order, clearly showing the path traveled.

## Behavior Specification

### When Tracking a Callsign

- **Zoom ≤ 8**: Display trail line (polyline) connecting all positions chronologically
  - Clear any existing heat map
  - Fetch all packets for tracked callsign within time threshold
  - Sort by timestamp (chronological order)
  - Draw polyline on map

- **Zoom > 8**: Display individual markers (current behavior)
  - Show APRS symbols at each position
  - Enable popup interactions

### When NOT Tracking a Callsign

- **Zoom ≤ 8**: Display heat map (current behavior)
- **Zoom > 8**: Display individual markers (current behavior)

## Technical Design

### Backend Changes (Elixir)

**File: `lib/aprsme_web/live/map_live/display_manager.ex`**

1. Modify `handle_zoom_threshold_crossing/2`:
   - Check if `socket.assigns.tracked_callsign != ""`
   - If tracking and zoom ≤ 8: call new function to send trail line
   - If tracking and zoom > 8: show markers as normal
   - If not tracking: use existing heat map/marker logic

2. Add new function `send_trail_line_for_tracked_callsign/1`:
   - Query all packets for tracked callsign within time threshold
   - Sort packets by `received_at` (chronological order)
   - Extract coordinates (lat, lng, timestamp)
   - Push event `show_trail_line` with trail data

**File: `lib/aprsme_web/live/map_live/index.ex`**

3. Update bounds update logic:
   - When processing bounds updates at zoom ≤ 8 with tracked callsign
   - Send trail line instead of heat map

### Frontend Changes (TypeScript)

**File: `assets/js/map.ts`**

1. Add state tracking:
   - `currentTrailLine: Polyline | null` - store reference to current trail line

2. Add event handler `show_trail_line`:
   - Clear existing trail line if present
   - Create Leaflet Polyline from coordinates
   - Style the polyline (color, weight, opacity)
   - Add to map
   - Store reference in state

3. Add event handler `clear_trail_line`:
   - Remove trail line from map
   - Clear state reference

4. Update existing handlers:
   - `show_heat_map`: Clear trail line before showing heat map
   - `show_markers`: Clear trail line before showing markers
   - `clear_all_markers`: Also clear trail line

### Data Structure

**Trail Line Event Payload:**
```typescript
{
  callsign: string,
  points: Array<{
    lat: number,
    lng: number,
    timestamp: string
  }>
}
```

### Visual Styling

**Trail Line Appearance:**
- Color: `#3B82F6` (blue-500) - distinct from other map elements
- Weight: 3 pixels - visible but not overwhelming
- Opacity: 0.8 - allows seeing map features underneath
- Line Cap: round - smoother appearance at endpoints
- Line Join: round - smoother corners
- Interactive: true - allow clicking for info
- Dash Pattern: none (solid line)

**Optional Enhancement (Future):**
- Direction arrows along the line
- Gradient color showing time progression
- Click on line to show timestamp at that point

## Implementation Notes

### Continuous Path Connection
- **Connect all points regardless of distance**: The trail line should connect all position points in chronological order, even if there are large gaps between positions
- This is important for tracking stations that:
  - Travel long distances (aircraft, vehicles on highways)
  - Have intermittent coverage (gaps in digipeater/igate range)
  - Cross large geographic areas
- Do NOT apply distance-based filtering or gap detection
- The continuous line shows the complete journey path for the tracked callsign

### Time Threshold Handling
- Use existing `socket.assigns.packet_age_threshold` for filtering
- Trail line respects the "Trail Duration" setting (1 hour, 6 hours, etc.)
- When trail duration changes, regenerate trail line

### Performance Considerations
- Trail lines are only generated for tracked callsigns (not all stations)
- Polylines are efficient for rendering many points
- Clear trail line when switching away from tracked callsign
- Debounce trail line updates during rapid zoom changes

### Edge Cases
- Empty trail (no packets): Don't draw anything
- Single point: Don't draw line (show marker instead)
- Zoom threshold crossing: Smoothly transition between trail and markers
- Callsign tracking cleared: Remove trail line immediately
- **International date line crossing**: Trail line should wrap correctly around the date line (Leaflet handles this automatically with proper coordinate ordering)
- **Large distance gaps**: Continue drawing line even with large geographic gaps between consecutive points

## Testing Strategy

1. **Manual Testing:**
   - Track a callsign and zoom out to ≤ 8: verify trail line appears
   - Zoom in to > 8: verify markers appear
   - Clear tracking: verify trail line disappears
   - Change trail duration: verify trail line updates

2. **Visual Testing:**
   - Verify trail line color and styling
   - Check line smoothness and appearance
   - Ensure map features remain visible underneath

3. **Edge Cases:**
   - Callsign with no packets
   - Callsign with single packet
   - Callsign with very long trail (performance)
   - International date line crossing

## Future Enhancements

1. **Direction Indicators**: Add small arrows along the trail to show direction of travel
2. **Time-based Coloring**: Gradient color from old (gray) to new (bright blue)
3. **Interactive Points**: Click on trail to see packet details at that position
4. **Trail Simplification**: Use line simplification algorithm (Douglas-Peucker) for very long trails
5. **Multiple Callsigns**: Support tracking multiple callsigns with different colored trails
