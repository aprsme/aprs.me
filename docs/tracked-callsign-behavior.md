# Tracked Callsign Behavior Documentation

## Overview

When viewing a specific callsign at the `/:callsign` route (e.g., `/KE0GB`), the APRS.me application displays the last received packet from that callsign **regardless of age**.

## Implementation Details

### 1. Route Handling
- Route defined in `router.ex`: `live "/:callsign", MapLive.Index, :index`
- The callsign is extracted from the URL and set as `tracked_callsign` in the LiveView socket

### 2. Latest Packet Retrieval
- `Packets.get_latest_packet_for_callsign/1` fetches the most recent packet for a callsign
- Uses `QueryBuilder.callsign_history/2` with only a limit parameter (no time filtering)
- **No time restrictions** are applied when fetching the latest packet

### 3. Map Centering
- If the tracked callsign has a known position, the map centers on that location at zoom level 12
- Handled by `Navigation.handle_callsign_tracking/4` in the MapLive.Navigation module

### 4. Packet Display Logic
The key function ensuring old packets are always displayed is `filter_packets_by_time_and_bounds_with_tracked/5` in MapLive.Index:

```elixir
defp filter_packets_by_time_and_bounds_with_tracked(
       packets,
       bounds,
       time_threshold,
       tracked_callsign,
       tracked_latest_packet
     ) do
  filtered = filter_packets_by_time_and_bounds(packets, bounds, time_threshold)

  # Always include the tracked callsign's latest packet if we have one
  if tracked_callsign != "" and tracked_latest_packet do
    key = get_callsign_key(tracked_latest_packet)
    Map.put(filtered, key, tracked_latest_packet)
  else
    filtered
  end
end
```

This function:
1. First filters packets by time threshold and map bounds
2. Then **always adds** the tracked callsign's latest packet to the filtered results
3. This ensures the tracked callsign is visible even if their last position is older than configured time windows

### 5. Trail Duration Independence
- Even with the shortest trail duration (1 hour), the tracked callsign's latest packet is displayed
- The trail duration setting only affects which other packets are shown, not the tracked callsign

## User Experience

When a user navigates to `/:callsign`:
1. The callsign appears in the search box
2. A "Tracking: CALLSIGN" indicator shows below the search
3. The map centers on the callsign's last known position (if available)
4. The callsign's latest packet is displayed on the map, no matter how old
5. Only packets from that callsign are shown (real-time filtering applies)

## Summary

The system is correctly implemented to **always show the last received packet** from a tracked callsign, regardless of:
- How old the packet is (could be days, weeks, or months old)
- The selected trail duration setting
- The historical data load setting
- Whether the packet is within the current map bounds
- **Whether the packet has position data or not** (as of 2025-07-30)

This ensures users can always find and view the last activity of any callsign they're tracking, including:
- Position packets with lat/lon coordinates
- Status updates without position
- Messages and telemetry data
- Weather reports
- Any other APRS packet types

## Fix Applied (2025-07-30)

The `get_recent_packets` function was modified to not filter by `has_position == true` when a specific callsign is being tracked. This ensures that all packet types are visible when viewing a callsign's activity, not just those with position data.