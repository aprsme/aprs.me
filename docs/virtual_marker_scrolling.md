# Virtual Marker Scrolling for Large Datasets

## Overview

When dealing with thousands of APRS markers on the map, rendering all markers at once can cause performance issues. Virtual scrolling for map markers is a technique where only visible markers within the viewport (plus a buffer zone) are rendered.

## Current Implementation

The current implementation already has some optimizations:
1. Viewport-based filtering in `get_recent_packets_optimized/3`
2. Marker state tracking to prevent unnecessary updates
3. Batch processing of historical packets

## Proposed Virtual Scrolling Enhancement

### 1. Marker Clustering at Low Zoom Levels
```typescript
// Use Leaflet.markercluster for automatic clustering
// Already included in the project but not fully utilized
if (self.map.getZoom() < 10) {
  // Use marker clustering
  self.clusterLayer = L.markerClusterGroup({
    maxClusterRadius: 80,
    spiderfyOnMaxZoom: true,
    showCoverageOnHover: false,
    zoomToBoundsOnClick: true
  });
}
```

### 2. Quadtree-based Spatial Indexing
```elixir
defmodule AprsmeWeb.MapLive.QuadTree do
  @moduledoc """
  Quadtree implementation for efficient spatial queries
  """
  
  defstruct [:bounds, :markers, :children, :max_markers]
  
  def insert(tree, marker) do
    # Insert marker into appropriate quadrant
  end
  
  def query(tree, bounds) do
    # Return only markers within bounds
  end
end
```

### 3. Progressive Rendering with RequestIdleCallback
```typescript
function renderMarkersProgressively(markers: MarkerData[]) {
  let index = 0;
  const batchSize = 50;
  
  function renderBatch() {
    const batch = markers.slice(index, index + batchSize);
    batch.forEach(marker => self.addMarker(marker));
    index += batchSize;
    
    if (index < markers.length) {
      requestIdleCallback(renderBatch);
    }
  }
  
  requestIdleCallback(renderBatch);
}
```

### 4. Server-side Aggregation
```elixir
def aggregate_markers_for_zoom(zoom_level, bounds) do
  case zoom_level do
    z when z < 8 ->
      # Return aggregated grid cells with counts
      Packets.get_grid_aggregates(bounds, grid_size: 50)
    
    z when z < 12 ->
      # Return simplified markers (no popups)
      Packets.get_simplified_markers(bounds)
    
    _ ->
      # Return full markers
      Packets.get_recent_packets_optimized(bounds)
  end
end
```

### 5. Viewport Buffer Strategy
```typescript
// Render markers in expanded viewport to reduce re-renders during panning
const bounds = self.map.getBounds();
const bufferedBounds = bounds.pad(0.5); // 50% buffer
```

## Implementation Priority

1. **Phase 1**: Implement marker clustering (easiest, biggest impact)
2. **Phase 2**: Add server-side aggregation for low zoom levels
3. **Phase 3**: Implement progressive rendering
4. **Phase 4**: Add quadtree spatial indexing if still needed

## Performance Metrics to Track

- Time to first marker render
- Frame rate during pan/zoom
- Memory usage with 10k+ markers
- Server query time by zoom level

## Notes

- The current implementation already handles viewport filtering well
- Virtual scrolling is most beneficial when dealing with 5000+ markers
- Consider implementing only if performance issues are reported by users