// Helper functions for map.ts to reduce code duplication

export interface MapState {
  lat: number;
  lng: number;
  zoom: number;
}

export interface BoundsData {
  north: number;
  south: number;
  east: number;
  west: number;
}

/**
 * Parse timestamp to milliseconds
 */
export function parseTimestamp(timestamp: any): number {
  if (!timestamp) return Date.now();
  
  if (typeof timestamp === "number") {
    return timestamp;
  } else if (typeof timestamp === "string") {
    return new Date(timestamp).getTime();
  }
  return Date.now();
}

/**
 * Get trail ID from marker data
 */
export function getTrailId(data: { callsign_group?: string; callsign?: string; id: string }): string {
  return data.callsign_group || data.callsign || data.id;
}

/**
 * Save map state to localStorage and send to server
 */
export function saveMapState(map: any, pushEvent: Function) {
  if (!map || !pushEvent) {
    console.warn("saveMapState called with invalid map or pushEvent");
    return;
  }
  
  try {
    const center = map.getCenter();
    const zoom = map.getZoom();
    
    // Truncate lat/lng to 5 decimal places for URL
    const truncatedLat = Math.round(center.lat * 100000) / 100000;
    const truncatedLng = Math.round(center.lng * 100000) / 100000;
    
    // Always save to localStorage, even if LiveView is disconnected
    // Temporarily disabled - using server-side geolocation instead
    /*
    localStorage.setItem(
      "aprs_map_state",
      JSON.stringify({ lat: truncatedLat, lng: truncatedLng, zoom }),
    );
    */
    
    // Send combined map state update to server for URL and bounds updating
    const payload = {
      center: { lat: truncatedLat, lng: truncatedLng },
      zoom: zoom,
      bounds: {
        north: map.getBounds().getNorth(),
        south: map.getBounds().getSouth(),
        east: map.getBounds().getEast(),
        west: map.getBounds().getWest(),
      }
    };
    
    console.debug("Sending update_map_state event:", payload);
    
    // Use safePushEvent to handle disconnected state
    safePushEvent(pushEvent, "update_map_state", payload);
  } catch (error) {
    console.error("Error in saveMapState:", error);
  }
}

/**
 * Safely push event to LiveView
 */
export function safePushEvent(pushEvent: Function | undefined, event: string, payload: any): boolean {
  if (!pushEvent || typeof pushEvent !== 'function') {
    console.debug(`pushEvent not available for ${event} event`);
    return false;
  }
  
  try {
    pushEvent(event, payload);
    return true;
  } catch (e) {
    console.debug(`Unable to send ${event} event:`, e);
    return false;
  }
}

/**
 * Check if LiveView socket is available
 */
export function isLiveViewConnected(): boolean {
  return !!(window as any).liveSocket;
}

/**
 * Get LiveView socket
 */
export function getLiveSocket(): any {
  return (window as any).liveSocket;
}