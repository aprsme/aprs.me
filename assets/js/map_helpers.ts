// Helper functions for map.ts to reduce code duplication

import type * as L from 'leaflet';
import type { BaseEventPayload, PushEventFunction } from './types/events';
import type { LiveSocket } from './types/map';

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
export function parseTimestamp(timestamp: string | number | Date | undefined): number {
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
export function getTrailId(data: { callsign_group?: string; callsign?: string; id: string | number }): string {
  // Prioritize callsign_group and callsign over extracting from ID
  if (data.callsign_group) {
    return data.callsign_group;
  }
  
  if (data.callsign) {
    return data.callsign;
  }
  
  // For historical markers like "hist_CALLSIGN_123", extract the base callsign
  const id = String(data.id);
  if (id.startsWith("hist_")) {
    // Remove hist_ prefix and any trailing numeric index
    const withoutPrefix = id.replace(/^hist_/, "");
    // Remove trailing _digits pattern
    return withoutPrefix.replace(/_\d+$/, "");
  }
  
  // For regular IDs, return as string
  return String(data.id);
}

/**
 * Save map state to localStorage and send to server
 */
// PushEventFunction is now imported from types/events

export function saveMapState(map: L.Map, pushEvent: PushEventFunction) {
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
    
    // Use safePushEvent to handle disconnected state
    safePushEvent(pushEvent, "update_map_state", payload);
  } catch (error) {
    console.error("Error in saveMapState:", error);
  }
}

/**
 * Safely push event to LiveView
 */
export function safePushEvent<T extends BaseEventPayload = BaseEventPayload>(pushEvent: PushEventFunction | undefined, event: string, payload: T): boolean {
  if (!pushEvent || typeof pushEvent !== 'function') {
    return false;
  }
  
  try {
    pushEvent(event, payload);
    return true;
  } catch (e) {
    return false;
  }
}

/**
 * Check if LiveView socket is available
 */
export function isLiveViewConnected(): boolean {
  return typeof window !== 'undefined' && !!window.liveSocket;
}

/**
 * Get LiveView socket
 */
export function getLiveSocket(): LiveSocket | null {
  return typeof window !== 'undefined' ? window.liveSocket || null : null;
}