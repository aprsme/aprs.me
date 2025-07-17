// Proposed fixes for map.ts issues

import type { Map as LeafletMap } from 'leaflet';
import type { LiveViewHookContext } from './types/map';
import type { BaseEventPayload, PushEventFunction } from './types/events';
import type { APRSMarker } from './types/marker-extensions';

// 1. Extract duplicated functions
export const MapHelpers = {
  // Centralized timestamp parsing
  parseTimestamp(timestamp: string | number | undefined | null): number {
    if (!timestamp) return Date.now();
    
    if (typeof timestamp === "number") {
      return timestamp;
    } else if (typeof timestamp === "string") {
      return new Date(timestamp).getTime();
    }
    return Date.now();
  },

  // Centralized trail ID calculation
  getTrailId(data: { callsign_group?: string; callsign?: string; id: string }): string {
    return data.callsign_group || data.callsign || data.id;
  },

  // Centralized map state saving
  saveMapState(map: LeafletMap, pushEvent: PushEventFunction) {
    const center = map.getCenter();
    const zoom = map.getZoom();
    
    // Truncate lat/lng to 5 decimal places for URL
    const truncatedLat = Math.round(center.lat * 100000) / 100000;
    const truncatedLng = Math.round(center.lng * 100000) / 100000;
    
    localStorage.setItem(
      "aprs_map_state",
      JSON.stringify({ lat: truncatedLat, lng: truncatedLng, zoom }),
    );
    
    // Send combined map state update to server for URL and bounds updating
    pushEvent("update_map_state", {
      center: { lat: truncatedLat, lng: truncatedLng },
      zoom: zoom,
      bounds: {
        north: map.getBounds().getNorth(),
        south: map.getBounds().getSouth(),
        east: map.getBounds().getEast(),
        west: map.getBounds().getWest(),
      }
    });
  },

  // Safe event pushing with connection check
  safePushEvent(pushEvent: PushEventFunction | undefined, event: string, payload: BaseEventPayload) {
    if (!pushEvent) return false;
    
    try {
      pushEvent(event, payload);
      return true;
    } catch (e) {
      console.debug(`Unable to send ${event} event - LiveView disconnected`);
      return false;
    }
  }
};

// 2. Improved LiveViewHookContext with proper cleanup tracking
interface ImprovedLiveViewHookContext extends LiveViewHookContext {
  cleanupInterval?: ReturnType<typeof setInterval>;
  mapEventHandlers?: Map<string, Function>;
  isDestroyed?: boolean;
}

// 3. Example of fixed initialization with proper cleanup tracking
export const setupMapWithCleanup = (self: ImprovedLiveViewHookContext) => {
  // Track if component is destroyed
  self.isDestroyed = false;
  
  // Store event handlers for cleanup
  self.mapEventHandlers = new Map();
  
  // Store interval for cleanup
  self.cleanupInterval = setInterval(() => {
    if (!self.isDestroyed && self.trailManager) {
      self.trailManager.cleanupOldPositions();
    }
  }, 5 * 60 * 1000);
  
  // Create wrapped event handlers that check if destroyed
  const createSafeHandler = <TArgs extends unknown[]>(handler: (...args: TArgs) => void) => {
    return (...args: TArgs) => {
      if (!self.isDestroyed) {
        handler(...args);
      }
    };
  };
  
  // Example of safe map event handler
  const moveEndHandler = createSafeHandler(() => {
    if (self.programmaticMoveCounter && self.programmaticMoveCounter > 0) {
      self.programmaticMoveCounter--;
      return;
    }
    
    if (self.boundsTimer) clearTimeout(self.boundsTimer);
    self.boundsTimer = setTimeout(() => {
      if (!self.isDestroyed) {
        MapHelpers.saveMapState(self.map, self.pushEvent);
      }
    }, 300);
  });
  
  self.map.on("moveend", moveEndHandler);
  self.mapEventHandlers.set("moveend", moveEndHandler);
};

// 4. Improved cleanup function
export const improvedDestroyed = (self: ImprovedLiveViewHookContext) => {
  // Mark as destroyed immediately
  self.isDestroyed = true;
  
  // Disable pushEvent to prevent any events from being sent during cleanup
  const originalPushEvent = self.pushEvent;
  self.pushEvent = () => {}; // No-op function
  
  // Clear interval timer
  if (self.cleanupInterval !== undefined) {
    clearInterval(self.cleanupInterval);
    self.cleanupInterval = undefined;
  }
  
  // Remove popup navigation event listener
  if (self.popupNavigationHandler) {
    document.removeEventListener('click', self.popupNavigationHandler);
    self.popupNavigationHandler = undefined;
  }
  
  // Close any open popups before cleanup
  if (self.map !== undefined) {
    try {
      self.map.closePopup();
    } catch (e) {
      console.debug("Error closing popup during cleanup:", e);
    }
  }
  
  // Clear timers
  if (self.boundsTimer !== undefined) {
    clearTimeout(self.boundsTimer);
    self.boundsTimer = undefined;
  }
  
  if (self.resizeHandler !== undefined) {
    window.removeEventListener("resize", self.resizeHandler);
    self.resizeHandler = undefined;
  }
  
  // Remove map event handlers
  if (self.map !== undefined && self.mapEventHandlers !== undefined) {
    self.mapEventHandlers.forEach((handler, event) => {
      try {
        self.map.off(event, handler);
      } catch (e) {
        console.debug(`Error removing ${event} handler:`, e);
      }
    });
    self.mapEventHandlers.clear();
  }
  
  // Remove all event listeners from markers before clearing layers
  if (self.markers !== undefined) {
    self.markers.forEach((marker: APRSMarker, id: string) => {
      try {
        marker.off(); // Remove all event listeners
        if (marker.getPopup()) {
          marker.unbindPopup(); // Unbind popup to prevent events
        }
      } catch (e) {
        console.debug(`Error cleaning up marker ${id}:`, e);
      }
    });
  }
  
  // Clear layers and data
  if (self.markerLayer !== undefined) {
    try {
      self.markerLayer!.clearLayers();
    } catch (e) {
      console.debug("Error clearing marker layer:", e);
    }
  }
  
  if (self.markers !== undefined) {
    self.markers!.clear();
  }
  
  if (self.markerStates !== undefined) {
    self.markerStates!.clear();
  }
  
  // Remove map
  if (self.map !== undefined) {
    try {
      self.map!.remove();
    } catch (e) {
      console.debug("Error removing map:", e);
    }
    self.map = undefined;
  }
  
  // Clear OMS if present
  if (self.oms !== undefined) {
    self.oms = undefined;
  }
  
  // Restore original pushEvent (though it won't be used since we're destroyed)
  self.pushEvent = originalPushEvent;
};

// 5. Optimized marker position lookup using spatial index
export class MarkerSpatialIndex {
  private grid: Map<string, Set<string>> = new Map();
  private cellSize: number = 0.0001; // ~11 meters at equator
  
  private getGridKey(lat: number, lng: number): string {
    const latCell = Math.floor(lat / this.cellSize);
    const lngCell = Math.floor(lng / this.cellSize);
    return `${latCell},${lngCell}`;
  }
  
  add(id: string, lat: number, lng: number) {
    const key = this.getGridKey(lat, lng);
    if (!this.grid.has(key)) {
      this.grid.set(key, new Set());
    }
    this.grid.get(key)!.add(id);
  }
  
  remove(id: string, lat: number, lng: number) {
    const key = this.getGridKey(lat, lng);
    const cell = this.grid.get(key);
    if (cell) {
      cell.delete(id);
      if (cell.size === 0) {
        this.grid.delete(key);
      }
    }
  }
  
  findNearby(lat: number, lng: number, radius: number = 0.00001): Set<string> {
    const results = new Set<string>();
    const cellRadius = Math.ceil(radius / this.cellSize);
    const centerLatCell = Math.floor(lat / this.cellSize);
    const centerLngCell = Math.floor(lng / this.cellSize);
    
    for (let latOffset = -cellRadius; latOffset <= cellRadius; latOffset++) {
      for (let lngOffset = -cellRadius; lngOffset <= cellRadius; lngOffset++) {
        const key = `${centerLatCell + latOffset},${centerLngCell + lngOffset}`;
        const cell = this.grid.get(key);
        if (cell) {
          cell.forEach(id => results.add(id));
        }
      }
    }
    
    return results;
  }
  
  clear() {
    this.grid.clear();
  }
}

// 6. Improved programmatic move counter with proper state management
export class ProgrammaticMoveTracker {
  private moveCount: number = 0;
  private timeoutId?: ReturnType<typeof setTimeout>;
  
  expectMoves(count: number) {
    this.moveCount += count;
    
    // Clear existing timeout
    if (this.timeoutId) {
      clearTimeout(this.timeoutId);
    }
    
    // Set safety timeout
    this.timeoutId = setTimeout(() => {
      if (this.moveCount > 0) {
        console.debug(`Resetting programmatic move counter from ${this.moveCount} to 0`);
        this.moveCount = 0;
      }
    }, 2000);
  }
  
  handleMove(): boolean {
    if (this.moveCount > 0) {
      this.moveCount--;
      return true; // This was a programmatic move
    }
    return false; // This was a user-initiated move
  }
  
  reset() {
    this.moveCount = 0;
    if (this.timeoutId) {
      clearTimeout(this.timeoutId);
      this.timeoutId = undefined;
    }
  }
}