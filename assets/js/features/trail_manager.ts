// Trail management module for APRS position history visualization

import type { LayerGroup, Polyline, CircleMarker, PolylineOptions } from 'leaflet';

// Declare Leaflet as a global
declare const L: typeof import('leaflet');

export interface PositionHistory {
  lat: number;
  lng: number;
  timestamp: number;
}

export interface TrailState {
  positions: PositionHistory[];
  trail?: Polyline;
  dots?: CircleMarker[];
  color?: string;
}

export class TrailManager {
  private trailLayer: LayerGroup;
  private trails: Map<string, TrailState>;
  private showTrails: boolean;
  private trailDuration: number; // in milliseconds
  private maxTrails: number = 500; // Maximum number of trails to keep in memory
  private maxPositionsPerTrail: number = 1000; // Maximum positions per trail
  private colorPalette: string[] = [
    '#1E90FF', // Dodger Blue
    '#00CED1', // Dark Turquoise
    '#32CD32', // Lime Green
    '#8B008B', // Dark Magenta
    '#9370DB', // Medium Purple
    '#FF8C00', // Dark Orange
    '#4682B4', // Steel Blue
    '#00FA9A', // Medium Spring Green
    '#DA70D6', // Orchid
    '#008B8B', // Dark Cyan
    '#48D1CC', // Medium Turquoise
    '#228B22', // Forest Green
    '#6495ED', // Cornflower Blue
    '#FF1493', // Deep Pink (distinct from highways)
    '#20B2AA', // Light Sea Green
  ];
  private proximityThreshold: number = 0.05; // ~5.5km at equator

  constructor(trailLayer: LayerGroup, trailDuration: number = 60 * 60 * 1000) {
    this.trailLayer = trailLayer;
    this.trails = new Map();
    this.showTrails = true;
    this.trailDuration = trailDuration;

    // Ensure trail layer is added to the map and visible
    if (this.trailLayer && this.trailLayer.bringToFront) {
      setTimeout(() => this.trailLayer.bringToFront(), 100);
    }
  }

  setShowTrails(show: boolean) {
    this.showTrails = show;
    if (!show) {
      // When hiding trails, just remove them from the layer but keep the data
      this.trails.forEach((trailState) => {
        if (trailState.trail) {
          this.trailLayer.removeLayer(trailState.trail);
          trailState.trail = undefined;
        }
        if (trailState.dots) {
          trailState.dots.forEach((dot) => this.trailLayer.removeLayer(dot));
          trailState.dots = [];
        }
      });
    } else {
      // When re-enabling trails, recreate all trails immediately, preserving colors
      this.trails.forEach((trailState, baseCallsign) => {
        this.updateTrailVisualization(baseCallsign, trailState, true);
      });
    }
  }

  setTrailDuration(durationHours: number) {
    this.trailDuration = durationHours * 60 * 60 * 1000; // Convert hours to milliseconds
    
    // Clean up existing trails based on new duration
    const cutoffTime = Date.now() - this.trailDuration;
    
    this.trails.forEach((trailState, baseCallsign) => {
      // Filter positions based on new duration (skip historical dots)
      trailState.positions = trailState.positions.filter((pos) => {
        const posTimestamp = typeof pos.timestamp === "string" ? new Date(pos.timestamp).getTime() : pos.timestamp;
        return posTimestamp >= cutoffTime;
      });
      
      // Update trail visualization
      this.updateTrailVisualization(baseCallsign, trailState, true);
    });
  }

  addPosition(
    markerId: string,
    lat: number,
    lng: number,
    timestamp: number,
    isHistoricalDot: boolean = false,
  ) {
    if (!this.showTrails) return;

    // Extract base callsign from markerId to group positions by callsign
    const baseCallsign = this.extractBaseCallsign(markerId);

    let trailState = this.trails.get(baseCallsign);
    if (!trailState) {
      // Check if we've reached the maximum number of trails
      if (this.trails.size >= this.maxTrails) {
        // Remove the oldest trail (first in the Map)
        const oldestKey = this.trails.keys().next().value;
        if (oldestKey) {
          this.removeTrail(oldestKey);
        }
      }
      trailState = { positions: [] };
      this.trails.set(baseCallsign, trailState);
    }

    // Check if this position already exists (avoid duplicates)
    const existingPos = trailState.positions.find(
      (pos) =>
        Math.abs(pos.lat - lat) < 0.00001 &&
        Math.abs(pos.lng - lng) < 0.00001 &&
        Math.abs(pos.timestamp - timestamp) < 1000, // Within 1 second
    );

    if (!existingPos) {
      // Add new position
      trailState.positions.push({ lat, lng, timestamp });

      // Sort positions by timestamp to maintain chronological order
      trailState.positions.sort((a, b) => a.timestamp - b.timestamp);
      
      // Limit the number of positions per trail
      if (trailState.positions.length > this.maxPositionsPerTrail) {
        // Keep the most recent positions
        trailState.positions = trailState.positions.slice(-this.maxPositionsPerTrail);
      }
    }

    // For historical dots, keep all positions. For live positions, filter by time.
    if (!isHistoricalDot) {
      const cutoffTime = Date.now() - this.trailDuration;
      trailState.positions = trailState.positions.filter((pos) => {
        // Ensure timestamp is a number for comparison
        const posTimestamp =
          typeof pos.timestamp === "string" ? new Date(pos.timestamp).getTime() : pos.timestamp;
        return posTimestamp >= cutoffTime;
      });
    }

    // Always update trail visualization, with immediate=true for historical dots
    // This ensures historical trails are immediately visible when loaded all at once
    this.updateTrailVisualization(baseCallsign, trailState, isHistoricalDot);
  }

  private extractBaseCallsign(markerId: string | number): string {
    // Ensure markerId is a string
    const id = String(markerId);

    // For historical markers like "hist_CALLSIGN_123", extract the CALLSIGN part
    if (id.startsWith("hist_")) {
      // Remove hist_ prefix and any trailing numeric index
      const withoutPrefix = id.replace(/^hist_/, "");
      // Remove trailing _digits pattern
      return withoutPrefix.replace(/_\d+$/, "");
    }

    // For regular callsigns or IDs, return as-is
    return id;
  }

  // Calculate distance between two points using Haversine formula
  private calculateDistance(lat1: number, lng1: number, lat2: number, lng2: number): number {
    const R = 6371; // Earth's radius in km
    const dLat = (lat2 - lat1) * Math.PI / 180;
    const dLng = (lng2 - lng1) * Math.PI / 180;
    const a = Math.sin(dLat/2) * Math.sin(dLat/2) +
              Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
              Math.sin(dLng/2) * Math.sin(dLng/2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    return R * c;
  }

  // Get the average position of a trail
  private getTrailCenter(positions: PositionHistory[]): { lat: number, lng: number } {
    if (positions.length === 0) return { lat: 0, lng: 0 };
    
    const sum = positions.reduce((acc, pos) => ({
      lat: acc.lat + pos.lat,
      lng: acc.lng + pos.lng
    }), { lat: 0, lng: 0 });
    
    return {
      lat: sum.lat / positions.length,
      lng: sum.lng / positions.length
    };
  }

  // Find nearby trails and get their colors
  private getNearbyTrailColors(baseCallsign: string, center: { lat: number, lng: number }): Set<string> {
    const nearbyColors = new Set<string>();
    
    this.trails.forEach((trailState, callsign) => {
      if (callsign === baseCallsign || !trailState.color || trailState.positions.length === 0) return;
      
      const otherCenter = this.getTrailCenter(trailState.positions);
      const distance = this.calculateDistance(center.lat, center.lng, otherCenter.lat, otherCenter.lng);
      
      if (distance < this.proximityThreshold * 111) { // Convert degrees to km (rough approximation)
        nearbyColors.add(trailState.color);
      }
    });
    
    return nearbyColors;
  }

  // Assign a color to a trail based on nearby trails
  private assignTrailColor(baseCallsign: string, positions: PositionHistory[]): string {
    if (positions.length === 0) return this.colorPalette[0];
    
    const center = this.getTrailCenter(positions);
    const nearbyColors = this.getNearbyTrailColors(baseCallsign, center);
    
    // Find the first available color not used by nearby trails
    for (const color of this.colorPalette) {
      if (!nearbyColors.has(color)) {
        return color;
      }
    }
    
    // If all colors are used, return the first color
    return this.colorPalette[0];
  }

  private updateTrailVisualization(
    baseCallsign: string,
    trailState: TrailState,
    immediate: boolean = false,
  ) {
    // Remove old trail and dots
    if (trailState.trail) {
      this.trailLayer.removeLayer(trailState.trail);
    }
    if (trailState.dots) {
      trailState.dots.forEach((dot) => this.trailLayer.removeLayer(dot));
      trailState.dots = [];
    }

    // Create new trail if we have at least 2 positions
    if (trailState.positions.length >= 2) {
      const latLngs: [number, number][] = trailState.positions.map((pos) => [pos.lat, pos.lng]);

      // Assign color if not already assigned
      if (!trailState.color) {
        trailState.color = this.assignTrailColor(baseCallsign, trailState.positions);
      }

      // Create polyline with assigned color
      // For historical positions (immediate=true), use higher opacity for better visibility
      const polylineOptions: PolylineOptions = {
        color: trailState.color,
        weight: 3,
        opacity: immediate ? 0.9 : 0.8,
        smoothFactor: 1,
        lineCap: "round",
        lineJoin: "round",
        className: "historical-trail-line",
      };
      
      trailState.trail = L.polyline(latLngs, polylineOptions).addTo(this.trailLayer);

      // Don't create additional dots here since historical positions are now shown as markers
      trailState.dots = [];
    }
  }

  removeTrail(markerId: string) {
    const baseCallsign = this.extractBaseCallsign(markerId);
    const trailState = this.trails.get(baseCallsign);
    if (trailState) {
      if (trailState.trail) {
        this.trailLayer.removeLayer(trailState.trail);
      }
      if (trailState.dots) {
        trailState.dots.forEach((dot) => this.trailLayer.removeLayer(dot));
      }
      this.trails.delete(baseCallsign);
    }
  }

  clearAllTrails() {
    this.trails.forEach((_, markerId) => {
      this.removeTrail(markerId);
    });
  }

  cleanupOldPositions() {
    // Don't clean up historical positions - only clean up very old live positions
    const veryOldCutoff = Date.now() - 24 * 60 * 60 * 1000; // 24 hours
    this.trails.forEach((trailState, markerId) => {
      const originalLength = trailState.positions.length;
      trailState.positions = trailState.positions.filter((pos) => {
        // Ensure timestamp is a number for comparison
        const posTimestamp =
          typeof pos.timestamp === "string" ? new Date(pos.timestamp).getTime() : pos.timestamp;
        // Keep all positions newer than 24 hours (includes all historical data we care about)
        return posTimestamp >= veryOldCutoff;
      });

      if (trailState.positions.length === 0) {
        this.removeTrail(markerId);
      } else if (trailState.positions.length !== originalLength) {
        this.updateTrailVisualization(markerId, trailState);
      }
    });
  }
}
