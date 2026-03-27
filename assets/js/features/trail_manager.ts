// Trail management module for APRS position history visualization

import type {
  LayerGroup,
  Polyline,
  CircleMarker,
  PolylineOptions,
} from "leaflet";

// Declare Leaflet as a global
declare const L: typeof import("leaflet");

export interface PositionHistory {
  lat: number;
  lng: number;
  timestamp: number;
  path?: string;
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
  private pendingTrailUpdates: Map<string, boolean> = new Map(); // callsign -> isHistorical
  private trailUpdateRafId: number | null = null;
  // Colors ordered to maximize contrast between consecutive assignments.
  // Avoids yellows, oranges, and grays that blend with roads on map tiles.
  private colorPalette: string[] = [
    "#E6194B", // Crimson
    "#469990", // Teal
    "#F032E6", // Magenta
    "#4363D8", // Blue
    "#D50000", // Red
    "#3CB44B", // Green
    "#FF1493", // Deep Pink
    "#42D4F4", // Cyan
    "#911EB4", // Purple
    "#00CC66", // Emerald
    "#7B68EE", // Slate Blue
    "#76FF03", // Lime
    "#E040FB", // Heliotrope
    "#1E90FF", // Dodger Blue
    "#00BFA5", // Aquamarine
  ];
  private proximityThreshold: number = 5.5; // kilometers
  private minMovementThreshold: number = 0.1; // km - minimum total distance to show a trail
  private maxHopDistance: number = 10; // km - max distance between consecutive points before breaking
  private minSegmentPoints: number = 3; // minimum points in a segment to draw it
  private onTrailHover?: (lat: number, lng: number, path: string) => void;
  private onTrailHoverEnd?: () => void;
  private trailHoverDebounceTimer?: ReturnType<typeof setTimeout>;
  private lastHoveredPath?: string;
  private isDestroyed: boolean = false;

  constructor(
    trailLayer: LayerGroup,
    trailDuration: number = 60 * 60 * 1000,
    onTrailHover?: (lat: number, lng: number, path: string) => void,
    onTrailHoverEnd?: () => void,
  ) {
    this.trailLayer = trailLayer;
    this.trails = new Map();
    this.showTrails = true;
    this.trailDuration = trailDuration;
    this.onTrailHover = onTrailHover;
    this.onTrailHoverEnd = onTrailHoverEnd;

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
        const posTimestamp =
          typeof pos.timestamp === "string"
            ? new Date(pos.timestamp).getTime()
            : pos.timestamp;
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
    path?: string,
  ) {
    if (!this.showTrails) return;

    // Validate coordinates before processing
    if (
      typeof lat !== "number" ||
      typeof lng !== "number" ||
      isNaN(lat) ||
      isNaN(lng) ||
      !isFinite(lat) ||
      !isFinite(lng) ||
      lat < -90 ||
      lat > 90 ||
      lng < -180 ||
      lng > 180
    ) {
      console.warn("Invalid coordinates provided to addPosition:", {
        markerId,
        lat,
        lng,
        timestamp,
      });
      return;
    }

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
      trailState.positions.push({ lat, lng, timestamp, path });

      // Sort positions by timestamp to maintain chronological order
      trailState.positions.sort((a, b) => a.timestamp - b.timestamp);

      // Limit the number of positions per trail
      if (trailState.positions.length > this.maxPositionsPerTrail) {
        // Keep the most recent positions
        trailState.positions = trailState.positions.slice(
          -this.maxPositionsPerTrail,
        );
      }
    }

    // For historical dots, keep all positions. For live positions, filter by time.
    if (!isHistoricalDot) {
      const cutoffTime = Date.now() - this.trailDuration;
      trailState.positions = trailState.positions.filter((pos) => {
        // Ensure timestamp is a number for comparison
        const posTimestamp =
          typeof pos.timestamp === "string"
            ? new Date(pos.timestamp).getTime()
            : pos.timestamp;
        return posTimestamp >= cutoffTime;
      });
    }

    // Defer trail visualization to batch multiple updates into one frame
    this.scheduleTrailUpdate(baseCallsign, isHistoricalDot);
  }

  private scheduleTrailUpdate(baseCallsign: string, isHistorical: boolean) {
    // Track which trails need updating; preserve isHistorical=true if set
    const existing = this.pendingTrailUpdates.get(baseCallsign);
    this.pendingTrailUpdates.set(baseCallsign, existing || isHistorical);

    if (this.trailUpdateRafId === null) {
      this.trailUpdateRafId = requestAnimationFrame(() => {
        this.flushPendingTrailUpdates();
      });
    }
  }

  private flushPendingTrailUpdates() {
    this.trailUpdateRafId = null;
    if (this.isDestroyed) return;
    for (const [callsign, isHistorical] of this.pendingTrailUpdates) {
      const trailState = this.trails.get(callsign);
      if (trailState) {
        this.updateTrailVisualization(callsign, trailState, isHistorical);
      }
    }
    this.pendingTrailUpdates.clear();
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
  private calculateDistance(
    lat1: number,
    lng1: number,
    lat2: number,
    lng2: number,
  ): number {
    const R = 6371; // Earth's radius in km
    const dLat = ((lat2 - lat1) * Math.PI) / 180;
    const dLng = ((lng2 - lng1) * Math.PI) / 180;
    const a =
      Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos((lat1 * Math.PI) / 180) *
        Math.cos((lat2 * Math.PI) / 180) *
        Math.sin(dLng / 2) *
        Math.sin(dLng / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    return R * c;
  }

  // Get the average position of a trail
  private getTrailCenter(positions: PositionHistory[]): {
    lat: number;
    lng: number;
  } {
    if (positions.length === 0) return { lat: 0, lng: 0 };

    const sum = positions.reduce(
      (acc, pos) => ({
        lat: acc.lat + pos.lat,
        lng: acc.lng + pos.lng,
      }),
      { lat: 0, lng: 0 },
    );

    return {
      lat: sum.lat / positions.length,
      lng: sum.lng / positions.length,
    };
  }

  // Find nearby trails and get their colors
  private getNearbyTrailColors(
    baseCallsign: string,
    center: { lat: number; lng: number },
  ): Set<string> {
    const nearbyColors = new Set<string>();

    this.trails.forEach((trailState, callsign) => {
      if (
        callsign === baseCallsign ||
        !trailState.color ||
        trailState.positions.length === 0
      )
        return;

      const otherCenter = this.getTrailCenter(trailState.positions);
      const distance = this.calculateDistance(
        center.lat,
        center.lng,
        otherCenter.lat,
        otherCenter.lng,
      );

      if (distance < this.proximityThreshold) {
        nearbyColors.add(trailState.color);
      }
    });

    return nearbyColors;
  }

  // Assign a color to a trail, ensuring global uniqueness when possible
  private assignTrailColor(
    baseCallsign: string,
    positions: PositionHistory[],
  ): string {
    // Collect all colors currently used by other trails
    const usedColors = new Set<string>();
    this.trails.forEach((trailState, callsign) => {
      if (callsign !== baseCallsign && trailState.color) {
        usedColors.add(trailState.color);
      }
    });

    // First priority: pick a color not used by any existing trail
    for (const color of this.colorPalette) {
      if (!usedColors.has(color)) {
        return color;
      }
    }

    // All 15 colors in use — pick one not used by nearby trails
    if (positions.length > 0) {
      const center = this.getTrailCenter(positions);
      const nearbyColors = this.getNearbyTrailColors(baseCallsign, center);
      for (const color of this.colorPalette) {
        if (!nearbyColors.has(color)) {
          return color;
        }
      }
    }

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

    // Create new trail if we have at least 2 positions with valid coordinates
    if (trailState.positions.length >= 2) {
      // Filter out positions with invalid coordinates and create coordinate pairs
      const validPositions: [number, number][] = trailState.positions
        .filter((pos) => {
          return (
            pos &&
            typeof pos.lat === "number" &&
            typeof pos.lng === "number" &&
            !isNaN(pos.lat) &&
            !isNaN(pos.lng) &&
            isFinite(pos.lat) &&
            isFinite(pos.lng) &&
            pos.lat >= -90 &&
            pos.lat <= 90 &&
            pos.lng >= -180 &&
            pos.lng <= 180
          );
        })
        .map((pos) => [pos.lat, pos.lng]);

      // Calculate total path distance to determine if station is actually moving
      let totalDistance = 0;
      for (let i = 1; i < validPositions.length; i++) {
        totalDistance += this.calculateDistance(
          validPositions[i - 1][0],
          validPositions[i - 1][1],
          validPositions[i][0],
          validPositions[i][1],
        );
      }

      // Skip trail if station hasn't moved enough (stationary station)
      if (totalDistance < this.minMovementThreshold) {
        trailState.dots = [];
        return;
      }

      // Break trail into segments at giant jumps (bad data / different igates)
      const segments: [number, number][][] = [[]];
      segments[0].push(validPositions[0]);
      for (let i = 1; i < validPositions.length; i++) {
        const hopDist = this.calculateDistance(
          validPositions[i - 1][0],
          validPositions[i - 1][1],
          validPositions[i][0],
          validPositions[i][1],
        );
        if (hopDist > this.maxHopDistance) {
          // Start a new segment
          segments.push([validPositions[i]]);
        } else {
          segments[segments.length - 1].push(validPositions[i]);
        }
      }

      // Draw only segments with enough points to indicate real movement
      const drawableSegments = segments.filter(
        (seg) => seg.length >= this.minSegmentPoints,
      );

      if (drawableSegments.length > 0) {
        if (!trailState.color) {
          trailState.color = this.assignTrailColor(
            baseCallsign,
            trailState.positions,
          );
        }

        const polylineOptions: PolylineOptions = {
          color: trailState.color,
          weight: 3,
          opacity: immediate ? 0.9 : 0.8,
          smoothFactor: 1,
          lineCap: "round",
          lineJoin: "round",
          className: "historical-trail-line",
        };

        try {
          // L.polyline accepts an array of arrays for multi-segment lines
          trailState.trail = L.polyline(
            drawableSegments,
            polylineOptions,
          ).addTo(this.trailLayer);

          // Attach hover handlers for RF path visualization
          if (this.onTrailHover && this.onTrailHoverEnd) {
            const positions = trailState.positions;
            const hoverCb = this.onTrailHover;
            const hoverEndCb = this.onTrailHoverEnd;

            trailState.trail.on("mousemove", (e: L.LeafletMouseEvent) => {
              const nearest = this.findNearestPositionWithPath(
                e.latlng.lat,
                e.latlng.lng,
                positions,
              );
              if (!nearest) return;

              if (this.trailHoverDebounceTimer) {
                clearTimeout(this.trailHoverDebounceTimer);
              }
              const mouseLat = e.latlng.lat;
              const mouseLng = e.latlng.lng;
              const path = nearest.path!;
              // Only push to server when the path changes; just reposition locally otherwise
              if (path !== this.lastHoveredPath) {
                this.lastHoveredPath = path;
                this.trailHoverDebounceTimer = setTimeout(() => {
                  if (!this.isDestroyed) hoverCb(mouseLat, mouseLng, path);
                }, 50);
              }
            });

            trailState.trail.on("mouseout", () => {
              this.lastHoveredPath = undefined;
              if (this.trailHoverDebounceTimer) {
                clearTimeout(this.trailHoverDebounceTimer);
                this.trailHoverDebounceTimer = undefined;
              }
              hoverEndCb();
            });
          }
        } catch (error) {
          console.error(
            "Error creating trail polyline for",
            baseCallsign,
            ":",
            error,
          );
        }
      }

      trailState.dots = [];
    }
  }

  private findNearestPositionWithPath(
    lat: number,
    lng: number,
    positions: PositionHistory[],
  ): PositionHistory | null {
    let nearest: PositionHistory | null = null;
    let minDist = Infinity;

    for (const pos of positions) {
      if (!pos.path || pos.path.trim() === "") continue;
      const dist = (pos.lat - lat) ** 2 + (pos.lng - lng) ** 2;
      if (dist < minDist) {
        minDist = dist;
        nearest = pos;
      }
    }

    return nearest;
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
    // Clear pending hover timer before removing all trails
    if (this.trailHoverDebounceTimer) {
      clearTimeout(this.trailHoverDebounceTimer);
      this.trailHoverDebounceTimer = undefined;
    }
    this.lastHoveredPath = undefined;
    this.trails.forEach((_, markerId) => {
      this.removeTrail(markerId);
    });
  }

  destroy() {
    this.isDestroyed = true;
    if (this.trailUpdateRafId !== null) {
      cancelAnimationFrame(this.trailUpdateRafId);
      this.trailUpdateRafId = null;
    }
    if (this.trailHoverDebounceTimer) {
      clearTimeout(this.trailHoverDebounceTimer);
      this.trailHoverDebounceTimer = undefined;
    }
    this.pendingTrailUpdates.clear();
  }

  cleanupOldPositions() {
    // Don't clean up historical positions - only clean up very old live positions
    const veryOldCutoff = Date.now() - 24 * 60 * 60 * 1000; // 24 hours
    this.trails.forEach((trailState, markerId) => {
      const originalLength = trailState.positions.length;
      trailState.positions = trailState.positions.filter((pos) => {
        // Ensure timestamp is a number for comparison
        const posTimestamp =
          typeof pos.timestamp === "string"
            ? new Date(pos.timestamp).getTime()
            : pos.timestamp;
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
