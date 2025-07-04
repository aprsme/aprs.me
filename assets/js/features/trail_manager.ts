// Trail management module for APRS position history visualization

export interface PositionHistory {
  lat: number;
  lng: number;
  timestamp: number;
}

export interface TrailState {
  positions: PositionHistory[];
  trail?: any; // L.Polyline
  dots?: any[]; // L.CircleMarker[]
}

export class TrailManager {
  private trailLayer: any; // L.LayerGroup
  private trails: Map<string, TrailState>;
  private showTrails: boolean;
  private trailDuration: number; // in milliseconds

  constructor(trailLayer: any, trailDuration: number = 60 * 60 * 1000) {
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
      this.clearAllTrails();
    } else {
      // When re-enabling trails, recreate all trails immediately
      this.trails.forEach((trailState, baseCallsign) => {
        this.updateTrailVisualization(baseCallsign, trailState, true);
      });
    }
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

  private extractBaseCallsign(markerId: string | any): string {
    // Ensure markerId is a string
    const id = typeof markerId === "string" ? markerId : String(markerId);

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
      const L = (window as any).L;
      const latLngs = trailState.positions.map((pos) => [pos.lat, pos.lng]);

      // Create blue polyline connecting the historical position dots
      // For historical positions (immediate=true), use higher opacity for better visibility
      trailState.trail = L.polyline(latLngs, {
        color: "#1E90FF",
        weight: 3,
        opacity: immediate ? 0.9 : 0.8,
        smoothFactor: 1,
        lineCap: "round",
        lineJoin: "round",
        className: "historical-trail-line",
        // Ensure the trail renders on top of other map elements
        zIndexOffset: immediate ? 1000 : 0,
      }).addTo(this.trailLayer);

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
