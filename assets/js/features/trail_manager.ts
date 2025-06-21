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
  }

  setShowTrails(show: boolean) {
    this.showTrails = show;
    if (!show) {
      this.clearAllTrails();
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

    // Only add if position is different from the last one (more than ~1 meter)
    const lastPos = trailState.positions[trailState.positions.length - 1];
    if (
      !lastPos ||
      Math.abs(lastPos.lat - lat) > 0.00001 ||
      Math.abs(lastPos.lng - lng) > 0.00001
    ) {
      trailState.positions.push({ lat, lng, timestamp });
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

    // Always update trail visualization
    this.updateTrailVisualization(baseCallsign, trailState);
  }

  private extractBaseCallsign(markerId: string | any): string {
    // Ensure markerId is a string
    const id = typeof markerId === "string" ? markerId : String(markerId);

    // Extract base callsign from various ID formats
    if (id.startsWith("hist_")) {
      // Remove hist_ prefix and any trailing index
      return id.replace(/^hist_/, "").replace(/_\d+$/, "");
    }
    return id;
  }

  private updateTrailVisualization(baseCallsign: string, trailState: TrailState) {
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
      trailState.trail = L.polyline(latLngs, {
        color: "#1E90FF",
        weight: 3,
        opacity: 0.8,
        smoothFactor: 1,
        lineCap: "round",
        lineJoin: "round",
        className: "historical-trail-line",
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
