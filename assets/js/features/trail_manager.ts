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

  addPosition(markerId: string, lat: number, lng: number, timestamp: number) {
    if (!this.showTrails) return;

    let trailState = this.trails.get(markerId);
    if (!trailState) {
      trailState = { positions: [] };
      this.trails.set(markerId, trailState);
    }

    // Only add if position is different from the last one
    const lastPos = trailState.positions[trailState.positions.length - 1];
    if (!lastPos || Math.abs(lastPos.lat - lat) > 0.0001 || Math.abs(lastPos.lng - lng) > 0.0001) {
      trailState.positions.push({ lat, lng, timestamp });
    }

    // Filter positions to keep only recent ones
    const cutoffTime = Date.now() - this.trailDuration;
    trailState.positions = trailState.positions.filter((pos) => pos.timestamp >= cutoffTime);

    this.updateTrailVisualization(markerId, trailState);
  }

  private updateTrailVisualization(markerId: string, trailState: TrailState) {
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

      // Create polyline for the trail
      trailState.trail = L.polyline(latLngs, {
        color: "#1E90FF",
        weight: 3,
        opacity: 0.5,
        smoothFactor: 1,
      }).addTo(this.trailLayer);

      // Create dots for each position
      trailState.dots = trailState.positions.map((pos, index) => {
        const age = (Date.now() - pos.timestamp) / this.trailDuration;
        const opacity = Math.max(0.4, 1 - age * 0.6);
        const isCurrentPosition = index === trailState.positions.length - 1;
        const radius = isCurrentPosition ? 5 : 3;

        const dot = L.circleMarker([pos.lat, pos.lng], {
          radius: radius,
          fillColor: "#FF4500",
          color: "#8B0000",
          weight: 1,
          opacity: opacity,
          fillOpacity: opacity * 0.8,
        });

        // Add tooltip with timestamp
        const time = new Date(pos.timestamp);
        dot.bindTooltip(time.toLocaleTimeString(), {
          permanent: false,
          direction: "top",
          className: "trail-tooltip",
        });

        return dot.addTo(this.trailLayer);
      });
    }
  }

  removeTrail(markerId: string) {
    const trailState = this.trails.get(markerId);
    if (trailState) {
      if (trailState.trail) {
        this.trailLayer.removeLayer(trailState.trail);
      }
      if (trailState.dots) {
        trailState.dots.forEach((dot) => this.trailLayer.removeLayer(dot));
      }
      this.trails.delete(markerId);
    }
  }

  clearAllTrails() {
    this.trails.forEach((_, markerId) => {
      this.removeTrail(markerId);
    });
  }

  cleanupOldPositions() {
    const cutoffTime = Date.now() - this.trailDuration;
    this.trails.forEach((trailState, markerId) => {
      trailState.positions = trailState.positions.filter((pos) => pos.timestamp >= cutoffTime);
      if (trailState.positions.length === 0) {
        this.removeTrail(markerId);
      } else {
        this.updateTrailVisualization(markerId, trailState);
      }
    });
  }
}
