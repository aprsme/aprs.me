// Declare Leaflet as a global variable
declare const L: any;

// Import trail management functionality
import { TrailManager } from "./features/trail_manager";

// APRS Map Hook - handles only basic map interaction
// All data logic handled by LiveView

type LiveViewHookContext = {
  el: HTMLElement & { _leaflet_id?: any };
  pushEvent: (event: string, payload: any) => void;
  handleEvent: (event: string, callback: Function) => void;
  map?: any;
  markers?: Map<string, any>;
  markerStates?: Map<string, MarkerState>;
  markerLayer?: any;
  trailManager?: TrailManager;
  boundsTimer?: ReturnType<typeof setTimeout>;
  resizeHandler?: () => void;
  errors?: string[];
  initializationAttempts?: number;
  maxInitializationAttempts?: number;
  lastZoom?: number;
  currentPopupMarkerId?: string | null;
  [key: string]: any;
};

interface MarkerData {
  id: string;
  lat: number;
  lng: number;
  callsign?: string;
  comment?: string;
  symbol_table_id?: string;
  symbol_code?: string;
  symbol_description?: string;
  popup?: string;
  historical?: boolean;
  color?: string;
  timestamp?: number;
  is_most_recent_for_callsign?: boolean;
  callsign_group?: string;
}

interface BoundsData {
  north: number;
  south: number;
  east: number;
  west: number;
}

interface CenterData {
  lat: number;
  lng: number;
}

interface MarkerState {
  lat: number;
  lng: number;
  symbol_table: string;
  symbol_code: string;
  popup?: string;
  historical?: boolean;
  is_most_recent_for_callsign?: boolean;
  callsign_group?: string;
}

interface MapEventData {
  bounds?: BoundsData;
  center?: CenterData;
  zoom?: number;
  id?: string;
  callsign?: string;
  lat?: number;
  lng?: number;
  markers?: MarkerData[];
}

let MapAPRSMap = {
  mounted() {
    const self = this as unknown as LiveViewHookContext;
    // Initialize error tracking
    self.errors = [];
    self.initializationAttempts = 0;
    self.maxInitializationAttempts = 3;

    self.attemptInitialization();
  },

  attemptInitialization() {
    const self = this as unknown as LiveViewHookContext;
    self.initializationAttempts!++;

    // Check if Leaflet is available
    if (typeof L === "undefined") {
      console.error("Leaflet library not loaded!");
      self.errors!.push("Leaflet library not available");

      if (self.initializationAttempts! < self.maxInitializationAttempts!) {
        setTimeout(() => self.attemptInitialization(), 1000);
        return;
      } else {
        self.handleFatalError("Leaflet library failed to load after multiple attempts");
        return;
      }
    }

    // Try to restore from localStorage
    let initialCenter: CenterData, initialZoom: number;
    try {
      const saved = localStorage.getItem("aprs_map_state");
      if (saved) {
        const { lat, lng, zoom } = JSON.parse(saved);
        if (
          typeof lat === "number" &&
          typeof lng === "number" &&
          typeof zoom === "number" &&
          lat >= -90 &&
          lat <= 90 &&
          lng >= -180 &&
          lng <= 180 &&
          zoom >= 1 &&
          zoom <= 20
        ) {
          initialCenter = { lat, lng };
          initialZoom = zoom;
        } else {
          throw new Error("Invalid saved map state");
        }
      } else {
        throw new Error("No saved map state");
      }
    } catch (e) {
      // Fallback to server-provided data attributes
      try {
        const centerData = self.el.dataset.center;
        const zoomData = self.el.dataset.zoom;
        if (!centerData || !zoomData) throw new Error("Missing map data attributes");
        initialCenter = JSON.parse(centerData);
        initialZoom = parseInt(zoomData);
        if (
          !initialCenter ||
          typeof initialCenter.lat !== "number" ||
          typeof initialCenter.lng !== "number"
        )
          throw new Error("Invalid center data");
        if (isNaN(initialZoom) || initialZoom < 1 || initialZoom > 20)
          throw new Error("Invalid zoom data");
      } catch (error) {
        console.error("Error parsing map data attributes:", error);
        initialCenter = { lat: 39.8283, lng: -98.5795 };
        initialZoom = 5;
      }
    }

    self.initializeMap(initialCenter, initialZoom);
  },

  initializeMap(initialCenter: CenterData, initialZoom: number) {
    const self = this as unknown as LiveViewHookContext;
    // Ensure the element and its parent exist
    if (!self.el || !self.el.parentNode) {
      console.warn("Map element or parent not found, retrying...");
      if (self.initializationAttempts! < self.maxInitializationAttempts!) {
        setTimeout(() => self.attemptInitialization(), 500);
        return;
      } else {
        self.handleFatalError("Map element structure invalid");
        return;
      }
    }

    // Check element dimensions
    const rect = self.el.getBoundingClientRect();

    // Validate element has dimensions
    if (rect.width === 0 || rect.height === 0) {
      console.warn("Map element has no dimensions, retrying...");
      self.errors!.push("Map element has zero dimensions");

      // Try to force dimensions
      self.el.style.width = "100%";
      self.el.style.height = "100vh";

      if (self.initializationAttempts! < self.maxInitializationAttempts!) {
        setTimeout(() => self.attemptInitialization(), 500);
        return;
      } else {
        self.handleFatalError("Map element never gained proper dimensions");
        return;
      }
    }

    // Initialize basic map
    try {
      // Check if map is already initialized
      if (self.map) {
        console.warn("Map already exists, reinitializing...");
        self.map.remove();
        self.map = null;
      }

      // Ensure element ID is unique and not already initialized
      if (self.el._leaflet_id) {
        console.warn("Map element already has Leaflet ID, cleaning up...");
        const el = L.DomUtil.get(self.el.id) as HTMLElement | null;
        if (el !== null) {
          L.DomUtil.remove(el);
        }
        delete self.el._leaflet_id;
      }

      self.map = L.map(self.el, {
        zoomControl: true,
        attributionControl: true,
        closePopupOnClick: true,
      }).setView([initialCenter.lat, initialCenter.lng], initialZoom);
    } catch (error) {
      console.error("Error initializing map:", error);
      self.errors!.push(
        "Map initialization failed: " + (error instanceof Error ? error.message : error),
      );

      if (self.initializationAttempts! < self.maxInitializationAttempts!) {
        setTimeout(() => self.attemptInitialization(), 1000);
        return;
      } else {
        self.handleFatalError("Map initialization failed after multiple attempts");
        return;
      }
    }

    // Add OpenStreetMap tile layer
    try {
      const tileLayer = L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
        attribution:
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
        maxZoom: 19,
      });
      tileLayer.addTo(self.map);
    } catch (error) {
      self.errors!.push("Tile layer failed: " + (error instanceof Error ? error.message : error));
    }

    // Store markers for management
    self.markers = new Map<string, any>();
    self.markerLayer = L.layerGroup().addTo(self.map);

    // Create trail layer and manager
    const trailLayer = L.layerGroup().addTo(self.map);
    self.trailManager = new TrailManager(trailLayer);

    // Track marker states to prevent unnecessary operations
    self.markerStates = new Map<string, MarkerState>();

    // Force initial size calculation
    try {
      self.map.invalidateSize();
    } catch (error) {
      console.error("Error invalidating map size:", error);
    }

    // Track when map is ready
    self.map!.whenReady(() => {
      try {
        self.lastZoom = self.map!.getZoom();
        self.pushEvent("map_ready", {});
        self.sendBoundsToServer();

        // Start periodic cleanup of old trail positions (every 5 minutes)
        setInterval(
          () => {
            if (self.trailManager) {
              self.trailManager.cleanupOldPositions();
            }
          },
          5 * 60 * 1000,
        );
      } catch (error) {
        console.error("Error in map ready callback:", error);
      }
    });

    // Send bounds to LiveView when map moves
    self.map!.on("moveend", () => {
      if (self.boundsTimer) clearTimeout(self.boundsTimer);
      self.boundsTimer = setTimeout(() => {
        self.sendBoundsToServer();
        // Save map state
        const center = self.map.getCenter();
        const zoom = self.map.getZoom();
        localStorage.setItem(
          "aprs_map_state",
          JSON.stringify({ lat: center.lat, lng: center.lng, zoom }),
        );
      }, 300);
    });

    // Handle zoom changes with optimization for large zoom differences
    self.map!.on("zoomend", () => {
      if (self.boundsTimer) clearTimeout(self.boundsTimer);
      self.boundsTimer = setTimeout(() => {
        const currentZoom = self.map!.getZoom();
        const zoomDifference = self.lastZoom ? Math.abs(currentZoom - self.lastZoom) : 0;

        // If zoom changed significantly (more than 2 levels), clear and reload
        if (zoomDifference > 2) {
          self.pushEvent("clear_and_reload_markers", {});
        }

        self.sendBoundsToServer();
        self.lastZoom = currentZoom;
        // Save map state
        const center = self.map.getCenter();
        const zoom = self.map.getZoom();
        localStorage.setItem(
          "aprs_map_state",
          JSON.stringify({ lat: center.lat, lng: center.lng, zoom }),
        );
      }, 300);
    });

    // Handle resize
    self.resizeHandler = () => {
      try {
        if (self.map) {
          self.map.invalidateSize();
        }
      } catch (error) {
        console.error("Error invalidating map size on resize:", error);
      }
    };
    window.addEventListener("resize", self.resizeHandler);

    // Add a delayed size check
    setTimeout(() => {
      if (self.el && self.el.parentNode) {
        const rect = self.el.getBoundingClientRect();
        if (self.map && rect.width > 0 && rect.height > 0) {
          try {
            self.map.invalidateSize();
          } catch (error) {
            console.error("Error re-invalidating map size:", error);
          }
        }
      }
    }, 1000);

    // LiveView event handlers
    self.setupLiveViewHandlers();
  },

  handleFatalError(message: string) {
    const self = this as unknown as LiveViewHookContext;
    console.error("Fatal map error:", message);
    console.error("All errors:", self.errors);

    // Display error message to user
    if (self.el) {
      self.el.innerHTML = `
        <div style="
          display: flex;
          justify-content: center;
          align-items: center;
          height: 100%;
          background: #f8f9fa;
          color: #721c24;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          text-align: center;
          padding: 20px;
        ">
          <div>
            <h3 style="margin: 0 0 10px 0;">Map Loading Error</h3>
            <p style="margin: 0; font-size: 14px;">${message}</p>
            <p style="margin: 10px 0 0 0; font-size: 12px; color: #856404;">
              Please refresh the page or check the browser console for details.
            </p>
          </div>
        </div>
      `;
    }
  },

  setupLiveViewHandlers() {
    const self = this as unknown as LiveViewHookContext;
    // Add single marker
    self.handleEvent("add_marker", (data: MarkerData) => {
      self.addMarker(data);
    });

    // Add multiple markers at once
    self.handleEvent("add_markers", (data: { markers: MarkerData[] }) => {
      if (data.markers && Array.isArray(data.markers)) {
        data.markers.forEach((marker) => self.addMarker(marker));
      }
    });

    // Remove marker
    self.handleEvent("remove_marker", (data: { id: string }) => {
      self.removeMarker(data.id);
    });

    // Clear all markers
    self.handleEvent("clear_markers", () => {
      self.clearAllMarkers();
    });

    // Update marker
    self.handleEvent("update_marker", (data: MarkerData) => {
      self.updateMarker(data);
    });

    // Zoom to location
    self.handleEvent("zoom_to_location", (data: { lat: number; lng: number; zoom?: number }) => {
      if (!self.map) {
        console.error("Map not initialized, cannot zoom");
        return;
      }

      if (data.lat && data.lng) {
        const lat = parseFloat(data.lat.toString());
        const lng = parseFloat(data.lng.toString());
        const zoom = parseInt(data.zoom?.toString() || "12");

        // Validate coordinates
        if (isNaN(lat) || isNaN(lng) || lat < -90 || lat > 90 || lng < -180 || lng > 180) {
          console.error("Invalid coordinates for zoom:", lat, lng);
          return;
        }

        if (isNaN(zoom) || zoom < 1 || zoom > 20) {
          console.error("Invalid zoom level:", zoom);
          return;
        }

        try {
          // Check element dimensions before zoom
          const beforeRect = self.el.getBoundingClientRect();

          // Force map size recalculation before zoom
          self.map.invalidateSize();

          // Use a slight delay to ensure map is ready
          setTimeout(() => {
            if (self.map) {
              self.map.setView([lat, lng], zoom, {
                animate: true,
                duration: 1,
              });

              // Check element dimensions after zoom
              setTimeout(() => {
                const afterRect = self.el.getBoundingClientRect();

                if (afterRect.width === 0 || afterRect.height === 0) {
                  console.error("Map element lost dimensions after zoom!");
                  // Try to restore dimensions
                  self.el.style.width = "100vw";
                  self.el.style.height = "100vh";
                  self.map.invalidateSize();
                }
              }, 1000);
            }
          }, 100);
        } catch (error) {
          console.error("Error during zoom operation:", error);
        }
      } else {
        console.warn("Missing lat/lng data for zoom operation:", data);
      }
    });

    // Handle geolocation requests
    self.handleEvent("request_geolocation", () => {
      if ("geolocation" in navigator) {
        navigator.geolocation.getCurrentPosition(
          (position) => {
            const { latitude, longitude } = position.coords;
            self.pushEvent("set_location", { lat: latitude, lng: longitude });
          },
          (error) => {
            console.warn("Geolocation error:", error.message);
            self.pushEvent("geolocation_error", { error: error.message });
          },
        );
      } else {
        console.warn("Geolocation not available");
        self.pushEvent("geolocation_error", { error: "Geolocation not supported" });
      }
    });

    // Toggle trails visibility
    self.handleEvent("toggle_trails", (data: { show: boolean }) => {
      if (self.trailManager) {
        self.trailManager.setShowTrails(data.show);
      }
    });

    // Handle new packets from LiveView
    self.handleEvent("new_packet", (data: MarkerData) => {
      self.addMarker({
        ...data,
        historical: false,
        popup: self.buildPopupContent(data),
        openPopup: true,
      });
    });

    // Handle highlighting the latest packet (open its popup)
    self.currentPopupMarkerId = null;
    self.handleEvent("highlight_packet", (data: { id: string }) => {
      if (!data.id) return;
      // Close previous popup if open
      if (self.currentPopupMarkerId && self.markers!.has(self.currentPopupMarkerId)) {
        const prevMarker = self.markers!.get(self.currentPopupMarkerId);
        if (prevMarker && prevMarker.closePopup) prevMarker.closePopup();
      }
      // Re-add marker with openPopup flag (will open after add)
      const markerData = self.markerStates!.get(data.id);
      if (markerData) {
        self.addMarker({ ...markerData, id: data.id, openPopup: true });
      } else {
        // fallback: try to open popup directly if marker exists
        const marker = self.markers!.get(data.id);
        if (marker && marker.openPopup) marker.openPopup();
      }
      self.currentPopupMarkerId = data.id;
    });

    // Handle historical packets during replay
    self.handleEvent("historical_packet", (data: MarkerData) => {
      self.addMarker({
        ...data,
        historical: true,
        popup: self.buildPopupContent(data),
      });
    });

    // Handle bulk loading of historical packets
    self.handleEvent("add_historical_packets", (data: { packets: MarkerData[] }) => {
      if (data.packets && Array.isArray(data.packets)) {
        // Group packets by callsign to process them in chronological order for proper trail drawing
        const packetsByCallsign = new Map<string, MarkerData[]>();

        data.packets.forEach((packet) => {
          const callsign = packet.callsign_group || packet.callsign || packet.id;
          if (!packetsByCallsign.has(callsign)) {
            packetsByCallsign.set(callsign, []);
          }
          packetsByCallsign.get(callsign)!.push(packet);
        });

        // Process each callsign group in chronological order (oldest first)
        packetsByCallsign.forEach((packets, callsign) => {
          // Sort by timestamp (oldest first) to ensure proper trail line drawing
          const sortedPackets = packets.sort((a, b) => {
            const timeA = a.timestamp
              ? typeof a.timestamp === "number"
                ? a.timestamp
                : new Date(a.timestamp).getTime()
              : 0;
            const timeB = b.timestamp
              ? typeof b.timestamp === "number"
                ? b.timestamp
                : new Date(b.timestamp).getTime()
              : 0;
            return timeA - timeB;
          });

          // Add markers in chronological order
          sortedPackets.forEach((packet) => {
            self.addMarker({
              ...packet,
              historical: true,
              popup: self.buildPopupContent(packet),
            });
          });
        });
      }
    });

    // Handle refresh markers event
    self.handleEvent("refresh_markers", () => {
      // Remove markers that are outside current bounds
      if (self.map) {
        const bounds = self.map.getBounds();
        self.removeMarkersOutsideBounds(bounds);
      }
    });

    // Handle clearing historical packets
    self.handleEvent("clear_historical_packets", () => {
      // Remove all historical markers
      const markersToRemove: string[] = [];
      self.markers!.forEach((marker: any, id: any) => {
        if ((marker as any)._isHistorical) {
          markersToRemove.push(String(id));
        }
      });
      markersToRemove.forEach((id) => self.removeMarker(id));
    });

    // Handle bounds-based marker filtering
    self.handleEvent("filter_markers_by_bounds", (data: { bounds: BoundsData }) => {
      if (data.bounds) {
        // Create Leaflet bounds object from server data
        const bounds = L.latLngBounds(
          [data.bounds.south, data.bounds.west],
          [data.bounds.north, data.bounds.east],
        );
        self.removeMarkersOutsideBounds(bounds);
      }
    });

    // Handle clearing all markers and reloading visible ones
    self.handleEvent("clear_and_reload_markers", () => {
      // This event is just a trigger - the server will handle clearing and adding markers
    });
  },

  sendBoundsToServer() {
    const self = this as unknown as LiveViewHookContext;
    if (!self.map) return;

    const bounds = self.map!.getBounds();
    const center = self.map!.getCenter();
    const zoom = self.map!.getZoom();

    // Remove markers that are now outside the visible bounds
    self.removeMarkersOutsideBounds(bounds);

    self.pushEvent("bounds_changed", {
      bounds: {
        north: bounds.getNorth(),
        south: bounds.getSouth(),
        east: bounds.getEast(),
        west: bounds.getWest(),
      },
      center: {
        lat: center.lat,
        lng: center.lng,
      },
      zoom: zoom,
    });
  },

  addMarker(data: MarkerData & { openPopup?: boolean }) {
    const self = this as unknown as LiveViewHookContext;
    if (!data.id || !data.lat || !data.lng) {
      console.warn("Invalid marker data:", data);
      return;
    }

    const lat = parseFloat(data.lat.toString());
    const lng = parseFloat(data.lng.toString());

    // Validate coordinates
    if (isNaN(lat) || isNaN(lng) || lat < -90 || lat > 90 || lng < -180 || lng > 180) {
      console.warn("Invalid coordinates:", lat, lng);
      return;
    }

    // Check if marker already exists with same position and data
    const existingMarker = self.markers!.get(data.id);
    const existingState = self.markerStates!.get(data.id);

    if (existingMarker && existingState) {
      // Check if marker needs updating
      const currentPos = existingMarker.getLatLng();
      const positionChanged =
        Math.abs(currentPos.lat - lat) > 0.0001 || Math.abs(currentPos.lng - lng) > 0.0001;
      const dataChanged =
        existingState.symbol_table !== data.symbol_table_id ||
        existingState.symbol_code !== data.symbol_code ||
        existingState.popup !== data.popup;

      if (positionChanged && self.trailManager) {
        // Position changed, update trail
        const isHistoricalDot = data.historical && !data.is_most_recent_for_callsign;
        const timestamp = data.timestamp
          ? typeof data.timestamp === "string"
            ? new Date(data.timestamp).getTime()
            : data.timestamp
          : Date.now();
        self.trailManager.addPosition(data.id, lat, lng, timestamp, isHistoricalDot);
      }

      if (!positionChanged && !dataChanged) {
        // No changes needed, skip update
        // But if openPopup is requested, open it
        if (data.openPopup && existingMarker.openPopup) {
          existingMarker.openPopup();
        }
        return;
      }
    }

    // Remove existing marker if it exists
    self.removeMarker(data.id);

    // Create marker - use simple dot for older historical positions, APRS icon for most recent
    const marker = L.marker([lat, lng], {
      icon: self.createMarkerIcon(data),
    });

    // Add popup if content provided
    if (data.popup) {
      marker.bindPopup(data.popup);
    }

    // Handle marker click
    marker.on("click", () => {
      if (marker.openPopup) marker.openPopup();
      self.pushEvent("marker_clicked", {
        id: data.id,
        callsign: data.callsign,
        lat: lat,
        lng: lng,
      });
    });

    // Mark historical markers for identification
    if (data.historical) {
      (marker as any)._isHistorical = true;
    }

    // Add to map and store reference
    marker.addTo(self.markerLayer!);
    self.markers!.set(data.id, marker);

    // Store marker state for optimization
    self.markerStates!.set(data.id, {
      lat: lat,
      lng: lng,
      symbol_table: data.symbol_table_id || "/",
      symbol_code: data.symbol_code || ">",
      popup: data.popup,
      historical: data.historical,
      is_most_recent_for_callsign: data.is_most_recent_for_callsign,
      callsign_group: data.callsign_group,
    });

    // Initialize trail for new marker - always add to trail for line drawing
    if (self.trailManager) {
      const isHistoricalDot = data.historical && !data.is_most_recent_for_callsign;
      const timestamp = data.timestamp
        ? typeof data.timestamp === "string"
          ? new Date(data.timestamp).getTime()
          : data.timestamp
        : Date.now();
      self.trailManager.addPosition(data.id, lat, lng, timestamp, isHistoricalDot);
    }

    // Open popup if requested
    if (data.openPopup && marker.openPopup) {
      marker.openPopup();
    }
  },

  removeMarker(id: string | any) {
    const self = this as unknown as LiveViewHookContext;
    // Ensure id is a string
    const markerId = typeof id === "string" ? id : String(id);

    const marker = self.markers!.get(markerId);
    if (marker) {
      self.markerLayer!.removeLayer(marker);
      self.markers!.delete(markerId);
      self.markerStates!.delete(markerId);
    }

    // Remove trail
    if (self.trailManager) {
      self.trailManager.removeTrail(markerId);
    }
  },

  updateMarker(data: MarkerData) {
    const self = this as unknown as LiveViewHookContext;
    if (!data.id) return;

    const existingMarker = self.markers!.get(data.id);
    if (existingMarker) {
      // Update position if provided
      if (data.lat && data.lng) {
        const lat = parseFloat(data.lat.toString());
        const lng = parseFloat(data.lng.toString());
        if (!isNaN(lat) && !isNaN(lng)) {
          const currentPos = existingMarker.getLatLng();
          const positionChanged =
            Math.abs(currentPos.lat - lat) > 0.0001 || Math.abs(currentPos.lng - lng) > 0.0001;

          if (positionChanged) {
            existingMarker.setLatLng([lat, lng]);
            if (self.trailManager) {
              const isHistoricalDot = data.historical && !data.is_most_recent_for_callsign;
              const timestamp = data.timestamp
                ? typeof data.timestamp === "string"
                  ? new Date(data.timestamp).getTime()
                  : data.timestamp
                : Date.now();
              self.trailManager.addPosition(data.id, lat, lng, timestamp, isHistoricalDot);
            }
          }
        }
      }

      // Update popup if provided
      if (data.popup) {
        existingMarker.setPopupContent(data.popup);
      }
    } else {
      // Marker doesn't exist, create it
      self.addMarker(data);
    }
  },

  clearAllMarkers() {
    const self = this as unknown as LiveViewHookContext;
    self.markerLayer!.clearLayers();
    self.markers!.clear();
    self.markerStates!.clear();

    // Clear all trails
    if (self.trailManager) {
      self.trailManager.clearAllTrails();
    }
  },

  removeMarkersOutsideBounds(bounds: L.LatLngBounds) {
    const self = this as unknown as LiveViewHookContext;
    if (!bounds || !self.markers) return;

    const markersToRemove: string[] = [];

    self.markers!.forEach((marker: L.Marker, id: string) => {
      const position = marker.getLatLng();
      const lat = position.lat;
      const lng = position.lng;

      // Check latitude bounds (straightforward)
      const latOutOfBounds = lat < bounds.getSouth() || lat > bounds.getNorth();

      // Check longitude bounds (handle potential wrapping)
      let lngOutOfBounds: boolean;
      if (bounds.getWest() <= bounds.getEast()) {
        // Normal case: bounds don't cross antimeridian
        lngOutOfBounds = lng < bounds.getWest() || lng > bounds.getEast();
      } else {
        // Bounds cross antimeridian (e.g., west=170, east=-170)
        lngOutOfBounds = lng < bounds.getWest() && lng > bounds.getEast();
      }

      // Check if marker is outside the current bounds
      if (latOutOfBounds || lngOutOfBounds) {
        markersToRemove.push(id);
      }
    });

    // Remove out-of-bounds markers
    markersToRemove.forEach((id) => self.removeMarker(String(id)));
  },

  createMarkerIcon(data: MarkerData): L.DivIcon {
    // For historical packets that are not the most recent for their callsign,
    // show a simple red dot (only positions where lat/lon actually changed)
    if (data.historical && !data.is_most_recent_for_callsign) {
      const iconHtml = `<div style="
        width: 8px;
        height: 8px;
        background-color: #FF6B6B;
        border: 2px solid #FFFFFF;
        border-radius: 50%;
        opacity: 0.8;
        box-shadow: 0 0 2px rgba(0,0,0,0.3);
      " title="Historical position for ${data.callsign} (position changed)"></div>`;

      return L.divIcon({
        html: iconHtml,
        className: "historical-dot-marker",
        iconSize: [12, 12],
        iconAnchor: [6, 6],
      });
    }

    // For current packets or most recent historical packets, show the full APRS symbol icon
    const symbolTableId = data.symbol_table_id || "/";
    const symbolCode = getValidSymbolCode(data.symbol_code, symbolTableId);

    // Map symbol table identifier to correct table index per hessu/aprs-symbols
    // Primary table: / (0)
    // Alternate table: \ (1)
    // Overlay table: ] (2)
    // Any other character is treated as alternate table (1)
    const tableMap: Record<string, string> = {
      "/": "0", // Primary table
      "\\": "1", // Alternate table
      "]": "2", // Overlay table
    };
    const tableId = symbolTableId === "/" ? "0" : symbolTableId === "]" ? "2" : "1";
    const spriteFile = `/aprs-symbols/aprs-symbols-128-${tableId}@2x.png`;

    // Calculate sprite position per hessu/aprs-symbols
    const charCode = symbolCode.charCodeAt(0);
    const index = charCode - 33; // ASCII printable characters start at 33 (!)
    // Clamp to valid range (0-93 for printable ASCII 33-126)
    const safeIndex = Math.max(0, Math.min(index, 93));
    const column = safeIndex % 16;
    const row = Math.floor(safeIndex / 16);
    const x = -column * 128;
    const y = -row * 128;

    // Create the HTML string directly to ensure proper style application
    const iconHtml = `<div style="
      width: 32px;
      height: 32px;
      background-image: url(${spriteFile});
      background-position: ${x / 4}px ${y / 4}px;
      background-size: 512px 192px;
      background-repeat: no-repeat;
      image-rendering: pixelated;
      opacity: ${data.historical && data.is_most_recent_for_callsign ? "0.9" : "1.0"};
      overflow: hidden;
    " data-symbol-table="${symbolTableId}" data-symbol-code="${symbolCode}" data-sprite-position="${x},${y}" data-expected-index="${index}" title="Symbol: ${symbolCode} (${charCode}) at row ${row}, col ${column}"></div>`;

    return L.divIcon({
      html: iconHtml,
      className: "", // Remove class to avoid CSS conflicts
      iconSize: [32, 32],
      iconAnchor: [16, 16],
    });
  },

  buildPopupContent(data: MarkerData): string {
    const callsign = data.callsign || data.id || "Unknown";
    const comment = data.comment || "";
    // const symbolTableId = data.symbol_table_id || "/";
    // const symbolCode = data.symbol_code || ">";
    // const symbolDesc = data.symbol_description || `Symbol: ${symbolTableId}${symbolCode}`;

    let content = `<div class="aprs-popup">
      <div class="aprs-callsign"><strong><a href="/${callsign}">${callsign}</a></strong></div>`;
    // Removed symbol info from popup
    // content += `<div class="aprs-symbol-info">${symbolDesc}</div>`;

    if (comment) {
      content += `<div class="aprs-comment">${comment}</div>`;
    }

    if (data.lat && data.lng) {
      content += `<div class="aprs-coords">
        ${data.lat.toFixed(4)}, ${data.lng.toFixed(4)}
      </div>`;
    }

    if (data.timestamp) {
      let date;
      if (typeof data.timestamp === "number") {
        date = new Date(data.timestamp * 1000);
      } else if (typeof data.timestamp === "string") {
        date = new Date(data.timestamp);
      }

      if (date && !isNaN(date.getTime())) {
        content += `<div class="aprs-timestamp">${date.toISOString()}</div>`;
      }
    }

    content += "</div>";
    return content;
  },

  destroyed() {
    const self = this as unknown as LiveViewHookContext;
    if (self.boundsTimer !== undefined) {
      clearTimeout(self.boundsTimer);
    }
    if (self.resizeHandler !== undefined) {
      window.removeEventListener("resize", self.resizeHandler);
    }
    if (self.markerLayer !== undefined) {
      self.markerLayer!.clearLayers();
    }
    if (self.markers !== undefined) {
      self.markers!.clear();
    }
    if (self.markerStates !== undefined) {
      self.markerStates!.clear();
    }
    if (self.map !== undefined) {
      self.map!.remove();
      self.map = undefined;
    }
  },
};

// Helper to validate and fallback symbol code per aprs.fi logic
function getValidSymbolCode(
  symbolCode: string | undefined,
  symbolTableId: string | undefined,
): string {
  if (!symbolCode || symbolCode.length !== 1) {
    return symbolTableId === "\\" ? "O" : ">";
  }
  const code = symbolCode.charCodeAt(0);
  if (code < 33 || code > 126) {
    return symbolTableId === "\\" ? "O" : ">";
  }
  return symbolCode;
}

export default MapAPRSMap;
