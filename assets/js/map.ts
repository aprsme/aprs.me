// Import type definitions
import type { 
  LiveViewHookContext, 
  MarkerData, 
  BoundsData, 
  CenterData, 
  MarkerState, 
  MapEventData 
} from './types/map';
import type { Map as LeafletMap, Marker, TileLayer, LayerGroup, DivIcon, LatLngBounds } from 'leaflet';

// Declare Leaflet as a global variable with proper typing
declare const L: typeof import('leaflet') & {
  heatLayer?: (latlngs: Array<[number, number, number?]>, options?: any) => any;
};
declare const OverlappingMarkerSpiderfier: any;

// Import trail management functionality
import { TrailManager } from "./features/trail_manager";
// Import helper functions
import { parseTimestamp, getTrailId, saveMapState, safePushEvent, getLiveSocket } from "./map_helpers";

// APRS Map Hook - handles only basic map interaction
// All data logic handled by LiveView


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

    // Initialize with URL parameters first (from data attributes)
    let initialCenter: CenterData, initialZoom: number;
    let useUrlParams = false;
    
    try {
      const centerData = self.el.dataset.center;
      const zoomData = self.el.dataset.zoom;
      console.log("Map data attributes - center:", centerData, "zoom:", zoomData);
      
      if (!centerData || !zoomData) throw new Error("Missing map data attributes");
      initialCenter = JSON.parse(centerData);
      initialZoom = parseInt(zoomData);
      
      console.log("Parsed initial values - center:", initialCenter, "zoom:", initialZoom);
      
      if (
        !initialCenter ||
        typeof initialCenter.lat !== "number" ||
        typeof initialCenter.lng !== "number"
      )
        throw new Error("Invalid center data");
      if (isNaN(initialZoom) || initialZoom < 1 || initialZoom > 20)
        throw new Error("Invalid zoom data");
        
      // Check if URL has explicit parameters (not default values)
      const urlParams = new URLSearchParams(window.location.search);
      useUrlParams = urlParams.has('lat') || urlParams.has('lng') || urlParams.has('z');
    } catch (error) {
      console.error("Error parsing map data attributes:", error);
      initialCenter = { lat: 39.8283, lng: -98.5795 };
      initialZoom = 5;
    }
    
    // Only use localStorage if no URL params are present
    if (!useUrlParams) {
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
            console.log("Using saved state from localStorage:", { lat, lng, zoom });
            initialCenter = { lat, lng };
            initialZoom = zoom;
          }
        }
      } catch (e) {
        console.log("Could not load from localStorage:", e);
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

    // Create heat layer (hidden by default)
    try {
      if (L.heatLayer) {
        self.heatLayer = L.heatLayer([], {
          radius: 25,
          blur: 15,
          maxZoom: 8,
          gradient: {
            0.4: 'blue',
            0.65: 'lime',
            0.85: 'yellow',
            1.0: 'red'
          }
        });
      }
    } catch (error) {
      console.error("Error creating heat layer:", error);
    }

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
        
        // Ensure we have a valid pushEvent function before using it
        if (self.pushEvent && typeof self.pushEvent === 'function') {
          console.log("Map ready - sending map_ready event");
          self.pushEvent("map_ready", {});
          // Send initial bounds to trigger historical loading
          console.log("Sending initial bounds to server");
          self.sendBoundsToServer();
          
          // Also send update_map_state to ensure URL updates and bounds processing
          // Increase delay to ensure LiveView is fully connected and ready
          setTimeout(() => {
            if (self.map && self.pushEvent && !self.isDestroyed) {
              console.log("Sending initial update_map_state for historical loading");
              saveMapState(self.map, (event: string, payload: any) => self.pushEvent(event, payload));
            }
          }, 500);
        } else {
          console.warn("pushEvent not available in whenReady callback");
          // Retry after a short delay
          setTimeout(() => {
            if (self.pushEvent && typeof self.pushEvent === 'function' && !self.isDestroyed) {
              self.pushEvent("map_ready", {});
              self.sendBoundsToServer();
              // Also trigger map state update after a delay
              setTimeout(() => {
                if (self.map && self.pushEvent && !self.isDestroyed) {
                  console.log("Sending initial update_map_state for historical loading (retry path)");
                  saveMapState(self.map, (event: string, payload: any) => self.pushEvent(event, payload));
                }
              }, 500);
            }
          }, 200);
        }

        // Start periodic cleanup of old trail positions (every 5 minutes)
        self.cleanupInterval = setInterval(
          () => {
            if (!self.isDestroyed && self.trailManager) {
              self.trailManager.cleanupOldPositions();
            }
          },
          5 * 60 * 1000,
        );
      } catch (error) {
        console.error("Error in map ready callback:", error);
      }
    });

    // Track map event handlers for cleanup
    self.mapEventHandlers = new Map();
    self.isDestroyed = false;

    // Send bounds to LiveView when map moves
    const moveEndHandler = () => {
      // Skip if this is a programmatic move from the server
      if (self.programmaticMoveId) {
        return;
      }
      
      if (self.boundsTimer) clearTimeout(self.boundsTimer);
      self.boundsTimer = setTimeout(() => {
        if (self.map && !self.isDestroyed) {
          saveMapState(self.map, (event: string, payload: any) => self.pushEvent(event, payload));
        }
      }, 300);
    };
    self.map!.on("moveend", moveEndHandler);
    self.mapEventHandlers!.set("moveend", moveEndHandler);

    // Handle zoom changes with optimization for large zoom differences
    const zoomEndHandler = () => {
      // Skip if this is a programmatic move from the server
      if (self.programmaticMoveId) {
        return;
      }
      
      if (self.boundsTimer) clearTimeout(self.boundsTimer);
      self.boundsTimer = setTimeout(() => {
        const currentZoom = self.map!.getZoom();
        const zoomDifference = self.lastZoom ? Math.abs(currentZoom - self.lastZoom) : 0;

        // If zoom changed significantly (more than 2 levels), request reload of normal markers
        // but preserve historical ones and current position
        if (zoomDifference > 2) {
          self.pushEvent("refresh_markers", {});
        }

        self.lastZoom = currentZoom;
        // Save map state and update URL
        if (self.map && !self.isDestroyed) {
          saveMapState(self.map, (event: string, payload: any) => self.pushEvent(event, payload));
        }
      }, 300);
    };
    self.map!.on("zoomend", zoomEndHandler);
    self.mapEventHandlers!.set("zoomend", zoomEndHandler);

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

    // Set up event delegation for popup navigation links
    self.setupPopupNavigation();

    if (typeof OverlappingMarkerSpiderfier !== "undefined") {
      self.oms = new OverlappingMarkerSpiderfier(self.map);
    }
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

  setupPopupNavigation() {
    const self = this as unknown as LiveViewHookContext;
    
    // Store the event handler so we can remove it later
    self.popupNavigationHandler = (e: Event) => {
      const target = e.target as HTMLElement;
      
      // Check if clicked element or its parent is a LiveView navigation link
      const navLink = target.closest('.aprs-lv-link') as HTMLAnchorElement;
      
      if (navLink && navLink.href) {
        e.preventDefault();
        e.stopPropagation();
        
        // Use Phoenix LiveView's built-in navigation
        const liveSocket = getLiveSocket();
        if (liveSocket) {
          liveSocket.pushHistoryPatch(navLink.href, "push", navLink);
        } else {
          // Fallback to regular navigation if LiveView socket not available
          window.location.href = navLink.href;
        }
      }
    };
    
    // Use event delegation to handle clicks on popup navigation links
    document.addEventListener('click', self.popupNavigationHandler);
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
              // Generate a unique ID for this programmatic move
              const moveId = `move_${Date.now()}_${Math.random()}`;
              self.programmaticMoveId = moveId;
              
              // Clear any existing timeout
              if (self.programmaticMoveTimeout) {
                clearTimeout(self.programmaticMoveTimeout);
              }
              
              // Set a timeout to clear the programmatic move flag
              // This ensures we don't block user interactions indefinitely
              self.programmaticMoveTimeout = setTimeout(() => {
                if (self.programmaticMoveId === moveId) {
                  self.programmaticMoveId = undefined;
                }
              }, 1500);
              
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
      try {
        // Skip if context is lost
        if (!self || !self.map || self.isDestroyed) {
          console.warn("Map context not ready or destroyed, skipping new packet");
          return;
        }
        
        // Check if map exists and has the hasLayer method
        if (!self.map.hasLayer) {
          console.warn("Map hasLayer method not available, skipping new packet");
          return;
        }
        
        // If heat layer is visible, we're in heat map mode - skip individual markers
        if (self.heatLayer && self.map.hasLayer(self.heatLayer)) {
          console.log("In heat map mode, skipping individual marker for new packet");
          return;
        }
        
        // Check if there's already a marker for this callsign
        const incomingCallsign = data.callsign_group || data.callsign || data.id;

      if (incomingCallsign) {
        // Find existing live markers for this callsign and convert them to historical dots
        const markersToConvert: string[] = [];

        // Ensure markerStates exists
        if (!self.markerStates) {
          console.warn("markerStates not initialized, skipping conversion");
          return;
        }

        self.markerStates.forEach((state, id) => {
          const stateCallsign = state.callsign_group || state.callsign;
          // Only convert non-historical markers for the same callsign, but exclude the incoming packet's ID
          if (
            stateCallsign === incomingCallsign &&
            !state.historical &&
            String(id) !== String(data.id)
          ) {
            markersToConvert.push(String(id));
          }
        });

        // Convert existing live markers to historical dots by updating their icon
        markersToConvert.forEach((id) => {
          if (!self.markers || !self.markerStates) {
            console.warn("markers or markerStates not available during conversion");
            return;
          }
          const existingMarker = self.markers.get(id);
          const existingState = self.markerStates.get(id);

          if (existingMarker && existingState) {
            // Update the state to historical
            existingState.historical = true;
            existingState.is_most_recent_for_callsign = false;

            // Create new historical dot icon
            const historicalIconData = {
              id: String(id),
              lat: existingState.lat,
              lng: existingState.lng,
              callsign: existingState.callsign || incomingCallsign,
              callsign_group: existingState.callsign_group || incomingCallsign,
              symbol_table_id: existingState.symbol_table,
              symbol_code: existingState.symbol_code,
              historical: true,
              is_most_recent_for_callsign: false,
              popup: existingState.popup,
            };

            // Update the marker's icon to show as a dot
            existingMarker.setIcon(self.createMarkerIcon(historicalIconData));

            // Mark as historical
            (existingMarker as any)._isHistorical = true;
          }
        });
      }

      // Add the new marker as the most recent for this callsign
      // Check if openPopup is explicitly set to false to prevent interrupting user
      const shouldOpenPopup = data.openPopup !== false;
      self.addMarker({
        ...data,
        historical: false,
        is_most_recent_for_callsign: true,
        callsign_group: data.callsign_group || data.callsign || incomingCallsign,
        popup: data.popup || self.buildPopupContent(data),
        openPopup: shouldOpenPopup,
      });
      } catch (error) {
        console.error("Error in new_packet handler:", error);
      }
    });

    // Handle highlighting the latest packet (open its popup)
    self.currentPopupMarkerId = null;
    self.handleEvent("highlight_packet", (data: { id: string }) => {
      if (!data.id || !self.markers || !self.markerStates) return;
      
      // Close previous popup if open
      if (self.currentPopupMarkerId && self.markers.has(self.currentPopupMarkerId)) {
        const prevMarker = self.markers.get(self.currentPopupMarkerId);
        if (prevMarker && prevMarker.closePopup) prevMarker.closePopup();
      }
      // Try to open popup directly first if marker exists
      const marker = self.markers.get(data.id);
      if (marker && marker.openPopup) {
        marker.openPopup();
      } else {
        // Fallback: re-add marker with openPopup flag if it doesn't exist
        const markerData = self.markerStates.get(data.id);
        if (markerData) {
          self.addMarker({ ...markerData, id: data.id, openPopup: true });
        }
      }
      self.currentPopupMarkerId = data.id;
    });

    // Handle historical packets during replay
    self.handleEvent("historical_packet", (data: MarkerData) => {
      self.addMarker({
        ...data,
        historical: true,
        popup: data.popup || self.buildPopupContent(data),
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
            const timeA = parseTimestamp(a.timestamp);
            const timeB = parseTimestamp(b.timestamp);
            return timeA - timeB;
          });

          // Add markers in chronological order
          sortedPackets.forEach((packet) => {
            self.addMarker({
              ...packet,
              historical: true,
              popup: packet.popup || self.buildPopupContent(packet),
            });
          });
        });
      }
    });

    // Handle progressive loading of historical packets (batch processing)
    self.handleEvent("add_historical_packets_batch", (data: { packets: MarkerData[], batch: number, is_final: boolean }) => {
      console.log("Received historical packet batch:", data.batch, "packet count:", data.packets?.length || 0);
      try {
        if (data.packets && Array.isArray(data.packets)) {
          // Process all packets immediately for maximum speed
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
            const timeA = parseTimestamp(a.timestamp);
            const timeB = parseTimestamp(b.timestamp);
            return timeA - timeB;
          });

          // Add markers in chronological order
          sortedPackets.forEach((packet) => {
            self.addMarker({
              ...packet,
              historical: true,
              popup: packet.popup || self.buildPopupContent(packet),
            });
          });
        });
      }
      } catch (error) {
        console.error("Error processing historical packets batch:", error);
        // Continue processing other batches even if one fails
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
      console.log("Clearing historical packets");
      // Remove only historical markers (preserve live markers and their trails)
      const markersToRemove: string[] = [];
      self.markers!.forEach((marker: any, id: any) => {
        const markerState = self.markerStates!.get(String(id));
        // Only remove markers that are explicitly historical
        if ((marker as any)._isHistorical || (markerState && markerState.historical)) {
          markersToRemove.push(String(id));
        }
      });

      // Remove historical markers without affecting trails of live markers
      markersToRemove.forEach((id) => {
        const marker = self.markers!.get(id);
        const markerState = self.markerStates!.get(id);

        if (marker) {
          // Remove from map and storage
          self.markerLayer!.removeLayer(marker);
          self.markers!.delete(id);
          self.markerStates!.delete(id);

          // We now NEVER remove trails when clearing historical packets
          // This preserves the historical trail visibility
        }
      });

      // Make sure trail layer is visible and on top
      if (self.trailLayer && self.trailLayer.bringToFront) {
        self.trailLayer.bringToFront();
      }
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

    // Handle clear_all_markers event
    self.handleEvent("clear_all_markers", () => {
      // Modified to preserve historical trails - only removes non-historical markers
      self.clearAllMarkers();

      // Force trail layer to front to ensure visibility
      if (self.trailLayer && self.trailLayer.bringToFront) {
        self.trailLayer.bringToFront();
      }
    });

    // Handle heat map data for low zoom levels
    self.handleEvent("show_heat_map", (data: { heat_points: Array<{lat: number, lng: number, intensity: number}> }) => {
      try {
        console.log("Received heat map data with", data.heat_points?.length || 0, "points");
        
        if (!self.map || self.isDestroyed) {
          console.warn("Map not ready or destroyed, skipping heat map update");
          return;
        }
        
        if (!L.heatLayer) {
          console.error("Leaflet.heat plugin not loaded!");
          return;
        }
        
        if (!self.heatLayer) {
          console.log("Creating heat layer for the first time");
          try {
            self.heatLayer = L.heatLayer([], {
              radius: 25,
              blur: 15,
              maxZoom: 8,
              gradient: {
                0.4: 'blue',
                0.65: 'lime',
                0.85: 'yellow',
                1.0: 'red'
              }
            });
          } catch (error) {
            console.error("Failed to create heat layer:", error);
            return;
          }
        }
        
        // Convert heat points to format expected by Leaflet.heat
        const heatData = data.heat_points.map(point => [
          point.lat,
          point.lng,
          Math.min(point.intensity / 50.0, 1.0) // Normalize intensity to 0-1 range, cap at 1
        ] as [number, number, number]);
        
        console.log("Setting heat layer data:", heatData.length, "points");
        
        // Update heat layer data
        self.heatLayer.setLatLngs(heatData);
        
        // Show heat layer and hide marker layer
        if (!self.map.hasLayer(self.heatLayer)) {
          console.log("Adding heat layer to map");
          self.map.addLayer(self.heatLayer);
        }
        if (self.markerLayer && self.map.hasLayer(self.markerLayer)) {
          console.log("Removing marker layer from map");
          self.map.removeLayer(self.markerLayer);
        }
      } catch (error) {
        console.error("Error in show_heat_map handler:", error);
      }
    });

    // Handle switching back to markers
    self.handleEvent("show_markers", () => {
      try {
        console.log("Switching from heat map to markers");
        
        if (!self.map || self.isDestroyed) {
          console.warn("Map not ready or destroyed, skipping marker display");
          return;
        }
        
        // Hide heat layer and show marker layer
        if (self.heatLayer && self.map.hasLayer(self.heatLayer)) {
          console.log("Removing heat layer");
          self.map.removeLayer(self.heatLayer);
        }
        if (self.markerLayer && !self.map.hasLayer(self.markerLayer)) {
          console.log("Adding marker layer");
          self.map.addLayer(self.markerLayer);
        }
      } catch (error) {
        console.error("Error in show_markers handler:", error);
      }
    });
  },

  sendBoundsToServer() {
    const self = this as unknown as LiveViewHookContext;
    if (!self.map || self.isDestroyed) return;

    try {
      const bounds = self.map!.getBounds();
      const center = self.map!.getCenter();
      const zoom = self.map!.getZoom();

      // We're no longer removing markers outside bounds during normal panning/zooming
      // to preserve historical positions and the current marker
      // self.removeMarkersOutsideBounds(bounds);

      // Use direct pushEvent call with proper context
      if (self.pushEvent && typeof self.pushEvent === 'function') {
        const boundsData = {
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
        };
        console.log("Sending bounds_changed event:", boundsData);
        self.pushEvent("bounds_changed", boundsData);
      }
    } catch (error) {
      console.error("Error sending bounds to server:", error);
    }
  },

  addMarker(data: MarkerData & { openPopup?: boolean }) {
    const self = this as unknown as LiveViewHookContext;
    if (!data.id || !data.lat || !data.lng) {
      console.warn("Invalid marker data:", data);
      return;
    }

    // Ensure maps are initialized
    if (!self.markers || !self.markerStates || !self.markerLayer) {
      console.warn("Map data structures not initialized, skipping marker add");
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
    const existingMarker = self.markers.get(data.id);
    const existingState = self.markerStates.get(data.id);

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
        const timestamp = parseTimestamp(data.timestamp);
        // Use callsign_group for proper trail grouping
        const trailId = getTrailId(data);

        self.trailManager.addPosition(trailId, lat, lng, timestamp, isHistoricalDot);
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

    // Remove existing marker if it exists (without affecting trails since we're just replacing it)
    self.removeMarkerWithoutTrail(data.id);

    // Create marker - use simple dot for older historical positions, APRS icon for most recent
    const marker = L.marker([lat, lng], {
      icon: self.createMarkerIcon(data),
    });

    // Add popup if content provided
    if (data.popup) {
      marker.bindPopup(data.popup, { autoPan: false });
      
      // Handle popup close events - check if hook is still connected
      marker.on("popupclose", () => {
        // Only send event if not destroyed and pushEvent is still the original function
        if (!self.isDestroyed && self.pushEvent && typeof self.pushEvent === 'function') {
          try {
            self.pushEvent("popup_closed", {});
          } catch (e) {
            // Silently ignore errors if context is lost
            console.debug("Unable to send popup_closed event:", e);
          }
        }
      });
    }

    // Handle marker click
    marker.on("click", () => {
      if (marker.openPopup) marker.openPopup();
      safePushEvent(self.pushEvent, "marker_clicked", {
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
    marker.addTo(self.markerLayer);
    self.markers.set(data.id, marker);

    // Make sure historical markers and trails stay visible
    if (data.historical || data.is_most_recent_for_callsign) {
      if (self.trailLayer && self.trailLayer.bringToFront) {
        setTimeout(() => self.trailLayer.bringToFront(), 50);
      }
    }

    // Store marker state for optimization
    self.markerStates.set(data.id, {
      lat: lat,
      lng: lng,
      symbol_table: data.symbol_table_id || "/",
      symbol_code: data.symbol_code || ">",
      popup: data.popup,
      historical: data.historical,
      is_most_recent_for_callsign: data.is_most_recent_for_callsign,
      callsign_group: data.callsign_group || data.callsign,
      callsign: data.callsign,
    });

    // Initialize trail for new marker - always add to trail for line drawing
    if (self.trailManager) {
      const isHistoricalDot = data.historical && !data.is_most_recent_for_callsign;
      const timestamp = parseTimestamp(data.timestamp);
      // Use callsign_group for proper trail grouping
      const trailId = getTrailId(data);

      self.trailManager.addPosition(trailId, lat, lng, timestamp, isHistoricalDot);
    }

    // Open popup if requested
    if (data.openPopup && marker.openPopup) {
      marker.openPopup();
    }

    if (self.oms && marker) {
      self.oms.addMarker(marker);
    }
  },

  removeMarker(id: string | any) {
    const self = this as unknown as LiveViewHookContext;
    // Ensure id is a string
    const markerId = typeof id === "string" ? id : String(id);

    const marker = self.markers!.get(markerId);
    const markerState = self.markerStates!.get(markerId);

    if (marker) {
      self.markerLayer!.removeLayer(marker);
      self.markers!.delete(markerId);
      self.markerStates!.delete(markerId);
    }

    // Remove trail - use callsign_group for proper trail identification
    if (self.trailManager) {
      const trailId = markerState?.callsign_group || markerState?.callsign || markerId;
      self.trailManager.removeTrail(trailId);
    }

    if (self.oms && marker) {
      self.oms.removeMarker(marker);
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
              const timestamp = parseTimestamp(data.timestamp);
              // Use callsign_group for proper trail grouping
              const trailId = getTrailId(data);
              self.trailManager.addPosition(trailId, lat, lng, timestamp, isHistoricalDot);
            }
          }
        }
      }

      // Update popup if provided
      if (data.popup) {
        existingMarker.setPopupContent(data.popup);
        // Force popup to refresh by unbinding and rebinding
        existingMarker.unbindPopup();
        existingMarker.bindPopup(data.popup, { autoPan: false });
      }
    } else {
      // Marker doesn't exist, create it
      self.addMarker(data);
    }
  },

  clearAllMarkers() {
    const self = this as unknown as LiveViewHookContext;

    // Instead of clearing all markers, only remove non-historical and non-current markers
    const markersToPreserve = new Map();
    const statesToPreserve = new Map();

    // Identify historical markers and most recent markers to preserve
    self.markers!.forEach((marker, id) => {
      const markerState = self.markerStates!.get(String(id));
      const isHistorical = (marker as any)._isHistorical || (markerState && markerState.historical);
      const isMostRecent = markerState && markerState.is_most_recent_for_callsign;

      // Keep historical markers and current position markers
      if (isHistorical || isMostRecent) {
        markersToPreserve.set(String(id), marker);
        if (markerState) {
          statesToPreserve.set(String(id), markerState);
        }
      } else {
        // Remove only non-historical, non-current markers
        self.markerLayer!.removeLayer(marker);
      }
    });

    // Replace the markers and states with just the preserved ones
    self.markers!.clear();
    self.markerStates!.clear();

    markersToPreserve.forEach((marker, id) => {
      self.markers!.set(id, marker);
    });

    statesToPreserve.forEach((state, id) => {
      self.markerStates!.set(id, state);
    });

    // Don't clear trails - keep them visible
    // if (self.trailManager) {
    //   self.trailManager.clearAllTrails();
    // }

    // Make sure trail layer is on top
    if (self.trailLayer && self.trailLayer.bringToFront) {
      setTimeout(() => self.trailLayer.bringToFront(), 100);
    }
  },

  removeMarkersOutsideBounds(bounds: L.LatLngBounds) {
    const self = this as unknown as LiveViewHookContext;
    if (!bounds || !self.markers) return;

    const markersToRemove: string[] = [];

    self.markers!.forEach((marker: L.Marker, id: string) => {
      // Check if this is a historical marker or the most recent marker for a callsign
      const markerState = self.markerStates!.get(String(id));
      const isHistorical = (marker as any)._isHistorical || (markerState && markerState.historical);
      const isMostRecent = markerState && markerState.is_most_recent_for_callsign;

      // Always preserve historical markers and the most recent marker for a callsign
      if (isHistorical || isMostRecent) {
        return; // Skip this marker, don't remove it
      }

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

    // Remove out-of-bounds markers without affecting trails
    markersToRemove.forEach((id) => self.removeMarkerWithoutTrail(String(id)));
  },

  removeMarkerWithoutTrail(id: string | any) {
    const self = this as unknown as LiveViewHookContext;
    // Ensure id is a string
    const markerId = typeof id === "string" ? id : String(id);

    const marker = self.markers!.get(markerId);

    if (marker) {
      // Remove marker from map but don't touch trails
      self.markerLayer!.removeLayer(marker);
      self.markers!.delete(markerId);
      self.markerStates!.delete(markerId);
      // Note: We intentionally don't remove trails here
    }
  },

  createMarkerIcon(data: MarkerData): L.DivIcon {
    const self = this as unknown as LiveViewHookContext;
    // If this is the most recent for the callsign, always show the APRS icon
    if (data.is_most_recent_for_callsign) {
      // Remove any historical dot at the same position for this callsign
      if (self.markers && self.markerStates) {
        self.markers.forEach((marker: any, id: string) => {
          const state = self.markerStates!.get(String(id));
          if (
            state &&
            state.historical &&
            state.callsign_group === data.callsign_group &&
            Math.abs(state.lat - data.lat) < 0.00001 &&
            Math.abs(state.lng - data.lng) < 0.00001
          ) {
            self.removeMarkerWithoutTrail(id);
          }
        });
      }
      // Use server-generated symbol HTML if available
      if (data.symbol_html) {
        return L.divIcon({
          html: data.symbol_html,
          className: "",
          iconSize: [120, 32], // Increased width to accommodate callsign label
          iconAnchor: [16, 16],
        });
      }
    }

    // For historical packets that are not the most recent for their callsign,
    // still show the proper APRS symbol but with reduced opacity
    if (data.historical && !data.is_most_recent_for_callsign) {
      // Use server-generated symbol HTML if available
      if (data.symbol_html) {
        // Add opacity to the symbol HTML for historical markers
        const historicalHtml = data.symbol_html.replace(
          /style="([^"]*)"/, 
          'style="$1 opacity: 0.7;"'
        );
        
        return L.divIcon({
          html: historicalHtml,
          className: "historical-aprs-marker",
          iconSize: [120, 32],
          iconAnchor: [16, 16],
        });
      }
      
      // Fallback: red dot for historical positions without symbol data
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

    // Fallback: Use server-generated symbol HTML if available
    if (data.symbol_html) {
      return L.divIcon({
        html: data.symbol_html,
        className: "",
        iconSize: [32, 32],
        iconAnchor: [16, 16],
      });
    }

    // Final fallback: Simple dot
    const iconHtml = `<div style="
      width: 8px;
      height: 8px;
      background-color: #2563eb;
      border: 2px solid #FFFFFF;
      border-radius: 50%;
      opacity: 0.8;
      box-shadow: 0 0 2px rgba(0,0,0,0.3);
    " title="APRS Station: ${data.callsign}"></div>`;

    return L.divIcon({
      html: iconHtml,
      className: "",
      iconSize: [12, 12],
      iconAnchor: [6, 6],
    });
  },

  buildPopupContent(data: MarkerData): string {
    const callsign = data.callsign || data.id || "Unknown";
    const comment = data.comment || "";
    // const symbolTableId = data.symbol_table_id || "/";
    // const symbolCode = data.symbol_code || ">";
    // const symbolDesc = data.symbol_description || `Symbol: ${symbolTableId}${symbolCode}`;

    let content = `<div class="aprs-popup">
      <div class="aprs-callsign"><strong><a href="/${callsign}" class="aprs-lv-link">${callsign}</a></strong> <a href="/info/${callsign}" class="aprs-lv-link aprs-info-link">info</a></div>`;
    // Removed symbol info from popup
    // content += `<div class="aprs-symbol-info">${symbolDesc}</div>`;

    if (comment) {
      content += `<div class="aprs-comment">${comment}</div>`;
    }

    if (data.timestamp) {
      const timestamp = parseTimestamp(data.timestamp);
      const date = new Date(timestamp);
      
      if (!isNaN(date.getTime())) {
        content += `<div class="aprs-timestamp">${date.toISOString()}</div>`;
      }
    }

    content += "</div>";
    return content;
  },

  destroyed() {
    const self = this as unknown as LiveViewHookContext;
    
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
    
    // Clear programmatic move timeout
    if (self.programmaticMoveTimeout !== undefined) {
      clearTimeout(self.programmaticMoveTimeout);
      self.programmaticMoveTimeout = undefined;
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
    
    if (self.boundsTimer !== undefined) {
      clearTimeout(self.boundsTimer);
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
      self.markers.forEach((marker: any) => {
        try {
          marker.off(); // Remove all event listeners
          if (marker.getPopup()) {
            marker.unbindPopup(); // Unbind popup to prevent events
          }
        } catch (e) {
          console.debug(`Error cleaning up marker:`, e);
        }
      });
    }
    
    // Clean up heat layer
    if (self.heatLayer !== undefined && self.map !== undefined) {
      try {
        if (self.map.hasLayer(self.heatLayer)) {
          self.map.removeLayer(self.heatLayer);
        }
        self.heatLayer = undefined;
      } catch (e) {
        console.debug("Error removing heat layer:", e);
      }
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
    
    // Restore original pushEvent (though it won't be used since we're destroyed)
    self.pushEvent = originalPushEvent;
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
