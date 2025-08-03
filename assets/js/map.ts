// Import type definitions
import type {
  LiveViewHookContext,
  MarkerData,
  BoundsData,
  CenterData,
  MarkerState,
  MapEventData,
} from "./types/map";
import type {
  Map as LeafletMap,
  Marker,
  TileLayer,
  LayerGroup,
  DivIcon,
  LatLngBounds,
  Polyline,
} from "leaflet";
import type { LeafletTouchEvent, LeafletPopupEvent } from "./types/leaflet-events";
import type {
  HeatLayer,
  MarkerClusterGroup,
  OverlappingMarkerSpiderfier,
  MarkerClusterGroupOptions,
  HeatLayerOptions,
  MarkerCluster,
  HeatLatLng,
} from "./types/leaflet-plugins";
import type { APRSMarker } from "./types/marker-extensions";
import type { BaseEventPayload } from "./types/events";

// Leaflet and plugins are loaded globally from vendor bundle
// We'll access window.L dynamically when needed instead of storing it at module load time

// CSS files are bundled in vendor.js

// The OverlappingMarkerSpiderfier is attached to window by the UMD module
declare global {
  interface Window {
    OverlappingMarkerSpiderfier: any;
  }
}

// Add plugin types to Leaflet
declare module "leaflet" {
  export function heatLayer(latlngs: HeatLatLng[], options?: HeatLayerOptions): HeatLayer;
  export function markerClusterGroup(options?: MarkerClusterGroupOptions): MarkerClusterGroup;
}

// Import trail management functionality
import { TrailManager } from "./features/trail_manager";
// Import helper functions
import {
  parseTimestamp,
  getTrailId,
  saveMapState,
  safePushEvent,
  getLiveSocket,
} from "./map_helpers";

// APRS Map Hook - handles only basic map interaction
// All data logic handled by LiveView

let MapAPRSMap = {
  mounted() {
    const self = this as unknown as LiveViewHookContext;
    console.log("APRSMap hook mounted() called on element:", this.el);
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
    if (typeof window.L === "undefined") {
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
    
    // Create a local reference to Leaflet for this function
    const L = window.L;

    // Initialize with URL parameters first (from data attributes)
    let initialCenter: CenterData, initialZoom: number;
    let useUrlParams = false;

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

      // Check if URL has explicit parameters (not default values)
      const urlParams = new URLSearchParams(window.location.search);
      useUrlParams = urlParams.has("lat") || urlParams.has("lng") || urlParams.has("z");
    } catch (error) {
      console.error("Error parsing map data attributes:", error);
      initialCenter = { lat: 39.8283, lng: -98.5795 };
      initialZoom = 5;
    }

    // Only use localStorage if no URL params are present
    // Temporarily disabled - using server-side geolocation instead
    /*
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
            initialCenter = { lat, lng };
            initialZoom = zoom;
          }
        }
      } catch (e) {
      }
    }
    */

    self.initializeMap(initialCenter, initialZoom);
  },

  initializeMap(initialCenter: CenterData, initialZoom: number) {
    const self = this as unknown as LiveViewHookContext;
    
    // Check if Leaflet is still available
    if (typeof window.L === "undefined") {
      console.error("Leaflet library not loaded!");
      self.handleFatalError("Leaflet library not available");
      return;
    }
    
    // Create a local reference to Leaflet for this function
    const L = window.L;
    
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

      // Detect if mobile device
      const isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(
        navigator.userAgent,
      );

      self.map = L.map(self.el, {
        zoomControl: !isMobile, // Hide default zoom control on mobile, we'll add a better one
        attributionControl: true,
        closePopupOnClick: true,
        tap: true, // Enable tap events for mobile
        tapTolerance: 30, // Increase tap tolerance for mobile
        touchZoom: true,
        bounceAtZoomLimits: false, // Disable bouncing at zoom limits
        worldCopyJump: true, // Better panning behavior
        preferCanvas: isMobile, // Use canvas renderer on mobile for better performance
        zoomAnimation: !isMobile, // Disable zoom animations on mobile for performance
        fadeAnimation: !isMobile, // Disable fade animations on mobile
        markerZoomAnimation: !isMobile, // Disable marker animations on mobile
      }).setView([initialCenter.lat, initialCenter.lng], initialZoom);

      // Add mobile-friendly zoom control if on mobile
      if (isMobile) {
        L.control
          .zoom({
            position: "bottomright",
          })
          .addTo(self.map);

        // Add touch gesture handling
        self.setupMobileGestures();
      }
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

    // Add OpenStreetMap tile layer with improved settings
    try {
      // Tile provider options
      const tileProviders = {
        osm: {
          url: "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          attribution:
            '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
          subdomains: ["a", "b", "c"],
        },
        osmDE: {
          url: "https://{s}.tile.openstreetmap.de/{z}/{x}/{y}.png",
          attribution:
            '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
          subdomains: ["a", "b", "c"],
        },
        carto: {
          url: "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
          attribution:
            '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a> | APRS.me',
          subdomains: ["a", "b", "c", "d"],
        },
      };

      // Create raster tile layer
      const provider = tileProviders.osm;
      const tileLayer = L.tileLayer(provider.url, {
        attribution: provider.attribution,
        maxZoom: 19,
        subdomains: provider.subdomains,
        tileSize: 256,
        keepBuffer: 2,
        updateWhenZooming: false,
        updateInterval: 200,
        errorTileUrl:
          "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=",
      });

      // Add event handler for tile errors with retry logic
      let retryCount = new Map<string, number>();
      tileLayer.on("tileerror", function (error: any) {
        const src = error.tile.src;
        const count = retryCount.get(src) || 0;

        if (count < 3) {
          console.warn(`Tile load error (attempt ${count + 1}/3):`, src);
          retryCount.set(src, count + 1);

          // Exponential backoff
          setTimeout(
            () => {
              error.tile.src = src + (src.includes("?") ? "&" : "?") + "_retry=" + Date.now();
            },
            Math.pow(2, count) * 500,
          );
        } else {
          console.error("Tile failed after 3 attempts:", src);
          retryCount.delete(src);
        }
      });

      tileLayer.addTo(self.map);
    } catch (error) {
      self.errors!.push("Tile layer failed: " + (error instanceof Error ? error.message : error));
    }

    // Store markers for management
    self.markers = new Map<string, APRSMarker>();

    // Create marker cluster group for medium zoom levels (9-14)
    if (L.markerClusterGroup) {
      self.markerClusterGroup = L.markerClusterGroup({
        showCoverageOnHover: false,
        zoomToBoundsOnClick: true,
        spiderfyOnMaxZoom: true,
        removeOutsideVisibleBounds: true,
        disableClusteringAtZoom: 11, // Show individual markers at zoom 11+
        maxClusterRadius: 80,
        iconCreateFunction: function (cluster: MarkerCluster) {
          const count = cluster.getChildCount();
          let size = "small";
          let className = "marker-cluster-small";

          if (count > 10) {
            size = "medium";
            className = "marker-cluster-medium";
          }
          if (count > 50) {
            size = "large";
            className = "marker-cluster-large";
          }

          return L.divIcon({
            html: "<div><span>" + count + "</span></div>",
            className: "marker-cluster " + className,
            iconSize: [40, 40],
          });
        },
      });
      self.markerClusterGroup.addTo(self.map);
      self.markerLayer = self.markerClusterGroup;
    } else {
      // Fallback to regular layer group if clustering not available
      self.markerLayer = L.layerGroup().addTo(self.map);
    }

    // Create heat layer (hidden by default)
    try {
      if (L.heatLayer) {
        self.heatLayer = L.heatLayer([], {
          radius: 25,
          blur: 15,
          maxZoom: 8,
          gradient: {
            0.4: "blue",
            0.65: "lime",
            0.85: "yellow",
            1.0: "red",
          },
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

    // Store RF path lines for hover visualization
    self.rfPathLines = [];

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
        if (self.pushEvent && typeof self.pushEvent === "function") {
          self.pushEvent("map_ready", {});
          // Send initial bounds to trigger historical loading
          if (self.map && self.pushEvent && typeof self.pushEvent === "function") {
            saveMapState(self.map, self.pushEvent.bind(self));
            // Also send bounds to server to trigger historical loading
            self.sendBoundsToServer();
          }

          // Also send update_map_state to ensure URL updates and bounds processing
          // Increase delay to ensure LiveView is fully connected and ready
          setTimeout(() => {
            if (self.map && self.pushEvent && !self.isDestroyed) {
              saveMapState(self.map, self.pushEvent.bind(self));
            }
          }, 500);
        } else {
          console.warn("pushEvent not available in whenReady callback");
          // Retry after a short delay
          setTimeout(() => {
            if (self.pushEvent && typeof self.pushEvent === "function" && !self.isDestroyed) {
              self.pushEvent("map_ready", {});
              if (self.map) {
                saveMapState(self.map, self.pushEvent.bind(self));
                // Also send bounds to server to trigger historical loading
                console.log("Calling sendBoundsToServer after map_ready (retry)");
                self.sendBoundsToServer();
              }
              // Also trigger map state update after a delay
              setTimeout(() => {
                if (self.map && self.pushEvent && !self.isDestroyed) {
                  saveMapState(self.map, self.pushEvent.bind(self));
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
        if (
          self.map &&
          !self.isDestroyed &&
          self.pushEvent &&
          typeof self.pushEvent === "function"
        ) {
          saveMapState(self.map, self.pushEvent.bind(self));
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

        // Handle OMS markers when crossing zoom threshold
        if (self.oms) {
          // Clear and re-add all markers to OMS on zoom change
          self.oms.clearMarkers();
          self.markers.forEach((marker, id) => {
            const markerState = self.markerStates.get(String(id));
            // Only add most recent markers (those with icons) to OMS for spidering
            // Fallback: if is_most_recent_for_callsign is undefined, exclude historical markers as before
            const shouldAddToOms = markerState?.is_most_recent_for_callsign === true || 
                                   (markerState?.is_most_recent_for_callsign == null && !marker._isHistorical);
            if (marker && !marker._isClusterMarker && markerState && shouldAddToOms) {
              self.oms.addMarker(marker);
            }
          });
        }

        // If zoom changed significantly (more than 2 levels), request reload of normal markers
        // but preserve historical ones and current position
        if (zoomDifference > 2) {
          self.pushEvent("refresh_markers", {});
        }

        self.lastZoom = currentZoom;
        // Save map state and update URL
        if (
          self.map &&
          !self.isDestroyed &&
          self.pushEvent &&
          typeof self.pushEvent === "function"
        ) {
          saveMapState(self.map, self.pushEvent.bind(self));
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

    // Only initialize OMS if clustering is not available or at high zoom levels
    if (typeof window.OverlappingMarkerSpiderfier !== "undefined") {
      self.oms = new window.OverlappingMarkerSpiderfier(self.map, {
        keepSpiderfied: true,
        nearbyDistance: 20,
        circleSpiralSwitchover: 15,
        legWeight: 2,
        legColors: {
          usual: '#222',
          highlighted: '#f00'
        },
        spiderfyDistanceMultiplier: 3.5
      });

      // Add click handler for spiderfied markers
      self.oms.addListener("click", function (marker: Marker) {
        if (marker.openPopup) marker.openPopup();
      });

      // Style the spider legs
      self.oms.addListener("spiderfy", function (markers: Marker[]) {
        self.map.closePopup();
      });

      self.oms.addListener("unspiderfy", function (markers: Marker[]) {
        // Markers return to normal positions
      });
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

  setupMobileGestures() {
    const self = this as unknown as LiveViewHookContext;

    // Long press timer
    let longPressTimer: any = null;
    let startX = 0;
    let startY = 0;
    const longPressDuration = 750; // 750ms for long press
    const movementThreshold = 10; // pixels

    const handleLongPress = (e: TouchEvent) => {
      if (e.touches.length !== 1) return; // Only handle single touch

      const touch = e.touches[0];
      const latlng = self.map!.containerPointToLatLng(L.point(touch.clientX, touch.clientY));

      // Find the nearest marker
      let nearestMarker: APRSMarker | null = null;
      let minDistance = Infinity;

      self.markers.forEach((marker) => {
        const markerLatLng = marker.getLatLng();
        const distance = latlng.distanceTo(markerLatLng);
        if (distance < minDistance && distance < 50) {
          // Within 50 meters
          minDistance = distance;
          nearestMarker = marker;
        }
      });

      if (nearestMarker) {
        nearestMarker.openPopup();
      }
    };

    self.map!.on("touchstart", (e: LeafletTouchEvent) => {
      if (e.originalEvent.touches.length !== 1) return;

      const touch = e.originalEvent.touches[0];
      startX = touch.clientX;
      startY = touch.clientY;

      longPressTimer = setTimeout(() => {
        handleLongPress(e.originalEvent);
      }, longPressDuration);
    });

    self.map!.on("touchmove", (e: LeafletTouchEvent) => {
      if (!longPressTimer) return;

      const touch = e.originalEvent.touches[0];
      const deltaX = Math.abs(touch.clientX - startX);
      const deltaY = Math.abs(touch.clientY - startY);

      // Cancel long press if moved too much
      if (deltaX > movementThreshold || deltaY > movementThreshold) {
        clearTimeout(longPressTimer);
        longPressTimer = null;
      }
    });

    self.map!.on("touchend", () => {
      if (longPressTimer) {
        clearTimeout(longPressTimer);
        longPressTimer = null;
      }
    });

    self.map!.on("touchcancel", () => {
      if (longPressTimer) {
        clearTimeout(longPressTimer);
        longPressTimer = null;
      }
    });
  },

  setupPopupNavigation() {
    const self = this as unknown as LiveViewHookContext;

    // Store the event handler so we can remove it later
    self.popupNavigationHandler = (e: Event) => {
      const target = e.target as HTMLElement;

      // Check if clicked element or its parent is a LiveView navigation link
      const navLink = target.closest(".aprs-lv-link") as HTMLAnchorElement;

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
    document.addEventListener("click", self.popupNavigationHandler);
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
              const dimensionCheckTimeout = setTimeout(() => {
                // Check if map still exists and not destroyed
                if (!self.map || self.isDestroyed) {
                  return;
                }

                const afterRect = self.el.getBoundingClientRect();

                if (afterRect.width === 0 || afterRect.height === 0) {
                  console.error("Map element lost dimensions after zoom!");
                  // Try to restore dimensions
                  self.el.style.width = "100vw";
                  self.el.style.height = "100vh";
                  if (self.map) {
                    self.map.invalidateSize();
                  }
                }
              }, 1000);

              // Store timeout for cleanup
              if (!self.cleanupTimeouts) {
                self.cleanupTimeouts = [];
              }
              self.cleanupTimeouts.push(dimensionCheckTimeout);
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

    // Handle trail duration updates from LiveView
    self.handleEvent("update_trail_duration", (data: { duration_hours: number }) => {
      if (self.trailManager) {
        self.trailManager.setTrailDuration(data.duration_hours);
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
              (existingMarker as APRSMarker)._isHistorical = true;
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
    self.handleEvent(
      "add_historical_packets_batch",
      (data: { packets: MarkerData[]; batch: number; is_final: boolean }) => {
        console.log("Received add_historical_packets_batch event:", {
          packetCount: data.packets?.length || 0,
          batch: data.batch,
          is_final: data.is_final
        });
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
      },
    );

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
      // Remove only historical markers (preserve live markers and their trails)
      const markersToRemove: string[] = [];
      self.markers!.forEach((marker: APRSMarker, id: string) => {
        const markerState = self.markerStates!.get(String(id));
        // Only remove markers that are explicitly historical
        if ((marker as APRSMarker)._isHistorical || (markerState && markerState.historical)) {
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

    // Handle drawing RF path lines when hovering over a station
    self.handleEvent(
      "draw_rf_path",
      (data: {
        station_lat: number;
        station_lng: number;
        path_stations: Array<{ callsign: string; lat: number; lng: number }>;
      }) => {
        if (!self.map || !data.path_stations || data.path_stations.length === 0) return;

        // Clear any existing path lines
        if (self.rfPathLines) {
          self.rfPathLines.forEach((line) => self.map!.removeLayer(line));
        }
        self.rfPathLines = [];

        // Draw lines from station to each digipeater/igate in sequence
        let prevLat = data.station_lat;
        let prevLng = data.station_lng;

        data.path_stations.forEach((station, index) => {
          // Draw line from previous position to this station
          const line = L.polyline(
            [
              [prevLat, prevLng],
              [station.lat, station.lng],
            ],
            {
              color: "#FF6B6B",
              weight: 3,
              opacity: 0.8,
              dashArray: index === 0 ? null : "5, 10", // Solid line for first hop, dashed for subsequent
            },
          );

          line.addTo(self.map!);
          self.rfPathLines.push(line);

          // Add a small circle marker at the digipeater/igate location
          const circle = L.circleMarker([station.lat, station.lng], {
            radius: 6,
            fillColor: "#2563eb",
            color: "#fff",
            weight: 2,
            opacity: 1,
            fillOpacity: 0.8,
          });

          circle.bindTooltip(station.callsign, {
            permanent: false,
            direction: "top",
            offset: [0, -10],
          });

          circle.addTo(self.map!);
          self.rfPathLines.push(circle);

          // Update prev position for next line
          prevLat = station.lat;
          prevLng = station.lng;
        });
      },
    );

    // Handle clearing RF path lines
    self.handleEvent("clear_rf_path", () => {
      if (self.rfPathLines) {
        self.rfPathLines.forEach((line) => {
          if (self.map && self.map.hasLayer(line)) {
            self.map.removeLayer(line);
          }
        });
        self.rfPathLines = [];
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
    self.handleEvent("show_heat_map", (data: { heat_points: HeatLatLng[] }) => {
      try {
        if (!self.map || self.isDestroyed) {
          console.warn("Map not ready or destroyed, skipping heat map update");
          return;
        }

        if (!L.heatLayer) {
          console.error("Leaflet.heat plugin not loaded!");
          return;
        }

        if (!self.heatLayer) {
          try {
            self.heatLayer = L.heatLayer([], {
              radius: 25,
              blur: 15,
              maxZoom: 8,
              gradient: {
                0.4: "blue",
                0.65: "lime",
                0.85: "yellow",
                1.0: "red",
              },
            });
          } catch (error) {
            console.error("Failed to create heat layer:", error);
            return;
          }
        }

        // Convert heat points to format expected by Leaflet.heat
        const heatData = data.heat_points.map(
          (point) =>
            [
              point.lat,
              point.lng,
              Math.min(point.intensity / 50.0, 1.0), // Normalize intensity to 0-1 range, cap at 1
            ] as [number, number, number],
        );

        // Update heat layer data
        self.heatLayer.setLatLngs(heatData);

        // Show heat layer and hide marker layer
        if (!self.map.hasLayer(self.heatLayer)) {
          self.map.addLayer(self.heatLayer);
        }
        if (self.markerLayer && self.map.hasLayer(self.markerLayer)) {
          self.map.removeLayer(self.markerLayer);
        }
      } catch (error) {
        console.error("Error in show_heat_map handler:", error);
      }
    });

    // Handle switching back to markers
    self.handleEvent("show_markers", () => {
      try {
        if (!self.map || self.isDestroyed) {
          console.warn("Map not ready or destroyed, skipping marker display");
          return;
        }

        // Hide heat layer and show marker layer
        if (self.heatLayer && self.map.hasLayer(self.heatLayer)) {
          self.map.removeLayer(self.heatLayer);
        }
        if (self.markerLayer && !self.map.hasLayer(self.markerLayer)) {
          self.map.addLayer(self.markerLayer);
        }
      } catch (error) {
        console.error("Error in show_markers handler:", error);
      }
    });
  },

  sendBoundsToServer() {
    const self = this as unknown as LiveViewHookContext;
    console.log("sendBoundsToServer called, map:", !!self.map, "isDestroyed:", self.isDestroyed);
    if (!self.map || self.isDestroyed) return;

    try {
      const bounds = self.map!.getBounds();
      const center = self.map!.getCenter();
      const zoom = self.map!.getZoom();

      // We're no longer removing markers outside bounds during normal panning/zooming
      // to preserve historical positions and the current marker
      // self.removeMarkersOutsideBounds(bounds);

      // Use direct pushEvent call with proper context
      if (self.pushEvent && typeof self.pushEvent === "function") {
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
    const L = window.L;
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
        if (!self.isDestroyed && self.pushEvent && typeof self.pushEvent === "function") {
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
    marker.on("click", (e: any) => {
      // Don't open popup if the marker is managed by OMS and there are nearby markers
      // The OMS will handle the click and open the popup after spiderfying
      if (self.oms && marker._oms && marker._omsData) {
        // Marker is currently spiderfied, allow normal popup
        if (marker.openPopup) marker.openPopup();
      } else if (!self.oms || !marker._oms) {
        // Normal marker not managed by OMS
        if (marker.openPopup) marker.openPopup();
      }
      // If marker._oms is true but no _omsData, let OMS handle it (will spiderfy)

      // Bring the clicked marker to front
      if (marker.getElement) {
        const element = marker.getElement();
        if (element) {
          // Find the highest z-index among all markers
          let maxZIndex = 1000;
          document.querySelectorAll(".leaflet-marker-icon").forEach((el) => {
            const zIndex = parseInt((el as HTMLElement).style.zIndex || "0", 10);
            if (zIndex > maxZIndex) maxZIndex = zIndex;
          });

          // Set this marker's z-index higher than all others
          element.style.zIndex = (maxZIndex + 1).toString();
        }
      }

      // Use bound pushEvent function to preserve context
      if (self.pushEvent && typeof self.pushEvent === "function" && !self.isDestroyed) {
        safePushEvent(self.pushEvent.bind(self), "marker_clicked", {
          id: data.id,
          callsign: data.callsign,
          lat: lat,
          lng: lng,
        });
      }
    });

    // Handle marker hover for RF path visualization
    // Check for non-empty RF path (not TCPIP and not empty string)
    if (data.path && data.path.trim() !== "" && !data.path.includes("TCPIP")) {
      marker.on("mouseover", () => {
        // Check if LiveView is still connected before sending event
        if (self.pushEvent && typeof self.pushEvent === "function" && !self.isDestroyed) {
          try {
            self.pushEvent.call(self, "marker_hover_start", {
              id: data.id,
              callsign: data.callsign,
              path: data.path,
              lat: lat,
              lng: lng,
            });
          } catch (error) {
            console.warn("Failed to send marker_hover_start event:", error);
          }
        } else {
          console.warn("LiveView not connected, cannot send hover event");
        }
      });

      marker.on("mouseout", () => {
        // Check if LiveView is still connected before sending event
        if (self.pushEvent && typeof self.pushEvent === "function" && !self.isDestroyed) {
          try {
            self.pushEvent.call(self, "marker_hover_end", {
              id: data.id,
            });
          } catch (error) {
            console.warn("Failed to send marker_hover_end event:", error);
          }
        }
      });
    }

    // Mark historical markers for identification
    if (data.historical) {
      (marker as APRSMarker)._isHistorical = true;
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

    // Add to OMS for overlapping marker handling (only most recent packets with icons)
    // Fallback: if is_most_recent_for_callsign is undefined, exclude historical markers as before
    const shouldAddToOms = data.is_most_recent_for_callsign === true || 
                           (data.is_most_recent_for_callsign == null && !(marker as APRSMarker)._isHistorical);
    if (self.oms && marker && self.map && !marker._isClusterMarker && shouldAddToOms) {
      self.oms.addMarker(marker);
    }
  },

  removeMarker(id: string) {
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
      try {
        self.oms.removeMarker(marker);
      } catch (e) {
        console.debug("Error removing marker from OMS:", e);
      }
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
      const isHistorical =
        (marker as APRSMarker)._isHistorical || (markerState && markerState.historical);
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
      const isHistorical =
        (marker as APRSMarker)._isHistorical || (markerState && markerState.historical);
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

  removeMarkerWithoutTrail(id: string) {
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
    const L = window.L;
    // If this is the most recent for the callsign, always show the APRS icon
    if (data.is_most_recent_for_callsign) {
      // Remove any historical dot at the same position for this callsign
      if (self.markers && self.markerStates) {
        self.markers.forEach((marker: APRSMarker, id: string) => {
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
    // show a simple dot instead of the full APRS symbol
    if (data.historical && !data.is_most_recent_for_callsign) {
      // Always show a red dot for historical positions
      const iconHtml = `<div style="
        width: 8px;
        height: 8px;
        background-color: #FF6B6B;
        border: 2px solid #FFFFFF;
        border-radius: 50%;
        opacity: 0.8;
        box-shadow: 0 0 2px rgba(0,0,0,0.3);
      " title="Historical position for ${data.callsign}"></div>`;

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

    // Clear any dimension check timeouts
    if (self.cleanupTimeouts !== undefined) {
      self.cleanupTimeouts.forEach((timeout: number) => {
        clearTimeout(timeout);
      });
      self.cleanupTimeouts = [];
    }

    // Remove popup navigation event listener
    if (self.popupNavigationHandler) {
      document.removeEventListener("click", self.popupNavigationHandler);
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
      self.markers.forEach((marker: APRSMarker) => {
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

    // Clean up OMS (OverlappingMarkerSpiderfier)
    if (self.oms !== undefined) {
      try {
        self.oms.clearMarkers();
        self.oms.clearListeners("click");
        self.oms.clearListeners("spiderfy");
        self.oms.clearListeners("unspiderfy");
        self.oms = undefined;
      } catch (e) {
        console.debug("Error cleaning up OMS:", e);
      }
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
      // If it's a cluster group, remove it from the map
      if (self.markerClusterGroup !== undefined && self.map !== undefined) {
        try {
          self.map.removeLayer(self.markerClusterGroup);
          self.markerClusterGroup = undefined;
        } catch (e) {
          console.debug("Error removing cluster group:", e);
        }
      }
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
