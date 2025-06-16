// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
// import "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "../vendor/some-package.js"
//
// Alternatively, you can `npm install some-package --prefix assets` and import
// them using a path starting with the package name:
//
//     import "some-package"
//

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html";
// Establish Phoenix Socket and LiveView configuration.
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";

let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");

// APRS Map Hook
let Hooks = {};
Hooks.APRSMap = {
  mounted() {
    console.log("APRSMap hook mounted");
    // Get initial center and zoom from server-provided data attributes
    const initialCenter = JSON.parse(this.el.dataset.center);
    const initialZoom = parseInt(this.el.dataset.zoom);

    console.log("Initializing map with center:", initialCenter, "and zoom:", initialZoom);
    console.log("Map container element:", this.el);
    console.log("Container dimensions:", this.el.offsetWidth, "x", this.el.offsetHeight);
    console.log("Parsed coordinates:", initialCenter.lat, initialCenter.lng, "zoom:", initialZoom);

    // Initialize the map with the server-provided location
    const map = L.map(this.el, {
      zoomControl: true,
      attributionControl: true,
      closePopupOnClick: true,
    }).setView([initialCenter.lat, initialCenter.lng], initialZoom);

    console.log("Map setView called with:", [initialCenter.lat, initialCenter.lng], initialZoom);
    console.log("Map object created:", map);

    // Validate container size and force refresh if needed
    if (this.el.offsetWidth === 0 || this.el.offsetHeight === 0) {
      console.warn("Map container has zero dimensions, forcing size refresh");
      setTimeout(() => {
        map.invalidateSize();
        map.setView([initialCenter.lat, initialCenter.lng], initialZoom);
        console.log("Map size refreshed and view reset");
      }, 100);
    }

    // Force initial size calculation
    map.invalidateSize();

    // Track when map is ready
    map.whenReady(() => {
      console.log("Map is fully ready and rendered");
      console.log("Map center after ready:", map.getCenter());
      console.log("Map zoom after ready:", map.getZoom());
      console.log("Map bounds after ready:", map.getBounds());
      this.mapReady = true;
      this.pushEvent("map_ready", {});
    });

    console.log("Map initialized");

    // Add OpenStreetMap tile layer
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
      maxZoom: 19,
    }).addTo(map);

    // Check if MarkerCluster plugin is available
    if (typeof L.markerClusterGroup === "function") {
      // Create marker cluster group
      this.markerClusterGroup = L.markerClusterGroup({
        chunkedLoading: true,
        maxClusterRadius: function (zoom) {
          // Adjust cluster radius based on zoom level
          // Tighter clustering when zoomed out, looser when zoomed in
          if (zoom <= 5) return 120;
          if (zoom <= 8) return 80;
          if (zoom <= 11) return 60;
          if (zoom <= 14) return 40;
          return 20;
        },
        spiderfyOnMaxZoom: true,
        showCoverageOnHover: true,
        zoomToBoundsOnClick: true,
        removeOutsideVisibleBounds: true,
        animate: true,
        animateAddingMarkers: true,
        disableClusteringAtZoom: 16,
        iconCreateFunction: function (cluster) {
          const childCount = cluster.getChildCount();
          let c = " marker-cluster-";
          if (childCount < 10) {
            c += "small";
          } else if (childCount < 100) {
            c += "medium";
          } else {
            c += "large";
          }
          return new L.DivIcon({
            html: "<div><span>" + childCount + "</span></div>",
            className: "marker-cluster" + c,
            iconSize: new L.Point(40, 40),
          });
        },
      });

      // Add cluster group to map
      map.addLayer(this.markerClusterGroup);
    } else {
      console.warn("Leaflet MarkerCluster plugin not loaded, falling back to regular markers");
      this.markerClusterGroup = null;
    }

    // Store markers to avoid duplicates
    this.markers = new Map();
    this.historicalMarkers = new Map();

    // Store map instance and initialize state
    this.map = map;
    this.mapReady = false;

    // Send initial bounds to server
    this.sendBoundsToServer();

    // Listen for new packets from the server
    this.handleEvent("new_packet", (packet) => {
      this.addPacketMarker(packet);
    });

    // Listen for historical packets
    this.handleEvent("historical_packet", (packet) => {
      this.addPacketMarker(packet, true);
    });

    // Listen for zoom to location event
    this.handleEvent("zoom_to_location", (data) => {
      console.log("Received zoom_to_location event:", data);

      if (data.lat && data.lng) {
        // Convert to numbers to ensure proper handling
        const lat = parseFloat(data.lat);
        const lng = parseFloat(data.lng);
        const zoom = parseInt(data.zoom || 12);

        console.log(`Setting map view to [${lat}, ${lng}] with zoom ${zoom}`);

        // Force map invalidation before setting view
        this.map.invalidateSize();

        // Check container dimensions before zoom
        if (this.el.offsetWidth === 0 || this.el.offsetHeight === 0) {
          console.warn(
            "Container has zero dimensions during zoom, width:",
            this.el.offsetWidth,
            "height:",
            this.el.offsetHeight,
          );
        }

        // Small delay to ensure map is ready
        setTimeout(() => {
          // Force another size refresh right before setView
          this.map.invalidateSize();
          console.log("About to call setView with:", [lat, lng], zoom);
          this.map.setView([lat, lng], zoom, {
            animate: true,
            duration: 1,
          });

          // Force final size refresh after setView
          setTimeout(() => {
            this.map.invalidateSize();
          }, 100);
          console.log("Map view updated after delay");
          console.log("New map center:", this.map.getCenter());
          console.log("New map zoom:", this.map.getZoom());
        }, 300);
      } else {
        console.error("Invalid coordinates in zoom_to_location event:", data);
      }
    });

    // Listen for clearing historical packets
    this.handleEvent("clear_historical_packets", () => {
      this.clearHistoricalMarkers();
    });

    // Listen for clear markers event
    this.handleEvent("clear_markers", () => {
      this.clearAllMarkers();
    });

    // Listen for refresh markers event
    this.handleEvent("refresh_markers", () => {
      this.refreshMarkers();
    });

    // Handle geolocation button clicks
    this.handleEvent("request_geolocation", () => {
      if ("geolocation" in navigator) {
        navigator.geolocation.getCurrentPosition(
          (position) => {
            const { latitude, longitude } = position.coords;
            // Send location back to server
            this.pushEvent("set_location", { lat: latitude, lng: longitude });
          },
          (error) => {
            console.warn("Geolocation error:", error.message);
          },
        );
      } else {
        console.warn("Geolocation not available in this browser");
      }
    });

    // Update bounds when map moves or zooms
    map.on("moveend", () => {
      // Debounce the bounds update to avoid too many server calls
      if (this.boundsTimer) clearTimeout(this.boundsTimer);
      this.boundsTimer = setTimeout(() => {
        this.sendBoundsToServer();
      }, 300);
    });

    // Handle map resize when window is resized
    window.addEventListener("resize", () => {
      map.invalidateSize();
    });
  },

  sendBoundsToServer() {
    const bounds = this.map.getBounds();
    this.pushEvent("update_bounds", {
      bounds: {
        north: bounds.getNorth(),
        south: bounds.getSouth(),
        east: bounds.getEast(),
        west: bounds.getWest(),
      },
    });

    // Remove markers that are now outside the visible bounds
    this.removeMarkersOutsideBounds(bounds);
  },

  clearAllMarkers() {
    // Remove all markers from the cluster group or map
    if (this.markerClusterGroup) {
      this.markerClusterGroup.clearLayers();
    } else {
      // Fallback: remove markers directly from map
      this.markers.forEach((marker) => {
        this.map.removeLayer(marker);
      });
      this.historicalMarkers.forEach((marker) => {
        this.map.removeLayer(marker);
      });
    }
    this.markers.clear();
    this.historicalMarkers.clear();
  },

  refreshMarkers() {
    // More efficient refresh that doesn't require clearing all markers
    // Filter markers to keep only those added in the last hour
    const now = new Date();
    const oneHourAgo = new Date(now.getTime() - 60 * 60 * 1000);

    // First, collect markers to remove
    const markersToRemove = [];
    this.markers.forEach((marker, callsign) => {
      if (marker.addedAt && marker.addedAt < oneHourAgo) {
        markersToRemove.push(callsign);
      }
    });

    // Then remove them from the map and collection
    markersToRemove.forEach((callsign) => {
      const marker = this.markers.get(callsign);
      if (marker) {
        if (this.markerClusterGroup) {
          this.markerClusterGroup.removeLayer(marker);
        } else {
          this.map.removeLayer(marker);
        }
        this.markers.delete(callsign);
      }
    });
  },

  clearHistoricalMarkers() {
    // Remove only historical markers
    if (this.markerClusterGroup) {
      this.historicalMarkers.forEach((marker) => {
        this.markerClusterGroup.removeLayer(marker);
      });
    } else {
      // Fallback: remove markers directly from map
      this.historicalMarkers.forEach((marker) => {
        this.map.removeLayer(marker);
      });
    }
    this.historicalMarkers.clear();
  },

  removeMarkersOutsideBounds(bounds) {
    // The cluster handles marker visibility automatically
  },

  addPacketMarker(packet, isHistorical = false) {
    // Skip packets without required data
    if (
      !packet["data_extended"] ||
      !packet["data_extended"]["latitude"] ||
      !packet["data_extended"]["longitude"]
    ) {
      return;
    }

    const lat = parseFloat(packet["data_extended"]["latitude"]);
    const lng = parseFloat(packet["data_extended"]["longitude"]);

    // Validate coordinates are within valid ranges
    if (isNaN(lat) || isNaN(lng) || lat < -90 || lat > 90 || lng < -180 || lng > 180) {
      return;
    }

    // Generate a unique ID for the marker
    const callsign = packet["base_callsign"] + (packet["ssid"] ? "-" + packet["ssid"] : "");
    // For historical packets, add a timestamp or unique ID to distinguish them
    const markerId = isHistorical ? `hist_${callsign}_${Date.now()}` : callsign;

    // Create popup content with historical indicator if needed
    const timestamp =
      isHistorical && packet["timestamp"]
        ? new Date(packet["timestamp"]).toLocaleString()
        : new Date().toLocaleTimeString();

    const popupContent = `
      <div style="min-width: 200px;">
        <h4 style="margin: 0 0 5px 0; font-weight: bold;">${callsign} ${isHistorical ? "(Historical)" : ""}</h4>
        <p style="margin: 2px 0; font-size: 12px;">
          <strong>Position:</strong> ${lat.toFixed(4)}°, ${lng.toFixed(4)}°<br>
          <strong>Type:</strong> ${packet["data_type"]}<br>
          ${packet["data_extended"]["comment"] ? `<strong>Comment:</strong> ${packet["data_extended"]["comment"]}<br>` : ""}
          <strong>Path:</strong> ${packet["path"]}<br>
          <strong>Time:</strong> ${timestamp}
        </p>
      </div>
    `;

    // Determine which marker collection to use
    const markerCollection = isHistorical ? this.historicalMarkers : this.markers;

    // Check if marker already exists (only for non-historical markers)
    if (!isHistorical && this.markers.has(markerId)) {
      try {
        // Update existing marker
        const existingMarker = this.markers.get(markerId);
        if (this.markerClusterGroup) {
          // Remove and re-add to cluster group
          this.markerClusterGroup.removeLayer(existingMarker);
          existingMarker.setLatLng([lat, lng]);
          existingMarker.setPopupContent(popupContent);
          this.markerClusterGroup.addLayer(existingMarker);
        } else {
          // Fallback: just update position
          existingMarker.setLatLng([lat, lng]);
          existingMarker.setPopupContent(popupContent);
        }
      } catch (e) {
        // If there's an error updating, proceed with creating a new marker
        this.markers.delete(markerId);
      }
    }

    // Create new marker if it doesn't exist or if we're handling a historical packet
    if (isHistorical || !this.markers.has(markerId)) {
      // Create new marker
      const icon = this.createAPRSIcon(
        packet["data_extended"]["symbol_table_id"] || "/",
        packet["data_extended"]["symbol_code"] || ">",
        isHistorical,
      );

      const marker = L.marker([lat, lng], { icon: icon }).bindPopup(popupContent);
      marker.addedAt = new Date(); // Track when the marker was added

      // Add CSS class for historical markers
      if (isHistorical) {
        marker.options.className = "historical-marker";
      }

      // Add to cluster group or directly to map
      try {
        if (this.markerClusterGroup) {
          this.markerClusterGroup.addLayer(marker);
        } else {
          marker.addTo(this.map);
        }
      } catch (e) {
        // Silently ignore errors when adding markers
      }

      // Store the marker
      markerCollection.set(markerId, marker);
    }
  },

  createAPRSIcon(symbolTable, symbolCode, isHistorical = false) {
    // Default icon color based on symbol table
    let color = symbolTable === "/" ? "#2563eb" : "#dc2626";

    // Use a different color for historical markers
    if (isHistorical) {
      color = symbolTable === "/" ? "#90b4ed" : "#e98a84";
    }

    return L.divIcon({
      html: `<div style="background-color: ${color}; width: 12px; height: 12px; border-radius: 50%; border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.4);"></div>`,
      className: isHistorical ? "aprs-marker historical-marker" : "aprs-marker",
      iconSize: [16, 16],
      iconAnchor: [8, 8],
      popupAnchor: [0, -8],
    });
  },

  destroyed() {
    if (this.boundsTimer) {
      clearTimeout(this.boundsTimer);
    }
    if (this.markerClusterGroup) {
      this.markerClusterGroup.clearLayers();
    }
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
    this.markers.clear();
    this.historicalMarkers.clear();
  },
};

let liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
});

// Show progress bar on live navigation and form submits
topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (info) => topbar.delayedShow(200));
window.addEventListener("phx:page-loading-stop", (info) => topbar.hide());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;
