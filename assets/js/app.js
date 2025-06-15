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

    // Initialize the map with the server-provided location
    const map = L.map(this.el).setView([initialCenter.lat, initialCenter.lng], initialZoom);
    console.log("Map initialized:", map);

    // Handle geolocation requests from server
    this.handleEvent("request_geolocation", () => {
      if ("geolocation" in navigator) {
        navigator.geolocation.getCurrentPosition(
          (position) => {
            const { latitude, longitude } = position.coords;
            console.log("User location:", latitude, longitude);
            map.setView([latitude, longitude], 12);
            // Notify server of new location
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
    this.packetCount = 0;

    // Store map instance
    this.map = map;

    // Send initial bounds to server
    this.sendBoundsToServer();

    // Listen for new packets from the server
    this.handleEvent("new_packet", (packet) => {
      console.log("Received new packet:", packet);
      this.addPacketMarker(packet);
    });

    // Listen for clear markers event
    this.handleEvent("clear_markers", () => {
      this.clearAllMarkers();
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
      this.sendBoundsToServer();
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
    }
    this.markers.clear();
    this.packetCount = 0;
    const counterElement = document.getElementById("packet-count");
    if (counterElement) {
      counterElement.textContent = this.packetCount;
    }
  },

  removeMarkersOutsideBounds(bounds) {
    // With clustering, the cluster group handles visibility automatically
    // We'll just track the count of markers within bounds
    let visibleCount = 0;

    this.markers.forEach((marker, callsign) => {
      const latLng = marker.getLatLng();
      if (bounds.contains(latLng)) {
        visibleCount++;
      }
    });

    // Update counter to show visible markers
    const counterElement = document.getElementById("packet-count");
    if (counterElement) {
      counterElement.textContent = visibleCount;
    }

    // Notify server of the updated packet count
    this.pushEvent("update_packet_count", { count: visibleCount });
  },

  addPacketMarker(packet) {
    console.log("addPacketMarker called with:", packet);
    if (
      !packet["data_extended"] ||
      !packet["data_extended"]["latitude"] ||
      !packet["data_extended"]["longitude"]
    ) {
      console.warn("Packet missing required location data:", packet);
      return;
    }

    const lat = parseFloat(packet["data_extended"]["latitude"]);
    const lng = parseFloat(packet["data_extended"]["longitude"]);

    // Validate coordinates are within valid ranges
    if (isNaN(lat) || isNaN(lng) || lat < -90 || lat > 90 || lng < -180 || lng > 180) {
      console.error("Invalid coordinates:", { lat, lng, packet });
      return;
    }
    console.log("Valid coordinates:", { lat, lng, callsign: packet["base_callsign"] });
    const callsign = packet["base_callsign"] + (packet["ssid"] ? "-" + packet["ssid"] : "");

    // Create popup content
    const popupContent = `
      <div style="min-width: 200px;">
        <h4 style="margin: 0 0 5px 0; font-weight: bold;">${callsign}</h4>
        <p style="margin: 2px 0; font-size: 12px;">
          <strong>Position:</strong> ${lat.toFixed(4)}°, ${lng.toFixed(4)}°<br>
          <strong>Type:</strong> ${packet["data_type"]}<br>
          ${packet["data_extended"]["comment"] ? `<strong>Comment:</strong> ${packet["data_extended"]["comment"]}<br>` : ""}
          <strong>Path:</strong> ${packet["path"]}<br>
          <strong>Time:</strong> ${new Date().toLocaleTimeString()}
        </p>
      </div>
    `;

    // Check if marker already exists
    if (this.markers.has(callsign)) {
      // Update existing marker
      const existingMarker = this.markers.get(callsign);
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
    } else {
      // Create new marker
      const icon = this.createAPRSIcon(
        packet["data_extended"]["symbol_table_id"] || "/",
        packet["data_extended"]["symbol_code"] || ">",
      );

      const marker = L.marker([lat, lng], { icon: icon }).bindPopup(popupContent);

      // Add to cluster group or directly to map
      if (this.markerClusterGroup) {
        this.markerClusterGroup.addLayer(marker);
      } else {
        marker.addTo(this.map);
      }

      this.markers.set(callsign, marker);
      console.log("New marker added for:", callsign, "Total markers:", this.markers.size);

      // Update packet counter
      this.packetCount++;
      const counterElement = document.getElementById("packet-count");
      if (counterElement) {
        counterElement.textContent = this.packetCount;
      }
    }
  },

  createAPRSIcon(symbolTable, symbolCode) {
    // Default icon color based on symbol table
    const color = symbolTable === "/" ? "#2563eb" : "#dc2626";

    return L.divIcon({
      html: `<div style="background-color: ${color}; width: 12px; height: 12px; border-radius: 50%; border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.4);"></div>`,
      className: "aprs-marker",
      iconSize: [16, 16],
      iconAnchor: [8, 8],
      popupAnchor: [0, -8],
    });
  },

  destroyed() {
    console.log("APRSMap hook destroyed");
    if (this.markerClusterGroup) {
      this.markerClusterGroup.clearLayers();
    }
    if (this.map) {
      console.log("Removing map instance");
      this.map.remove();
      this.map = null;
    }
    this.markers.clear();
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
