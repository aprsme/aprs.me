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
    // Initialize the map centered on the United States
    const map = L.map(this.el).setView([39.8283, -98.5795], 5);
    console.log("Map initialized:", map);

    // Add OpenStreetMap tile layer
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
      maxZoom: 19,
    }).addTo(map);

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
    // Remove all markers from the map
    this.markers.forEach((marker) => {
      this.map.removeLayer(marker);
    });
    this.markers.clear();
    this.packetCount = 0;
    const counterElement = document.getElementById("packet-count");
    if (counterElement) {
      counterElement.textContent = this.packetCount;
    }
  },

  removeMarkersOutsideBounds(bounds) {
    // Remove markers that are outside the current bounds
    const markersToRemove = [];

    this.markers.forEach((marker, callsign) => {
      const latLng = marker.getLatLng();
      if (!bounds.contains(latLng)) {
        this.map.removeLayer(marker);
        markersToRemove.push(callsign);
      }
    });

    // Remove from our tracking map
    markersToRemove.forEach((callsign) => {
      this.markers.delete(callsign);
      this.packetCount--;
    });

    // Update counter
    const counterElement = document.getElementById("packet-count");
    if (counterElement) {
      counterElement.textContent = this.packetCount;
    }

    // Notify server of the updated packet count
    this.pushEvent("update_packet_count", { count: this.packetCount });
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
      existingMarker.setLatLng([lat, lng]);
      existingMarker.setPopupContent(popupContent);
    } else {
      // Create new marker
      const icon = this.createAPRSIcon(
        packet["data_extended"]["symbol_table_id"] || "/",
        packet["data_extended"]["symbol_code"] || ">",
      );

      const marker = L.marker([lat, lng], { icon: icon }).addTo(this.map).bindPopup(popupContent);

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
