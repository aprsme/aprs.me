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

// Import minimal APRS map hook
import MinimalAPRSMap from "./minimal_map.js";

// Debug Map Hook
let DebugMap = {
  mounted() {
    console.log("DebugMap hook mounted");
    window.liveSocket.pushEvent("debug_info", { hook_mounted: true });

    // Check if Leaflet is available
    if (typeof L === "undefined") {
      console.error("Leaflet library not loaded!");
      window.liveSocket.pushEvent("debug_info", {
        leaflet_loaded: false,
        errors: ["Leaflet not loaded"],
      });
      return;
    }

    window.liveSocket.pushEvent("debug_info", { leaflet_loaded: true });

    // Get initial center and zoom from server-provided data attributes
    let initialCenter, initialZoom;
    try {
      initialCenter = JSON.parse(this.el.dataset.center);
      initialZoom = parseInt(this.el.dataset.zoom);
      console.log("Parsed data - center:", initialCenter, "zoom:", initialZoom);
    } catch (error) {
      console.error("Error parsing map data attributes:", error);
      window.liveSocket.pushEvent("debug_info", {
        errors: ["Error parsing map data: " + error.message],
      });
      // Fallback values
      initialCenter = { lat: 39.8283, lng: -98.5795 };
      initialZoom = 5;
    }

    // Check element dimensions
    const rect = this.el.getBoundingClientRect();
    console.log("Map element dimensions:", rect);

    // Initialize basic map
    try {
      this.map = L.map(this.el, {
        zoomControl: true,
        attributionControl: true,
        closePopupOnClick: true,
      }).setView([initialCenter.lat, initialCenter.lng], initialZoom);

      console.log("Map initialized successfully");
      window.liveSocket.pushEvent("debug_info", { map_initialized: true });
      window.debugMapInstance = this.map;
    } catch (error) {
      console.error("Error initializing map:", error);
      window.liveSocket.pushEvent("debug_info", {
        map_initialized: false,
        errors: ["Map init error: " + error.message],
      });
      return;
    }

    // Add OpenStreetMap tile layer
    try {
      const tileLayer = L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
        attribution:
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
        maxZoom: 19,
      });
      tileLayer.addTo(this.map);
      console.log("Tile layer added successfully");

      // Listen for tile layer events
      tileLayer.on("loading", () => console.log("Tiles loading..."));
      tileLayer.on("load", () => console.log("Tiles loaded"));
      tileLayer.on("tileerror", (e) => console.error("Tile error:", e));
    } catch (error) {
      console.error("Error adding tile layer:", error);
      window.liveSocket.pushEvent("debug_info", {
        errors: ["Tile layer error: " + error.message],
      });
    }

    // Force initial size calculation
    try {
      this.map.invalidateSize();
      console.log("Map size invalidated");
    } catch (error) {
      console.error("Error invalidating map size:", error);
    }

    // Track when map is ready
    this.map.whenReady(() => {
      console.log("Debug map is ready");
      console.log("Map container size:", this.map.getSize());
      console.log("Map zoom:", this.map.getZoom());
      console.log("Map center:", this.map.getCenter());

      try {
        this.pushEvent("map_ready", {});
      } catch (error) {
        console.error("Error in map ready callback:", error);
      }
    });

    // Handle resize
    window.addEventListener("resize", () => {
      console.log("Window resized, invalidating map size");
      try {
        this.map.invalidateSize();
      } catch (error) {
        console.error("Error invalidating map size on resize:", error);
      }
    });

    // Add a delayed size check
    setTimeout(() => {
      console.log("Delayed map size check");
      const rect = this.el.getBoundingClientRect();
      console.log("Map element dimensions after delay:", rect);
      if (this.map) {
        try {
          this.map.invalidateSize();
          console.log("Map size re-invalidated after delay");
        } catch (error) {
          console.error("Error re-invalidating map size:", error);
        }
      }
    }, 1000);
  },

  destroyed() {
    console.log("DebugMap hook destroyed");
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
    if (window.debugMapInstance) {
      window.debugMapInstance = null;
    }
  },
};

// APRS Map Hook
let Hooks = {};
Hooks.APRSMap = MinimalAPRSMap;
Hooks.DebugMap = DebugMap;

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
