// Map bundle entry point - combines Leaflet and map plugins
import "../vendor/js/leaflet-minimal.js";
import "../vendor/js/plugins-optimized.js";

// Ensure Leaflet is available as window.L (standard convention)
if (window.leaflet && !window.L) {
  window.L = window.leaflet;
}

// Mark bundle as loaded
window.mapBundleLoaded = true;