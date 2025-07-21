// Vendor libraries bundle
// This file loads all external libraries and makes them available globally

// Load Leaflet first (required by plugins)
import '../vendor/leaflet.js';

// Load Leaflet plugins (they expect window.L to exist)
import '../vendor/leaflet-heat.js';
import '../vendor/leaflet.markercluster.js';
import '../vendor/oms.min.js';

// Load Chart.js
import '../vendor/chart.umd.js';

// Load Chart.js adapter (requires Chart to be loaded first)
import '../vendor/chartjs-adapter-date-fns.bundle.min.js';

// Ensure global availability
if (typeof window !== 'undefined') {
  // Leaflet should already be on window.L from the library
  // Chart should already be on window.Chart from the library
  console.log('Vendor libraries loaded: Leaflet', !!window.L, 'Chart.js', !!window.Chart);
}