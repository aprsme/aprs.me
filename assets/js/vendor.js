// Vendor libraries bundle
// This file loads all external libraries and makes them available globally

// IMPORTANT: Order matters! Dependencies must be loaded before their plugins

// 1. Load Leaflet first (required by plugins)
import '../vendor/leaflet.js';

// 2. Load Leaflet plugins (they expect window.L to exist)
import '../vendor/leaflet-heat.js';
import '../vendor/leaflet.markercluster.js';
import '../vendor/oms.min.js';

// 3. Load Chart.js BEFORE the date adapter
// The UMD build automatically sets window.Chart
import '../vendor/chart.umd.js';

// 4. Load Chart.js date adapter AFTER Chart.js
// The adapter needs Chart to be available globally to register itself
import '../vendor/chartjs-adapter-date-fns.bundle.min.js';

// Verify libraries are loaded
if (typeof window !== 'undefined') {
  console.log('Vendor libraries loaded:', {
    'Leaflet': !!window.L,
    'Chart.js': !!window.Chart,
    'Chart.adapters': !!(window.Chart && window.Chart.adapters)
  });
}