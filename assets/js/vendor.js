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

// 4. Create a fake CommonJS environment for the date adapter
// The adapter expects to find Chart.js via require()
if (typeof window !== 'undefined' && !window.require) {
  window.require = function(module) {
    if (module === 'chart.js') {
      return window.Chart;
    }
    throw new Error('Module not found: ' + module);
  };
}

// 5. Load Chart.js date adapter AFTER Chart.js and require shim
// The adapter needs Chart to be available globally to register itself
import '../vendor/chartjs-adapter-date-fns.bundle.min.js';

// 6. Clean up the require shim
if (typeof window !== 'undefined' && window.require) {
  delete window.require;
}

// Verify libraries are loaded
if (typeof window !== 'undefined') {
  console.log('Vendor libraries loaded:', {
    'Leaflet': !!window.L,
    'Chart.js': !!window.Chart,
    'Chart.adapters': !!(window.Chart && window.Chart._adapters)
  });
}