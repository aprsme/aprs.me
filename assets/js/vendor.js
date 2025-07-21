// Vendor bundle for all third-party dependencies
// This file bundles all npm packages to avoid resolution issues during Docker builds

// Import CSS files first
import 'leaflet/dist/leaflet.css';
import 'leaflet.markercluster/dist/MarkerCluster.css';
import 'leaflet.markercluster/dist/MarkerCluster.Default.css';

// Export Leaflet and plugins
import * as L from 'leaflet';
import 'leaflet.heat';
import 'leaflet.markercluster';
import 'overlapping-marker-spiderfier-leaflet';

// Export Chart.js and adapter
import Chart from 'chart.js/auto';
import 'chartjs-adapter-date-fns';

// Export topbar
import topbar from 'topbar';

// Make libraries available globally
window.L = L;
window.Chart = Chart;
window.topbar = topbar;

// Export for ES modules
export { L, Chart, topbar };