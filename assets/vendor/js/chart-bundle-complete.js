// Chart.js bundle with date-fns adapter
// This file combines Chart.js and the date adapter in the correct order

// First, include Chart.js
import './chart-minimal.js';

// Then include the date adapter
// The adapter expects Chart.js to be available globally
import './date-adapter.js';

// Export Chart for module usage if needed
export default window.Chart;