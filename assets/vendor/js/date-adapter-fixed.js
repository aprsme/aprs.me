/*!
 * chartjs-adapter-date-fns v3.0.0 - Fixed version
 * Handles Chart.js dependency properly
 */
(function() {
  // Wait for Chart.js to be available
  function loadAdapter() {
    if (typeof window !== 'undefined' && window.Chart) {
      // Chart.js is available, load the adapter
      const adapterCode = function(t) {
        // Date-fns adapter code here
        ${fs.readFileSync('/Users/graham/dev/aprs.me/assets/vendor/js/date-adapter.js', 'utf8').replace(/!function\(t,e\)\{[^}]+\}/, 'function(t) {')}
      };
      
      // Execute the adapter with Chart.js
      adapterCode(window.Chart);
    } else {
      // Chart.js not ready yet, try again
      setTimeout(loadAdapter, 50);
    }
  }
  
  // Start loading
  loadAdapter();
})();