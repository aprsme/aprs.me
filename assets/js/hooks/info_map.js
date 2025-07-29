// Simple map hook for displaying a single station on the info page
export const InfoMap = {
  mounted() {
    this.initializeMap();
  },

  updated() {
    // When the element updates, check if we need to update the marker
    const lat = parseFloat(this.el.dataset.lat);
    const lon = parseFloat(this.el.dataset.lon);
    const symbolHtml = this.el.dataset.symbolHtml;

    // Validate coordinates
    if (isNaN(lat) || isNaN(lon)) {
      return;
    }

    // If map doesn't exist yet, initialize it
    if (!this.map) {
      this.initializeMap();
      return;
    }

    // Update marker position if it changed
    if (this.marker) {
      const currentPos = this.marker.getLatLng();
      if (currentPos.lat !== lat || currentPos.lng !== lon) {
        // Animate the marker to the new position
        this.marker.setLatLng([lat, lon]);
        
        // Update the popup content
        const callsign = this.el.dataset.callsign;
        this.marker.setPopupContent(`<strong>${callsign}</strong><br/>Lat: ${lat.toFixed(6)}<br/>Lon: ${lon.toFixed(6)}`);
        
        // Optionally pan the map to the new position with animation
        this.map.panTo([lat, lon], { animate: true, duration: 1 });
      }

      // Update marker icon if symbol changed
      if (symbolHtml !== this.lastSymbolHtml) {
        const markerIcon = this.createMarkerIcon(symbolHtml);
        this.marker.setIcon(markerIcon);
        this.lastSymbolHtml = symbolHtml;
      }
    }
  },

  initializeMap() {
    // Check if Leaflet is available
    if (typeof L === "undefined") {
      console.error("Leaflet not loaded for InfoMap");
      return;
    }

    // Get data from element attributes
    const lat = parseFloat(this.el.dataset.lat);
    const lon = parseFloat(this.el.dataset.lon);
    const zoom = parseInt(this.el.dataset.zoom) || 13;
    const callsign = this.el.dataset.callsign;
    const symbolHtml = this.el.dataset.symbolHtml;

    // Validate coordinates
    if (isNaN(lat) || isNaN(lon)) {
      console.error("Invalid coordinates for InfoMap");
      return;
    }

    // Hide loading spinner
    const loadingEl = this.el.querySelector(`#${this.el.id}-loading`);
    if (loadingEl) {
      loadingEl.style.display = 'none';
    }

    // Initialize the map
    try {
      this.map = L.map(this.el, {
        center: [lat, lon],
        zoom: zoom,
        scrollWheelZoom: false, // Disable scroll zoom for embedded maps
        zoomControl: true,
        attributionControl: true
      });

      // Add tile layer
      L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
        maxZoom: 19
      }).addTo(this.map);

      // Create marker icon
      const markerIcon = this.createMarkerIcon(symbolHtml);
      this.lastSymbolHtml = symbolHtml;

      // Add marker for the station
      this.marker = L.marker([lat, lon], { icon: markerIcon })
        .addTo(this.map)
        .bindPopup(`<strong>${callsign}</strong><br/>Lat: ${lat.toFixed(6)}<br/>Lon: ${lon.toFixed(6)}`);

      // Invalidate size after a short delay to ensure proper rendering
      setTimeout(() => {
        if (this.map) {
          this.map.invalidateSize();
        }
      }, 250);

    } catch (error) {
      console.error("Error initializing InfoMap:", error);
    }
  },

  createMarkerIcon(symbolHtml) {
    if (symbolHtml) {
      // Use the APRS symbol if provided
      return L.divIcon({
        html: symbolHtml,
        className: 'aprs-info-marker',
        iconSize: [32, 32],
        iconAnchor: [16, 16]
      });
    } else {
      // Default marker
      return L.divIcon({
        html: `<div style="
          width: 24px;
          height: 24px;
          background-color: #3b82f6;
          border: 3px solid white;
          border-radius: 50%;
          box-shadow: 0 2px 5px rgba(0,0,0,0.3);
        "></div>`,
        className: '',
        iconSize: [24, 24],
        iconAnchor: [12, 12]
      });
    }
  },

  destroyed() {
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
  }
};