// Simple map hook for displaying a single station on the info page
export const InfoMap = {
  mounted() {
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
      let markerIcon;
      if (symbolHtml) {
        // Use the APRS symbol if provided
        markerIcon = L.divIcon({
          html: symbolHtml,
          className: 'aprs-info-marker',
          iconSize: [32, 32],
          iconAnchor: [16, 16]
        });
      } else {
        // Default marker
        markerIcon = L.divIcon({
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

      // Add marker for the station
      const marker = L.marker([lat, lon], { icon: markerIcon })
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

  destroyed() {
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
  }
};