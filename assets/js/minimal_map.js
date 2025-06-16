// Minimal APRS Map Hook - handles only basic map interaction
// All data logic handled by LiveView

let MinimalAPRSMap = {
  mounted() {
    console.log("MinimalAPRSMap hook mounted");

    // Get initial center and zoom from server-provided data attributes
    const initialCenter = JSON.parse(this.el.dataset.center);
    const initialZoom = parseInt(this.el.dataset.zoom);

    console.log("Initializing minimal map with center:", initialCenter, "and zoom:", initialZoom);

    // Initialize basic map
    this.map = L.map(this.el, {
      zoomControl: true,
      attributionControl: true,
      closePopupOnClick: true,
    }).setView([initialCenter.lat, initialCenter.lng], initialZoom);

    // Add OpenStreetMap tile layer
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
      maxZoom: 19,
    }).addTo(this.map);

    // Store markers for management
    this.markers = new Map();
    this.markerLayer = L.layerGroup().addTo(this.map);

    // Force initial size calculation
    this.map.invalidateSize();

    // Track when map is ready
    this.map.whenReady(() => {
      console.log("Minimal map is ready");
      this.pushEvent("map_ready", {});
      this.sendBoundsToServer();
    });

    // Send bounds to LiveView when map moves
    this.map.on("moveend", () => {
      if (this.boundsTimer) clearTimeout(this.boundsTimer);
      this.boundsTimer = setTimeout(() => {
        this.sendBoundsToServer();
      }, 300);
    });

    // Handle resize
    window.addEventListener("resize", () => {
      this.map.invalidateSize();
    });

    // LiveView event handlers
    this.setupLiveViewHandlers();
  },

  setupLiveViewHandlers() {
    // Add single marker
    this.handleEvent("add_marker", (data) => {
      this.addMarker(data);
    });

    // Add multiple markers at once
    this.handleEvent("add_markers", (data) => {
      if (data.markers && Array.isArray(data.markers)) {
        data.markers.forEach((marker) => this.addMarker(marker));
      }
    });

    // Remove marker
    this.handleEvent("remove_marker", (data) => {
      this.removeMarker(data.id);
    });

    // Clear all markers
    this.handleEvent("clear_markers", () => {
      this.clearAllMarkers();
    });

    // Update marker
    this.handleEvent("update_marker", (data) => {
      this.updateMarker(data);
    });

    // Zoom to location
    this.handleEvent("zoom_to_location", (data) => {
      if (data.lat && data.lng) {
        const lat = parseFloat(data.lat);
        const lng = parseFloat(data.lng);
        const zoom = parseInt(data.zoom || 12);

        this.map.setView([lat, lng], zoom, {
          animate: true,
          duration: 1,
        });
      }
    });

    // Handle geolocation requests
    this.handleEvent("request_geolocation", () => {
      if ("geolocation" in navigator) {
        navigator.geolocation.getCurrentPosition(
          (position) => {
            const { latitude, longitude } = position.coords;
            this.pushEvent("set_location", { lat: latitude, lng: longitude });
          },
          (error) => {
            console.warn("Geolocation error:", error.message);
            this.pushEvent("geolocation_error", { error: error.message });
          },
        );
      } else {
        console.warn("Geolocation not available");
        this.pushEvent("geolocation_error", { error: "Geolocation not supported" });
      }
    });

    // Handle new packets from LiveView
    this.handleEvent("new_packet", (data) => {
      this.addMarker({
        ...data,
        historical: false,
        popup: this.buildPopupContent(data),
      });
    });

    // Handle historical packets during replay
    this.handleEvent("historical_packet", (data) => {
      this.addMarker({
        ...data,
        historical: true,
        popup: this.buildPopupContent(data),
      });
    });

    // Handle refresh markers event
    this.handleEvent("refresh_markers", () => {
      // Remove markers for packets that are no longer visible
      // This could be implemented to clean up old markers based on timestamp
      console.log("Refresh markers event received");
    });

    // Handle clearing historical packets
    this.handleEvent("clear_historical_packets", () => {
      // Remove all historical markers
      const markersToRemove = [];
      this.markers.forEach((marker, id) => {
        if (marker._isHistorical) {
          markersToRemove.push(id);
        }
      });
      markersToRemove.forEach((id) => this.removeMarker(id));
    });
  },

  sendBoundsToServer() {
    if (!this.map) return;

    const bounds = this.map.getBounds();
    const center = this.map.getCenter();
    const zoom = this.map.getZoom();

    this.pushEvent("bounds_changed", {
      bounds: {
        north: bounds.getNorth(),
        south: bounds.getSouth(),
        east: bounds.getEast(),
        west: bounds.getWest(),
      },
      center: {
        lat: center.lat,
        lng: center.lng,
      },
      zoom: zoom,
    });
  },

  addMarker(data) {
    if (!data.id || !data.lat || !data.lng) {
      console.warn("Invalid marker data:", data);
      return;
    }

    const lat = parseFloat(data.lat);
    const lng = parseFloat(data.lng);

    // Validate coordinates
    if (isNaN(lat) || isNaN(lng) || lat < -90 || lat > 90 || lng < -180 || lng > 180) {
      console.warn("Invalid coordinates:", lat, lng);
      return;
    }

    // Remove existing marker if it exists
    this.removeMarker(data.id);

    // Create marker icon
    const icon = this.createMarkerIcon(data);

    // Create marker
    const marker = L.marker([lat, lng], { icon: icon });

    // Add popup if content provided
    if (data.popup) {
      marker.bindPopup(data.popup);
    }

    // Handle marker click
    marker.on("click", () => {
      this.pushEvent("marker_clicked", {
        id: data.id,
        callsign: data.callsign,
        lat: lat,
        lng: lng,
      });
    });

    // Mark historical markers for identification
    if (data.historical) {
      marker._isHistorical = true;
    }

    // Add to map and store reference
    marker.addTo(this.markerLayer);
    this.markers.set(data.id, marker);
  },

  removeMarker(id) {
    const marker = this.markers.get(id);
    if (marker) {
      this.markerLayer.removeLayer(marker);
      this.markers.delete(id);
    }
  },

  updateMarker(data) {
    if (!data.id) return;

    const existingMarker = this.markers.get(data.id);
    if (existingMarker) {
      // Update position if provided
      if (data.lat && data.lng) {
        const lat = parseFloat(data.lat);
        const lng = parseFloat(data.lng);
        if (!isNaN(lat) && !isNaN(lng)) {
          existingMarker.setLatLng([lat, lng]);
        }
      }

      // Update popup if provided
      if (data.popup) {
        existingMarker.setPopupContent(data.popup);
      }

      // Update icon if data changed
      if (data.symbol_table || data.symbol_code || data.color) {
        const newIcon = this.createMarkerIcon(data);
        existingMarker.setIcon(newIcon);
      }
    } else {
      // Marker doesn't exist, create it
      this.addMarker(data);
    }
  },

  clearAllMarkers() {
    this.markerLayer.clearLayers();
    this.markers.clear();
  },

  createMarkerIcon(data) {
    // Get symbol information
    const symbolTableId = data.symbol_table_id || "/";
    const symbolCode = data.symbol_code || ">";

    // Determine sprite file based on symbol table
    let spriteFile;
    switch (symbolTableId) {
      case "/":
        spriteFile = "/aprs-symbols/aprs-symbols-24-0.png";
        break;
      case "\\":
        spriteFile = "/aprs-symbols/aprs-symbols-24-1.png";
        break;
      default:
        spriteFile = "/aprs-symbols/aprs-symbols-24-0.png";
    }

    // Calculate sprite position
    const charCode = symbolCode.charCodeAt(0);
    const position = charCode - 32;
    const col = position % 16;
    const row = Math.floor(position / 16);
    const x = -col * 24;
    const y = -row * 24;

    // Use different opacity for historical markers
    const opacity = data.historical ? 0.7 : 1.0;

    return L.divIcon({
      html: `<div style="
        background-image: url('${spriteFile}');
        background-position: ${x}px ${y}px;
        background-repeat: no-repeat;
        width: 24px;
        height: 24px;
        opacity: ${opacity};
        background-size: auto;
      "></div>`,
      className: data.historical ? "aprs-marker historical-marker" : "aprs-marker",
      iconSize: [24, 24],
      iconAnchor: [12, 12],
      popupAnchor: [0, -12],
    });
  },

  buildPopupContent(data) {
    const callsign = data.callsign || data.id || "Unknown";
    const comment = data.comment || "";
    const symbolDesc = data.symbol_description || "Unknown symbol";

    let content = `<div class="aprs-popup">
      <div class="aprs-callsign"><strong>${callsign}</strong></div>
      <div class="aprs-symbol-info">${symbolDesc}</div>`;

    if (comment) {
      content += `<div class="aprs-comment">${comment}</div>`;
    }

    if (data.lat && data.lng) {
      content += `<div class="aprs-coords">
        ${data.lat.toFixed(4)}, ${data.lng.toFixed(4)}
      </div>`;
    }

    content += `</div>`;
    return content;
  },

  destroyed() {
    console.log("MinimalAPRSMap hook destroyed");

    // Clean up timers
    if (this.boundsTimer) {
      clearTimeout(this.boundsTimer);
    }

    // Clean up markers
    if (this.markerLayer) {
      this.markerLayer.clearLayers();
    }

    if (this.markers) {
      this.markers.clear();
    }

    // Clean up map
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
  },
};

export default MinimalAPRSMap;
