// Minimal APRS Map Hook - handles only basic map interaction
// All data logic handled by LiveView

let MinimalAPRSMap = {
  mounted() {
    // Initialize error tracking
    this.errors = [];
    this.initializationAttempts = 0;
    this.maxInitializationAttempts = 3;

    this.attemptInitialization();
  },

  attemptInitialization() {
    this.initializationAttempts++;

    // Check if Leaflet is available
    if (typeof L === "undefined") {
      console.error("Leaflet library not loaded!");
      this.errors.push("Leaflet library not available");

      if (this.initializationAttempts < this.maxInitializationAttempts) {
        setTimeout(() => this.attemptInitialization(), 1000);
        return;
      } else {
        this.handleFatalError("Leaflet library failed to load after multiple attempts");
        return;
      }
    }

    // Get initial center and zoom from server-provided data attributes
    let initialCenter, initialZoom;
    try {
      const centerData = this.el.dataset.center;
      const zoomData = this.el.dataset.zoom;

      if (!centerData || !zoomData) {
        throw new Error("Missing map data attributes");
      }

      initialCenter = JSON.parse(centerData);
      initialZoom = parseInt(zoomData);

      // Validate parsed data
      if (
        !initialCenter ||
        typeof initialCenter.lat !== "number" ||
        typeof initialCenter.lng !== "number"
      ) {
        throw new Error("Invalid center data");
      }

      if (isNaN(initialZoom) || initialZoom < 1 || initialZoom > 20) {
        throw new Error("Invalid zoom data");
      }
    } catch (error) {
      console.error("Error parsing map data attributes:", error);

      // Fallback values
      initialCenter = { lat: 39.8283, lng: -98.5795 };
      initialZoom = 5;
    }

    this.initializeMap(initialCenter, initialZoom);
  },

  initializeMap(initialCenter, initialZoom) {
    // Check element dimensions
    const rect = this.el.getBoundingClientRect();

    // Validate element has dimensions
    if (rect.width === 0 || rect.height === 0) {
      console.warn("Map element has no dimensions, retrying...");
      this.errors.push("Map element has zero dimensions");

      if (this.initializationAttempts < this.maxInitializationAttempts) {
        setTimeout(() => this.attemptInitialization(), 500);
        return;
      } else {
        this.handleFatalError("Map element never gained proper dimensions");
        return;
      }
    }

    // Initialize basic map
    try {
      this.map = L.map(this.el, {
        zoomControl: true,
        attributionControl: true,
        closePopupOnClick: true,
      }).setView([initialCenter.lat, initialCenter.lng], initialZoom);
    } catch (error) {
      console.error("Error initializing map:", error);
      this.errors.push("Map initialization failed: " + error.message);

      if (this.initializationAttempts < this.maxInitializationAttempts) {
        setTimeout(() => this.attemptInitialization(), 1000);
        return;
      } else {
        this.handleFatalError("Map initialization failed after multiple attempts");
        return;
      }
    }

    // Add OpenStreetMap tile layer
    try {
      const tileLayer = L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
        attribution:
          '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors | APRS.me',
        maxZoom: 19,
      });
      tileLayer.addTo(this.map);
    } catch (error) {
      this.errors.push("Tile layer failed: " + error.message);
    }

    // Store markers for management
    this.markers = new Map();
    this.markerLayer = L.layerGroup().addTo(this.map);

    // Track marker states to prevent unnecessary operations
    this.markerStates = new Map();

    // Force initial size calculation
    try {
      this.map.invalidateSize();
    } catch (error) {
      console.error("Error invalidating map size:", error);
    }

    // Track when map is ready
    this.map.whenReady(() => {
      try {
        this.lastZoom = this.map.getZoom();
        this.pushEvent("map_ready", {});
        this.sendBoundsToServer();
      } catch (error) {
        console.error("Error in map ready callback:", error);
      }
    });

    // Send bounds to LiveView when map moves
    this.map.on("moveend", () => {
      if (this.boundsTimer) clearTimeout(this.boundsTimer);
      this.boundsTimer = setTimeout(() => {
        this.sendBoundsToServer();
      }, 300);
    });

    // Handle zoom changes with optimization for large zoom differences
    this.map.on("zoomend", () => {
      if (this.boundsTimer) clearTimeout(this.boundsTimer);
      this.boundsTimer = setTimeout(() => {
        const currentZoom = this.map.getZoom();
        const zoomDifference = this.lastZoom ? Math.abs(currentZoom - this.lastZoom) : 0;

        // If zoom changed significantly (more than 2 levels), clear and reload
        if (zoomDifference > 2) {
          this.pushEvent("clear_and_reload_markers", {});
        }

        this.sendBoundsToServer();
        this.lastZoom = currentZoom;
      }, 300);
    });

    // Handle resize
    this.resizeHandler = () => {
      try {
        if (this.map) {
          this.map.invalidateSize();
        }
      } catch (error) {
        console.error("Error invalidating map size on resize:", error);
      }
    };
    window.addEventListener("resize", this.resizeHandler);

    // Add a delayed size check
    setTimeout(() => {
      const rect = this.el.getBoundingClientRect();
      if (this.map) {
        try {
          this.map.invalidateSize();
        } catch (error) {
          console.error("Error re-invalidating map size:", error);
        }
      }
    }, 1000);

    // LiveView event handlers
    this.setupLiveViewHandlers();
  },

  handleFatalError(message) {
    console.error("Fatal map error:", message);
    console.error("All errors:", this.errors);

    // Display error message to user
    if (this.el) {
      this.el.innerHTML = `
        <div style="
          display: flex;
          justify-content: center;
          align-items: center;
          height: 100%;
          background: #f8f9fa;
          color: #721c24;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          text-align: center;
          padding: 20px;
        ">
          <div>
            <h3 style="margin: 0 0 10px 0;">Map Loading Error</h3>
            <p style="margin: 0; font-size: 14px;">${message}</p>
            <p style="margin: 10px 0 0 0; font-size: 12px; color: #856404;">
              Please refresh the page or check the browser console for details.
            </p>
          </div>
        </div>
      `;
    }
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
      if (!this.map) {
        console.error("Map not initialized, cannot zoom");
        return;
      }

      if (data.lat && data.lng) {
        const lat = parseFloat(data.lat);
        const lng = parseFloat(data.lng);
        const zoom = parseInt(data.zoom || 12);

        // Validate coordinates
        if (isNaN(lat) || isNaN(lng) || lat < -90 || lat > 90 || lng < -180 || lng > 180) {
          console.error("Invalid coordinates for zoom:", lat, lng);
          return;
        }

        if (isNaN(zoom) || zoom < 1 || zoom > 20) {
          console.error("Invalid zoom level:", zoom);
          return;
        }

        try {
          // Check element dimensions before zoom
          const beforeRect = this.el.getBoundingClientRect();

          // Force map size recalculation before zoom
          this.map.invalidateSize();

          // Use a slight delay to ensure map is ready
          setTimeout(() => {
            if (this.map) {
              this.map.setView([lat, lng], zoom, {
                animate: true,
                duration: 1,
              });

              // Check element dimensions after zoom
              setTimeout(() => {
                const afterRect = this.el.getBoundingClientRect();

                if (afterRect.width === 0 || afterRect.height === 0) {
                  console.error("Map element lost dimensions after zoom!");
                  // Try to restore dimensions
                  this.el.style.width = "100vw";
                  this.el.style.height = "100vh";
                  this.map.invalidateSize();
                }
              }, 1000);
            }
          }, 100);
        } catch (error) {
          console.error("Error during zoom operation:", error);
        }
      } else {
        console.warn("Missing lat/lng data for zoom operation:", data);
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
      // Remove markers that are outside current bounds
      if (this.map) {
        const bounds = this.map.getBounds();
        this.removeMarkersOutsideBounds(bounds);
      }
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

    // Handle bounds-based marker filtering
    this.handleEvent("filter_markers_by_bounds", (data) => {
      if (data.bounds) {
        // Create Leaflet bounds object from server data
        const bounds = L.latLngBounds(
          [data.bounds.south, data.bounds.west],
          [data.bounds.north, data.bounds.east],
        );
        this.removeMarkersOutsideBounds(bounds);
      }
    });

    // Handle clearing all markers and reloading visible ones
    this.handleEvent("clear_and_reload_markers", () => {
      // This event is just a trigger - the server will handle clearing and adding markers
    });
  },

  sendBoundsToServer() {
    if (!this.map) return;

    const bounds = this.map.getBounds();
    const center = this.map.getCenter();
    const zoom = this.map.getZoom();

    // Remove markers that are now outside the visible bounds
    this.removeMarkersOutsideBounds(bounds);

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

    // Check if marker already exists with same position and data
    const existingMarker = this.markers.get(data.id);
    const existingState = this.markerStates.get(data.id);

    if (existingMarker && existingState) {
      // Check if marker needs updating
      const currentPos = existingMarker.getLatLng();
      const positionChanged =
        Math.abs(currentPos.lat - lat) > 0.0001 || Math.abs(currentPos.lng - lng) > 0.0001;
      const dataChanged =
        existingState.symbol_table !== data.symbol_table ||
        existingState.symbol_code !== data.symbol_code ||
        existingState.popup !== data.popup;

      if (!positionChanged && !dataChanged) {
        // No changes needed, skip update
        return;
      }
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

    // Store marker state for optimization
    this.markerStates.set(data.id, {
      lat: lat,
      lng: lng,
      symbol_table: data.symbol_table,
      symbol_code: data.symbol_code,
      popup: data.popup,
      historical: data.historical,
    });
  },

  removeMarker(id) {
    const marker = this.markers.get(id);
    if (marker) {
      this.markerLayer.removeLayer(marker);
      this.markers.delete(id);
      this.markerStates.delete(id);
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
    this.markerStates.clear();
  },

  removeMarkersOutsideBounds(bounds) {
    if (!bounds || !this.markers) return;

    const markersToRemove = [];

    this.markers.forEach((marker, id) => {
      const position = marker.getLatLng();
      const lat = position.lat;
      const lng = position.lng;

      // Check latitude bounds (straightforward)
      const latOutOfBounds = lat < bounds.getSouth() || lat > bounds.getNorth();

      // Check longitude bounds (handle potential wrapping)
      let lngOutOfBounds;
      if (bounds.getWest() <= bounds.getEast()) {
        // Normal case: bounds don't cross antimeridian
        lngOutOfBounds = lng < bounds.getWest() || lng > bounds.getEast();
      } else {
        // Bounds cross antimeridian (e.g., west=170, east=-170)
        lngOutOfBounds = lng < bounds.getWest() && lng > bounds.getEast();
      }

      // Check if marker is outside the current bounds
      if (latOutOfBounds || lngOutOfBounds) {
        markersToRemove.push(id);
      }
    });

    // Remove out-of-bounds markers
    markersToRemove.forEach((id) => this.removeMarker(id));
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
      <div class="aprs-callsign"><strong><a href="/${callsign}">${callsign}</a></strong></div>
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
    // Clean up timers
    if (this.boundsTimer) {
      clearTimeout(this.boundsTimer);
    }

    // Clean up event listeners
    if (this.resizeHandler) {
      window.removeEventListener("resize", this.resizeHandler);
    }

    // Clean up markers
    if (this.markerLayer) {
      this.markerLayer.clearLayers();
    }

    if (this.markers) {
      this.markers.clear();
    }

    if (this.markerStates) {
      this.markerStates.clear();
    }

    // Clean up map
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
  },
};

export default MinimalAPRSMap;
