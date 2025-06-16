// Minimal APRS Map Hook - handles only basic map interaction
// All data logic handled by LiveView

let MinimalAPRSMap = {
  mounted() {
    console.log("MinimalAPRSMap hook mounted");
    console.log("Element:", this.el);
    console.log("Element dataset:", this.el.dataset);

    // Initialize error tracking
    this.errors = [];
    this.initializationAttempts = 0;
    this.maxInitializationAttempts = 3;

    this.attemptInitialization();
  },

  attemptInitialization() {
    this.initializationAttempts++;
    console.log(
      `Initialization attempt ${this.initializationAttempts}/${this.maxInitializationAttempts}`,
    );

    // Check if Leaflet is available
    if (typeof L === "undefined") {
      console.error("Leaflet library not loaded!");
      this.errors.push("Leaflet library not available");

      if (this.initializationAttempts < this.maxInitializationAttempts) {
        console.log("Retrying initialization in 1 second...");
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

      console.log("Parsed data - center:", initialCenter, "zoom:", initialZoom);
    } catch (error) {
      console.error("Error parsing map data attributes:", error);
      console.log("Raw center data:", this.el.dataset.center);
      console.log("Raw zoom data:", this.el.dataset.zoom);

      // Fallback values
      initialCenter = { lat: 39.8283, lng: -98.5795 };
      initialZoom = 5;
      console.log("Using fallback values:", initialCenter, initialZoom);
    }

    this.initializeMap(initialCenter, initialZoom);
  },

  initializeMap(initialCenter, initialZoom) {
    console.log("Initializing minimal map with center:", initialCenter, "and zoom:", initialZoom);

    // Check element dimensions
    const rect = this.el.getBoundingClientRect();
    console.log("Map element dimensions:", rect);

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
      console.log("Map initialized successfully");
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
      console.log("Tile layer added successfully");

      // Listen for tile layer events
      tileLayer.on("loading", () => console.log("Tiles loading..."));
      tileLayer.on("load", () => console.log("Tiles loaded"));
      tileLayer.on("tileerror", (e) => console.error("Tile error:", e));
    } catch (error) {
      console.error("Error adding tile layer:", error);
      this.errors.push("Tile layer failed: " + error.message);
    }

    // Store markers for management
    this.markers = new Map();
    this.markerLayer = L.layerGroup().addTo(this.map);

    // Force initial size calculation
    try {
      this.map.invalidateSize();
      console.log("Map size invalidated");
    } catch (error) {
      console.error("Error invalidating map size:", error);
    }

    // Track when map is ready
    this.map.whenReady(() => {
      console.log("Minimal map is ready");
      console.log("Map container size:", this.map.getSize());
      console.log("Map zoom:", this.map.getZoom());
      console.log("Map center:", this.map.getCenter());

      try {
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

    // Handle resize
    this.resizeHandler = () => {
      console.log("Window resized, invalidating map size");
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
      console.log("Delayed map size check");
      const rect = this.el.getBoundingClientRect();
      console.log("Map element dimensions after delay:", rect);
      if (this.map) {
        try {
          this.map.invalidateSize();
          console.log("Map size re-invalidated after delay");
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

    // Clean up map
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
  },
};

export default MinimalAPRSMap;
