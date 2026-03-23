// Simple map hook for displaying a single station on the info page

declare const L: typeof import("leaflet");

interface InfoMapContext {
  el: HTMLElement;
  map: import("leaflet").Map | null;
  marker: import("leaflet").Marker | null;
  lastSymbolHtml: string | null;
  initializing: boolean;
  resizeTimer: ReturnType<typeof setTimeout> | null;
  destroyed?: boolean;
}

export const InfoMap = {
  mounted(this: InfoMapContext) {
    this.map = null;
    this.marker = null;
    this.lastSymbolHtml = null;
    this.initializing = false;
    this.resizeTimer = null;
    this.destroyed = false;
    initializeMap.call(this);
  },

  updated(this: InfoMapContext) {
    const lat = parseFloat(this.el.dataset.lat || "");
    const lon = parseFloat(this.el.dataset.lon || "");
    const symbolHtml = this.el.dataset.symbolHtml || null;
    const callsign = this.el.dataset.callsign || "unknown";

    if (isNaN(lat) || isNaN(lon)) {
      console.warn(
        `InfoMap: Invalid coordinates lat=${lat}, lon=${lon} for ${callsign} during update`,
      );
      return;
    }

    if (
      !this.map ||
      !(this.map as any)._container ||
      !(this.map as any)._container.parentNode
    ) {
      initializeMap.call(this);
      return;
    }

    if (this.marker) {
      const currentPos = this.marker.getLatLng();
      if (currentPos.lat !== lat || currentPos.lng !== lon) {
        this.marker.setLatLng([lat, lon]);

        this.marker.setPopupContent(
          `<strong>${callsign}</strong><br/>Lat: ${lat.toFixed(6)}<br/>Lon: ${lon.toFixed(6)}`,
        );

        this.map!.panTo([lat, lon], { animate: true, duration: 1 });
      }

      if (symbolHtml !== this.lastSymbolHtml) {
        const markerIcon = createMarkerIcon(symbolHtml);
        this.marker.setIcon(markerIcon);
        this.lastSymbolHtml = symbolHtml;
      }
    }
  },

  destroyed(this: InfoMapContext) {
    this.destroyed = true;
    if (this.resizeTimer) {
      clearTimeout(this.resizeTimer);
      this.resizeTimer = null;
    }
    if (this.map) {
      this.map.remove();
      this.map = null;
    }
    this.marker = null;
    this.lastSymbolHtml = null;
    this.initializing = false;
  },
};

function initializeMap(this: InfoMapContext) {
  if (this.initializing || this.destroyed) return;

  if (typeof L === "undefined") {
    console.error("Leaflet not loaded for InfoMap");
    return;
  }

  this.initializing = true;

  const lat = parseFloat(this.el.dataset.lat || "");
  const lon = parseFloat(this.el.dataset.lon || "");
  const zoom = parseInt(this.el.dataset.zoom || "13") || 13;
  const callsign = this.el.dataset.callsign || "unknown";
  const symbolHtml = this.el.dataset.symbolHtml || null;

  if (isNaN(lat) || isNaN(lon)) {
    console.warn(
      `InfoMap: Invalid coordinates lat=${lat}, lon=${lon} for ${callsign}`,
    );
    this.initializing = false;
    return;
  }

  const loadingEl = this.el.querySelector(
    `#${this.el.id}-loading`,
  ) as HTMLElement | null;
  if (loadingEl) {
    loadingEl.style.display = "none";
  }

  try {
    this.map = L.map(this.el, {
      center: [lat, lon],
      zoom: zoom,
      scrollWheelZoom: false,
      zoomControl: true,
      attributionControl: true,
    });

    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      attribution:
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
      maxZoom: 19,
    }).addTo(this.map);

    const markerIcon = createMarkerIcon(symbolHtml);
    this.lastSymbolHtml = symbolHtml;

    this.marker = L.marker([lat, lon], { icon: markerIcon })
      .addTo(this.map)
      .bindPopup(
        `<strong>${callsign}</strong><br/>Lat: ${lat.toFixed(6)}<br/>Lon: ${lon.toFixed(6)}`,
      );

    const map = this.map;
    this.resizeTimer = setTimeout(() => {
      if (!this.destroyed && this.el.isConnected && map && this.map === map) {
        map.invalidateSize();
      }
      this.resizeTimer = null;
    }, 250);

    this.initializing = false;
  } catch (error) {
    console.error("Error initializing InfoMap:", error);
    this.initializing = false;
  }
}

function createMarkerIcon(symbolHtml: string | null): import("leaflet").DivIcon {
  if (symbolHtml) {
    return L.divIcon({
      html: symbolHtml,
      className: "aprs-info-marker",
      iconSize: [32, 32],
      iconAnchor: [16, 16],
    });
  } else {
    return L.divIcon({
      html: `<div style="
        width: 24px;
        height: 24px;
        background-color: #3b82f6;
        border: 3px solid white;
        border-radius: 50%;
        box-shadow: 0 2px 5px rgba(0,0,0,0.3);
      "></div>`,
      className: "",
      iconSize: [24, 24],
      iconAnchor: [12, 12],
    });
  }
}
