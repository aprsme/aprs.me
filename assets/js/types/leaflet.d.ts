declare namespace L {
  class Map {
    constructor(element: string | HTMLElement, options?: MapOptions);
    setView(center: LatLngExpression, zoom: number, options?: ZoomOptions): this;
    getCenter(): LatLng;
    getZoom(): number;
    getBounds(): LatLngBounds;
    remove(): void;
    invalidateSize(options?: { animate?: boolean; pan?: boolean }): this;
    whenReady(callback: () => void): this;
    on(event: string, handler: (e: import('./leaflet-events').LeafletEvent) => void): this;
    getContainer(): HTMLElement;
    project(latlng: LatLngExpression, zoom?: number): Point;
    unproject(point: PointExpression, zoom?: number): LatLng;
    latLngToContainerPoint(latlng: LatLngExpression): Point;
    panTo(latlng: LatLngExpression, options?: PanOptions): this;
  }

  interface MapOptions {
    zoomControl?: boolean;
    attributionControl?: boolean;
    closePopupOnClick?: boolean;
    tap?: boolean;
    tapTolerance?: number;
    touchZoom?: boolean;
    bounceAtZoomLimits?: boolean;
    worldCopyJump?: boolean;
    preferCanvas?: boolean;
    zoomAnimation?: boolean;
    fadeAnimation?: boolean;
    markerZoomAnimation?: boolean;
  }

  interface ZoomOptions {
    animate?: boolean;
    duration?: number;
  }

  interface PanOptions {
    animate?: boolean;
    duration?: number;
    easeLinearity?: number;
    noMoveStart?: boolean;
  }

  class LatLng {
    lat: number;
    lng: number;
    constructor(lat: number, lng: number);
  }

  type LatLngExpression = LatLng | [number, number];
  type PointExpression = Point | [number, number];

  class Point {
    x: number;
    y: number;
    constructor(x: number, y: number);
  }

  class Popup {
    getLatLng(): LatLng;
    getElement(): HTMLElement | undefined;
  }

  class LatLngBounds {
    constructor(southWest: LatLngExpression, northEast: LatLngExpression);
    getNorth(): number;
    getSouth(): number;
    getEast(): number;
    getWest(): number;
  }

  class Marker {
    constructor(latLng: LatLngExpression, options?: MarkerOptions);
    setLatLng(latLng: LatLngExpression): this;
    bindPopup(content: string | HTMLElement, options?: PopupOptions): this;
    unbindPopup(): this;
    openPopup(): this;
    closePopup(): this;
    setPopupContent(content: string | HTMLElement): this;
    setIcon(icon: DivIcon): this;
    getLatLng(): LatLng;
  }

  interface MarkerOptions {
    icon?: DivIcon;
  }

  interface PopupOptions {
    autoPan?: boolean;
  }

  class DivIcon {
    constructor(options: DivIconOptions);
  }

  interface DivIconOptions {
    html?: string | HTMLElement;
    className?: string;
    iconSize?: [number, number];
    iconAnchor?: [number, number];
  }

  class LayerGroup {
    addTo(map: Map): this;
    clearLayers(): this;
    removeLayer(layer: Layer): this;
  }

  interface Layer {
    _leaflet_id?: number;
  }

  class TileLayer {
    constructor(urlTemplate: string, options?: TileLayerOptions);
    addTo(map: Map): this;
  }

  interface TileLayerOptions {
    attribution?: string;
    maxZoom?: number;
  }

  function map(element: string | HTMLElement, options?: MapOptions): Map;
  function marker(latLng: LatLngExpression, options?: MarkerOptions): Marker;
  function tileLayer(urlTemplate: string, options?: TileLayerOptions): TileLayer;
  function latLngBounds(southWest: LatLngExpression, northEast: LatLngExpression): LatLngBounds;
  function divIcon(options: DivIconOptions): DivIcon;
  function layerGroup(): LayerGroup;
  function latLng(lat: number, lng: number): LatLng;
  function DomUtil(): {
    get(id: string): HTMLElement | null;
    remove(el: HTMLElement): void;
  };
}
