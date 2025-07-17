// Type definitions for Leaflet plugins used in APRS.me

import type { LatLng, Layer, LayerOptions, Map, Marker } from 'leaflet';

// Leaflet.heat plugin types
export interface HeatLatLng extends Array<number> {
  0: number; // latitude
  1: number; // longitude
  2?: number; // intensity
}

export interface HeatLayerOptions {
  minOpacity?: number;
  maxZoom?: number;
  max?: number;
  radius?: number;
  blur?: number;
  gradient?: Record<number, string>;
}

export interface HeatLayer extends Layer {
  setLatLngs(latlngs: HeatLatLng[]): this;
  addLatLng(latlng: HeatLatLng): this;
  setOptions(options: HeatLayerOptions): this;
  redraw(): this;
}

// Leaflet.markercluster plugin types
export interface MarkerClusterGroupOptions extends LayerOptions {
  maxClusterRadius?: number | ((zoom: number) => number);
  iconCreateFunction?: (cluster: MarkerCluster) => L.Icon | L.DivIcon;
  clusterPane?: string;
  spiderfyOnMaxZoom?: boolean;
  showCoverageOnHover?: boolean;
  zoomToBoundsOnClick?: boolean;
  singleMarkerMode?: boolean;
  disableClusteringAtZoom?: number;
  removeOutsideVisibleBounds?: boolean;
  animate?: boolean;
  animateAddingMarkers?: boolean;
  spiderfyDistanceMultiplier?: number;
  spiderLegPolylineOptions?: L.PolylineOptions;
  chunkedLoading?: boolean;
  chunkInterval?: number;
  chunkDelay?: number;
  chunkProgress?: (processed: number, total: number, elapsed: number) => void;
}

export interface MarkerCluster extends Marker {
  getChildCount(): number;
  getAllChildMarkers(): Marker[];
  spiderfy(): void;
  unspiderfy(): void;
}

export interface MarkerClusterGroup extends Layer {
  addLayer(layer: Layer): this;
  removeLayer(layer: Layer): this;
  clearLayers(): this;
  getVisibleParent(marker: Marker): Marker | MarkerCluster | null;
  refreshClusters(layerOrLayers?: Layer | Layer[]): this;
  getLayer(id: number): Layer | undefined;
  getLayers(): Layer[];
  hasLayer(layer: Layer): boolean;
  zoomToShowLayer(layer: Layer, callback?: () => void): void;
}

// OverlappingMarkerSpiderfier types
export interface OMSOptions {
  keepSpiderfied?: boolean;
  nearbyDistance?: number;
  circleSpiralSwitchover?: number;
  circleFootSeparation?: number;
  spiralFootSeparation?: number;
  spiralLengthStart?: number;
  spiralLengthFactor?: number;
  legWeight?: number;
  legColors?: {
    usual?: string;
    highlighted?: string;
  };
}

export interface OverlappingMarkerSpiderfierStatic {
  new(map: Map, options?: OMSOptions): OverlappingMarkerSpiderfier;
}

export interface OverlappingMarkerSpiderfier {
  addMarker(marker: Marker): void;
  removeMarker(marker: Marker): void;
  getMarkers(): Marker[];
  clearMarkers(): void;
  addListener(event: 'click', handler: (marker: Marker) => void): void;
  addListener(event: 'spiderfy', handler: (markers: Marker[]) => void): void;
  addListener(event: 'unspiderfy', handler: (markers: Marker[]) => void): void;
  removeListener(event: string, handler: Function): void;
  clearListeners(event?: string): void;
  unspiderfy(): void;
}

// Extend the global L namespace
declare global {
  namespace L {
    function heatLayer(latlngs: HeatLatLng[], options?: HeatLayerOptions): HeatLayer;
    function markerClusterGroup(options?: MarkerClusterGroupOptions): MarkerClusterGroup;
  }
}

// Export the constructor type for OverlappingMarkerSpiderfier
export const OverlappingMarkerSpiderfier: OverlappingMarkerSpiderfierStatic;