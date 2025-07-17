// Type definitions for Leaflet event objects

import type { LatLng, Point, LatLngBounds } from 'leaflet';

export interface LeafletEvent {
  type: string;
  target: any;
}

export interface LeafletMouseEvent extends LeafletEvent {
  latlng: LatLng;
  layerPoint: Point;
  containerPoint: Point;
  originalEvent: MouseEvent;
}

export interface LeafletLocationEvent extends LeafletEvent {
  latlng: LatLng;
  bounds: LatLngBounds;
  accuracy: number;
  altitude: number;
  altitudeAccuracy: number;
  heading: number;
  speed: number;
  timestamp: number;
}

export interface LeafletResizeEvent extends LeafletEvent {
  oldSize: Point;
  newSize: Point;
}

export interface LeafletLayerEvent extends LeafletEvent {
  layer: L.Layer;
}

export interface LeafletZoomAnimEvent extends LeafletEvent {
  center: LatLng;
  zoom: number;
  noUpdate: boolean;
}

export type LeafletEventHandlerFn = (event: LeafletEvent) => void;