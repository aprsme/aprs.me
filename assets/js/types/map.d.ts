// Type definitions for APRS Map application

import type { Map as LeafletMap, Marker, LatLng, LatLngBounds, DivIcon, LayerGroup, Popup, Polyline } from 'leaflet';
import type { HeatLayer, MarkerClusterGroup, OverlappingMarkerSpiderfier } from './leaflet-plugins';
import type { PushEventFunction, HandleEventFunction } from './events';
import type { TrailManager } from '../features/trail_manager';

export interface LiveViewHookContext {
  el: HTMLElement & { _leaflet_id?: number };
  pushEvent: PushEventFunction;
  handleEvent: HandleEventFunction;
  map?: LeafletMap;
  markers?: Map<string, Marker>;
  markerStates?: Map<string, MarkerState>;
  markerLayer?: LayerGroup;
  heatLayer?: HeatLayer;
  trailManager?: TrailManager;
  boundsTimer?: ReturnType<typeof setTimeout>;
  resizeHandler?: () => void;
  errors?: string[];
  initializationAttempts?: number;
  maxInitializationAttempts?: number;
  lastZoom?: number;
  currentPopupMarkerId?: string | null;
  oms?: OverlappingMarkerSpiderfier;
  programmaticMoveId?: string;
  programmaticMoveTimeout?: ReturnType<typeof setTimeout>;
  cleanupInterval?: ReturnType<typeof setInterval>;
  mapEventHandlers?: Map<string, Function>;
  isDestroyed?: boolean;
  popupNavigationHandler?: (e: Event) => void;
  moveEndHandler?: () => void;
  zoomEndHandler?: () => void;
  rfPathLines?: Array<Polyline | Marker>; // Array of Leaflet polylines and markers for RF path visualization
  trailLayer?: LayerGroup;
  markerClusterGroup?: MarkerClusterGroup;
  cleanupTimeouts?: ReturnType<typeof setTimeout>[];
  pendingMarkers?: MarkerData[];
  initializationTimeout?: ReturnType<typeof setTimeout>;
}

export interface MarkerData {
  id: string;
  lat: number;
  lng: number;
  callsign?: string;
  comment?: string;
  symbol_table_id?: string;
  symbol_code?: string;
  symbol_description?: string;
  popup?: string;
  historical?: boolean;
  color?: string;
  timestamp?: number | string;
  is_most_recent_for_callsign?: boolean;
  callsign_group?: string;
  symbol_html?: string;
  openPopup?: boolean;
  path?: string;
}

export interface BoundsData {
  north: number;
  south: number;
  east: number;
  west: number;
}

export interface CenterData {
  lat: number;
  lng: number;
}

export interface MarkerState {
  lat: number;
  lng: number;
  symbol_table: string;
  symbol_code: string;
  popup?: string;
  historical?: boolean;
  is_most_recent_for_callsign?: boolean;
  callsign_group?: string;
  callsign?: string;
}

export interface HeatMapPoint {
  lat: number;
  lng: number;
  intensity: number;
}

export interface MapEventData {
  bounds?: BoundsData;
  center?: CenterData;
  zoom?: number;
  id?: string;
  callsign?: string;
  lat?: number;
  lng?: number;
  markers?: MarkerData[];
  heat_points?: HeatMapPoint[];
}

export interface MapState {
  lat: number;
  lng: number;
  zoom: number;
}

export interface LiveSocket {
  connected: boolean;
  pushHistoryPatch: (href: string, state: string, target: HTMLElement) => void;
}

declare global {
  interface Window {
    liveSocket?: LiveSocket;
  }
}