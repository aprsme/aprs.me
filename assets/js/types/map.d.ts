// Type definitions for APRS Map application

import type { Map as LeafletMap, Marker, LatLng, LatLngBounds, DivIcon, LayerGroup, Popup } from 'leaflet';

export interface LiveViewHookContext {
  el: HTMLElement & { _leaflet_id?: any };
  pushEvent: (event: string, payload: any) => void;
  handleEvent: (event: string, callback: (data: any) => void) => void;
  map?: LeafletMap;
  markers?: Map<string, Marker>;
  markerStates?: Map<string, MarkerState>;
  markerLayer?: LayerGroup;
  trailManager?: any; // Import from trail_manager.ts when typed
  boundsTimer?: ReturnType<typeof setTimeout>;
  resizeHandler?: () => void;
  errors?: string[];
  initializationAttempts?: number;
  maxInitializationAttempts?: number;
  lastZoom?: number;
  currentPopupMarkerId?: string | null;
  oms?: any; // OverlappingMarkerSpiderfier type
  programmaticMoveId?: string;
  programmaticMoveTimeout?: ReturnType<typeof setTimeout>;
  cleanupInterval?: ReturnType<typeof setInterval>;
  mapEventHandlers?: Map<string, Function>;
  isDestroyed?: boolean;
  popupNavigationHandler?: (e: Event) => void;
  moveEndHandler?: () => void;
  zoomEndHandler?: () => void;
  [key: string]: any;
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

export interface MapEventData {
  bounds?: BoundsData;
  center?: CenterData;
  zoom?: number;
  id?: string;
  callsign?: string;
  lat?: number;
  lng?: number;
  markers?: MarkerData[];
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