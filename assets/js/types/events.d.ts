// Type definitions for event payloads used in APRS.me

import type { BoundsData, CenterData, MarkerData, HeatMapPoint } from './map';

// Base event payload
export interface BaseEventPayload {
  [key: string]: unknown;
}

// Map navigation events
export interface MapNavigationPayload extends BaseEventPayload {
  lat: number;
  lng: number;
  zoom?: number;
}

// Marker interaction events
export interface MarkerClickPayload extends BaseEventPayload {
  id: string;
  lat: number;
  lng: number;
}

export interface MarkerPopupPayload extends BaseEventPayload {
  id: string;
}

// Map state events
export interface MapStatePayload extends BaseEventPayload {
  lat: number;
  lng: number;
  zoom: number;
}

export interface MapBoundsPayload extends BaseEventPayload {
  bounds: BoundsData;
}

// Data update events
export interface UpdateMarkersPayload extends BaseEventPayload {
  markers: MarkerData[];
}

export interface UpdateHeatmapPayload extends BaseEventPayload {
  heat_points: HeatMapPoint[];
}

export interface RemoveMarkerPayload extends BaseEventPayload {
  id: string;
}

export interface ClearMarkersPayload extends BaseEventPayload {
  // No additional fields needed
}

// RF path events
export interface RFPathPayload extends BaseEventPayload {
  from_lat: number;
  from_lng: number;
  to_lat: number;
  to_lng: number;
  packet_count: number;
  last_heard: string;
}

export interface ClearRFPathsPayload extends BaseEventPayload {
  // No additional fields needed
}

// Trail events
export interface UpdateTrailPayload extends BaseEventPayload {
  callsign: string;
  positions: Array<{
    lat: number;
    lng: number;
    timestamp: string | number;
  }>;
}

export interface RemoveTrailPayload extends BaseEventPayload {
  callsign: string;
}

// Weather data events
export interface WeatherDataPayload extends BaseEventPayload {
  callsign: string;
  data: Array<{
    timestamp: string;
    temperature?: number;
    humidity?: number;
    pressure?: number;
    wind_speed?: number;
    wind_direction?: number;
    rain_1h?: number;
    rain_24h?: number;
    rain_midnight?: number;
  }>;
}

// Event handler callback types
export type EventCallback<T = BaseEventPayload> = (data: T) => void;

// Push event function type
export type PushEventFunction = <T extends BaseEventPayload = BaseEventPayload>(
  event: string, 
  payload: T
) => void;

// Handle event function type
export type HandleEventFunction = <T extends BaseEventPayload = BaseEventPayload>(
  event: string, 
  callback: EventCallback<T>
) => void;