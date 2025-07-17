// Type definitions for extended marker properties used in APRS.me

import type { Marker } from 'leaflet';

// Extended marker interface with APRS-specific properties
export interface APRSMarker extends Marker {
  _isHistorical?: boolean;
  _markerId?: string;
}