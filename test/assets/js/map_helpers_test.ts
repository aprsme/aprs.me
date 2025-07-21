import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';
import { 
  parseTimestamp, 
  getTrailId, 
  saveMapState, 
  safePushEvent,
  isLiveViewConnected,
  getLiveSocket
} from '../../../assets/js/map_helpers';

describe('map_helpers', () => {
  describe('parseTimestamp', () => {
    test('returns current time for null/undefined', () => {
      const before = Date.now();
      const result = parseTimestamp(null);
      const after = Date.now();
      expect(result).toBeGreaterThanOrEqual(before);
      expect(result).toBeLessThanOrEqual(after);
    });

    test('returns number timestamp as-is', () => {
      const timestamp = 1234567890;
      expect(parseTimestamp(timestamp)).toBe(timestamp);
    });

    test('parses string timestamp', () => {
      const dateStr = '2024-01-01T00:00:00Z';
      const expected = new Date(dateStr).getTime();
      expect(parseTimestamp(dateStr)).toBe(expected);
    });

    test('returns current time for invalid input', () => {
      const before = Date.now();
      const result = parseTimestamp({});
      const after = Date.now();
      expect(result).toBeGreaterThanOrEqual(before);
      expect(result).toBeLessThanOrEqual(after);
    });
  });

  describe('getTrailId', () => {
    test('prioritizes callsign_group', () => {
      const data = {
        callsign_group: 'GROUP-1',
        callsign: 'CALL-1',
        id: 'ID-1'
      };
      expect(getTrailId(data)).toBe('GROUP-1');
    });

    test('falls back to callsign when no callsign_group', () => {
      const data = {
        callsign: 'CALL-1',
        id: 'ID-1'
      };
      expect(getTrailId(data)).toBe('CALL-1');
    });

    test('falls back to id when no callsign_group or callsign', () => {
      const data = {
        id: 'ID-1'
      };
      expect(getTrailId(data)).toBe('ID-1');
    });

    test('extracts base callsign from historical ID format', () => {
      const data = {
        id: 'hist_MYCALL_123'
      };
      expect(getTrailId(data)).toBe('MYCALL');
    });

    test('handles historical ID with complex callsign', () => {
      const data = {
        id: 'hist_KD8ABC-9_456'
      };
      expect(getTrailId(data)).toBe('KD8ABC-9');
    });

    test('prioritizes callsign_group over historical ID extraction', () => {
      const data = {
        callsign_group: 'GROUP-1',
        id: 'hist_MYCALL_123'
      };
      expect(getTrailId(data)).toBe('GROUP-1');
    });

    test('prioritizes callsign over historical ID extraction', () => {
      const data = {
        callsign: 'REALCALL',
        id: 'hist_MYCALL_123'
      };
      expect(getTrailId(data)).toBe('REALCALL');
    });
  });

  describe('saveMapState', () => {
    let mockMap: any;
    let mockPushEvent: any;

    beforeEach(() => {
      // Mock localStorage
      const localStorageMock = {
        setItem: vi.fn()
      };
      Object.defineProperty(window, 'localStorage', {
        value: localStorageMock,
        writable: true
      });

      // Mock map object
      mockMap = {
        getCenter: vi.fn().mockReturnValue({ lat: 40.7128, lng: -74.0060 }),
        getZoom: vi.fn().mockReturnValue(10),
        getBounds: vi.fn().mockReturnValue({
          getNorth: vi.fn().mockReturnValue(41.0),
          getSouth: vi.fn().mockReturnValue(40.0),
          getEast: vi.fn().mockReturnValue(-73.0),
          getWest: vi.fn().mockReturnValue(-75.0)
        })
      };

      mockPushEvent = vi.fn();
    });

    test('saves truncated coordinates to localStorage', () => {
      saveMapState(mockMap, mockPushEvent);
      
      expect(localStorage.setItem).toHaveBeenCalledWith(
        'aprs_map_state',
        JSON.stringify({ lat: 40.7128, lng: -74.006, zoom: 10 })
      );
    });

    test('pushes event with map state and bounds', () => {
      saveMapState(mockMap, mockPushEvent);
      
      expect(mockPushEvent).toHaveBeenCalledWith('update_map_state', {
        center: { lat: 40.7128, lng: -74.006 },
        zoom: 10,
        bounds: {
          north: 41.0,
          south: 40.0,
          east: -73.0,
          west: -75.0
        }
      });
    });

    test('truncates coordinates to 5 decimal places', () => {
      mockMap.getCenter.mockReturnValue({ lat: 40.71281234567, lng: -74.00601234567 });
      
      saveMapState(mockMap, mockPushEvent);
      
      const call = mockPushEvent.mock.calls[0][1];
      expect(call.center.lat).toBe(40.71281);
      expect(call.center.lng).toBe(-74.00601);
    });
  });

  describe('safePushEvent', () => {
    test('calls pushEvent and returns true on success', () => {
      const mockPushEvent = vi.fn();
      const result = safePushEvent(mockPushEvent, 'test_event', { data: 'test' });
      
      expect(mockPushEvent).toHaveBeenCalledWith('test_event', { data: 'test' });
      expect(result).toBe(true);
    });

    test('returns false when pushEvent is undefined', () => {
      const result = safePushEvent(undefined, 'test_event', { data: 'test' });
      expect(result).toBe(false);
    });

    test('catches error and returns false', () => {
      const mockPushEvent = vi.fn().mockImplementation(() => {
        throw new Error('LiveView not connected');
      });
      
      const consoleSpy = vi.spyOn(console, 'debug').mockImplementation(() => {});
      const result = safePushEvent(mockPushEvent, 'test_event', { data: 'test' });
      
      expect(result).toBe(false);
      expect(consoleSpy).toHaveBeenCalledWith('Unable to send test_event event - LiveView disconnected');
      
      consoleSpy.mockRestore();
    });
  });

  describe('LiveView socket helpers', () => {
    afterEach(() => {
      // Clean up window.liveSocket
      delete (window as any).liveSocket;
    });

    test('isLiveViewConnected returns true when socket exists', () => {
      (window as any).liveSocket = { connected: true };
      expect(isLiveViewConnected()).toBe(true);
    });

    test('isLiveViewConnected returns false when socket missing', () => {
      expect(isLiveViewConnected()).toBe(false);
    });

    test('getLiveSocket returns the socket', () => {
      const mockSocket = { connected: true, pushHistoryPatch: vi.fn() };
      (window as any).liveSocket = mockSocket;
      
      expect(getLiveSocket()).toBe(mockSocket);
    });

    test('getLiveSocket returns undefined when missing', () => {
      expect(getLiveSocket()).toBeUndefined();
    });
  });
});