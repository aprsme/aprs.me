# Mobile API Documentation

## Overview

The APRS.me Mobile API provides real-time streaming of APRS packets filtered by geographic bounds for iOS and Android applications.

## Connection

**WebSocket URL:** `wss://aprs.me/mobile/websocket`

**Protocol:** Phoenix Channels over WebSocket

## Authentication

Currently, connections are anonymous. Future versions will support token-based authentication.

## Channel: `mobile:packets`

### Joining

```json
{
  "topic": "mobile:packets",
  "event": "phx_join",
  "payload": {},
  "ref": "1"
}
```

**Response:**
```json
{
  "topic": "mobile:packets",
  "event": "phx_reply",
  "payload": {
    "status": "ok",
    "response": {
      "message": "Connected to APRS mobile channel"
    }
  },
  "ref": "1"
}
```

### Subscribing to Geographic Bounds

Subscribe to receive packets within specific geographic bounds. Upon subscription, the server will immediately send historical packets within the bounds, followed by real-time streaming packets.

**Event:** `subscribe_bounds`

**Payload:**
```json
{
  "north": 33.2,
  "south": 33.0,
  "east": -96.0,
  "west": -96.2,
  "limit": 1000,
  "hours_back": 1
}
```

**Payload Fields:**

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `north` | float | Yes | - | Northern boundary latitude |
| `south` | float | Yes | - | Southern boundary latitude |
| `east` | float | Yes | - | Eastern boundary longitude |
| `west` | float | Yes | - | Western boundary longitude |
| `limit` | integer | No | 1000 | Maximum historical packets to load (max: 5000) |
| `hours_back` | integer | No | 1 | How many hours of historical data to load (max: 24) |

**Example:**
```json
{
  "topic": "mobile:packets",
  "event": "subscribe_bounds",
  "payload": {
    "north": 33.2,
    "south": 33.0,
    "east": -96.0,
    "west": -96.2,
    "limit": 2000,
    "hours_back": 2
  },
  "ref": "2"
}
```

**Response:**
```json
{
  "topic": "mobile:packets",
  "event": "phx_reply",
  "payload": {
    "status": "ok",
    "response": {
      "bounds": {
        "north": 33.2,
        "south": 33.0,
        "east": -96.0,
        "west": -96.2
      },
      "message": "Subscribed to packet stream"
    }
  },
  "ref": "2"
}
```

### Updating Bounds

Update the geographic bounds (e.g., when user pans/zooms the map).

**Event:** `update_bounds`

**Payload:**
```json
{
  "north": 33.3,
  "south": 32.9,
  "east": -95.9,
  "west": -96.3
}
```

**Response:**
```json
{
  "topic": "mobile:packets",
  "event": "phx_reply",
  "payload": {
    "status": "ok",
    "response": {
      "bounds": {
        "north": 33.3,
        "south": 32.9,
        "east": -95.9,
        "west": -96.3
      },
      "message": "Bounds updated"
    }
  },
  "ref": "3"
}
```

### Receiving Packets

Once subscribed, you'll receive packets within your bounds. Historical packets are sent immediately after subscription, followed by real-time streaming packets as they arrive.

**Event:** `packet`

**Payload:**
```json
{
  "id": "d7249877-d4a6-45c2-b314-2a8a355d2566",
  "callsign": "K5GVL-10",
  "lat": 33.1225,
  "lng": -96.124,
  "timestamp": "2025-10-25T16:17:20Z",
  "symbol_table_id": "/",
  "symbol_code": "#",
  "comment": "6/SVARA U=13.9V,T=75.3F",
  "altitude": 150.5,
  "speed": 45.2,
  "course": 180,
  "path": "TCPIP*,qAS,K5GVL"
}
```

**Packet Fields:**

| Field | Type | Description | Required |
|-------|------|-------------|----------|
| `id` | string | Unique packet identifier | Yes |
| `callsign` | string | Station callsign | Yes |
| `lat` | float | Latitude (-90 to 90) | Yes |
| `lng` | float | Longitude (-180 to 180) | Yes |
| `timestamp` | string | ISO 8601 timestamp | Yes |
| `symbol_table_id` | string | APRS symbol table (/, \\) | Yes |
| `symbol_code` | string | APRS symbol code | Yes |
| `comment` | string | Station comment/status | Optional |
| `altitude` | float | Altitude in meters | Optional |
| `speed` | float | Speed in knots | Optional |
| `course` | integer | Course in degrees (0-359) | Optional |
| `path` | string | APRS digipeater path | Optional |

### Unsubscribing

Stop receiving packets.

**Event:** `unsubscribe`

**Payload:** `{}`

**Response:**
```json
{
  "topic": "mobile:packets",
  "event": "phx_reply",
  "payload": {
    "status": "ok",
    "response": {
      "message": "Unsubscribed from packet stream"
    }
  },
  "ref": "4"
}
```

## Error Responses

**Invalid Bounds:**
```json
{
  "topic": "mobile:packets",
  "event": "phx_reply",
  "payload": {
    "status": "error",
    "response": {
      "message": "North must be greater than south"
    }
  },
  "ref": "2"
}
```

**Not Subscribed:**
```json
{
  "topic": "mobile:packets",
  "event": "phx_reply",
  "payload": {
    "status": "error",
    "response": {
      "message": "Not subscribed. Call subscribe_bounds first."
    }
  },
  "ref": "3"
}
```

## Swift Example

```swift
import SwiftPhoenixClient

class APRSService {
    let socket: Socket
    var channel: Channel?

    init() {
        socket = Socket("wss://aprs.me/mobile/websocket")
        socket.connect()
    }

    func joinChannel() {
        channel = socket.channel("mobile:packets")

        channel?.on("packet") { message in
            if let packet = message.payload as? [String: Any] {
                self.handlePacket(packet)
            }
        }

        channel?.join()
            .receive("ok") { _ in
                print("Joined mobile:packets channel")
                self.subscribeToBounds()
            }
            .receive("error") { error in
                print("Failed to join: \\(error)")
            }
    }

    func subscribeToBounds(north: Double, south: Double, east: Double, west: Double) {
        let bounds = [
            "north": north,
            "south": south,
            "east": east,
            "west": west
        ]

        channel?.push("subscribe_bounds", payload: bounds)
            .receive("ok") { response in
                print("Subscribed to bounds: \\(response)")
            }
            .receive("error") { error in
                print("Subscribe error: \\(error)")
            }
    }

    func updateBounds(north: Double, south: Double, east: Double, west: Double) {
        let bounds = [
            "north": north,
            "south": south,
            "east": east,
            "west": west
        ]

        channel?.push("update_bounds", payload: bounds)
    }

    func handlePacket(_ packet: [String: Any]) {
        guard let callsign = packet["callsign"] as? String,
              let lat = packet["lat"] as? Double,
              let lng = packet["lng"] as? Double else {
            return
        }

        // Update your map with the new packet
        print("Received packet from \\(callsign) at \\(lat), \\(lng)")
    }
}
```

## SwiftUI MapKit Integration

```swift
import SwiftUI
import MapKit

struct APRSMapView: View {
    @StateObject private var aprsService = APRSService()
    @State private var region = MKCoordinateRegion(
        center: CLLocationCoordinate2D(latitude: 33.1, longitude: -96.1),
        span: MKCoordinateSpan(latitudeDelta: 0.2, longitudeDelta: 0.2)
    )

    var body: some View {
        Map(coordinateRegion: $region)
            .onAppear {
                aprsService.joinChannel()
            }
            .onChange(of: region) { newRegion in
                let center = newRegion.center
                let span = newRegion.span

                aprsService.updateBounds(
                    north: center.latitude + span.latitudeDelta / 2,
                    south: center.latitude - span.latitudeDelta / 2,
                    east: center.longitude + span.longitudeDelta / 2,
                    west: center.longitude - span.longitudeDelta / 2
                )
            }
    }
}
```

## Rate Limiting

The API is rate-limited to prevent abuse:
- 200 requests per minute per IP address
- Connections are automatically closed if idle for >60 seconds

## Best Practices

1. **Update bounds only when map movement stops** - Use a debounce to avoid excessive updates
2. **Unsubscribe when app backgrounds** - Save battery and bandwidth
3. **Handle reconnection** - Phoenix Channels will automatically reconnect on connection loss
4. **Validate coordinates** - Always check lat/lng before adding to map
5. **Limit visible area** - Don't subscribe to bounds larger than what the user can see

## Dependencies

**Swift:**
- [SwiftPhoenixClient](https://github.com/davidstump/SwiftPhoenixClient) - Phoenix Channels client

**Installation (Swift Package Manager):**
```swift
dependencies: [
    .package(url: "https://github.com/davidstump/SwiftPhoenixClient.git", from: "5.3.0")
]
```

## Support

For issues or questions:
- GitHub: https://github.com/aprsme/aprs.me/issues
- Documentation: https://docs.aprs.me
