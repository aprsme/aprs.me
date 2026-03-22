# Weather Nearby API Endpoint Design

**Date:** 2026-03-22
**Status:** Approved

## Overview

API endpoint to find weather stations within a specified radius of a geographic point, returning stations with recent weather data.

## Specification

**Endpoint:** `GET /api/v1/weather/nearby`

**Purpose:** Find weather stations within a specified radius of a geographic point.

**Query Parameters:**
- `lat` (required) - Latitude (-90 to 90)
- `lon` (required) - Longitude (-180 to 180)
- `radius` (required) - Search radius in miles (> 0, max 1000)
- `hours` (optional) - Time window for "recent" weather data in hours (default: 6, max: 168 for 7 days)
- `limit` (optional) - Maximum number of results (default: 50, max: 100)

**Example Request:**
```
GET /api/v1/weather/nearby?lat=37.7749&lon=-122.4194&radius=25&hours=6&limit=50
```

## Implementation Details

### Database Query Strategy

Use PostGIS geography type with `ST_DWithin` for efficient spatial filtering:

```sql
-- Conceptual query structure
SELECT DISTINCT ON (base_callsign) *
FROM packets
WHERE has_weather = true
  AND received_at >= (NOW() - interval '6 hours')
  AND ST_DWithin(
    location::geography,
    ST_SetSRID(ST_MakePoint(lon, lat), 4326)::geography,
    radius_meters
  )
ORDER BY base_callsign, received_at DESC
LIMIT 50
```

Then order the results by distance using `ST_Distance` in a subquery.

**Performance Notes:**
- Leverages existing spatial index on `location` column
- `has_weather` boolean index for fast filtering
- `DISTINCT ON` prevents duplicate SSIDs
- Subquery pattern keeps distance calculation efficient
- Miles converted to meters internally (1 mile = 1609.34 meters)

### Code Structure

1. **Controller:** `lib/aprsme_web/controllers/api/v1/weather_controller.ex`
   - Parameter validation (lat/lon ranges, radius > 0, etc.)
   - Call context function
   - Render JSON response

2. **Context Function:** Add to `lib/aprsme/packets/prepared_queries.ex`
   - `get_nearby_weather_stations(lat, lon, radius_miles, opts)`
   - Uses prepared query pattern like existing `get_nearby_stations_knn`
   - Converts miles to meters for PostGIS

3. **JSON View:** `lib/aprsme_web/controllers/api/v1/weather_json.ex`
   - Format response structure
   - Handle null weather fields
   - Include metadata

4. **Router:** Add route to `/api/v1` scope in `router.ex`

## Response Format

### Success Response (200 OK)

```json
{
  "data": [
    {
      "callsign": "N0CALL-13",
      "base_callsign": "N0CALL",
      "position": {
        "lat": 37.7849,
        "lon": -122.4094
      },
      "distance_miles": 0.87,
      "weather": {
        "temperature": 72.5,
        "humidity": 65.0,
        "pressure": 1013.2,
        "wind_speed": 8.5,
        "wind_direction": 270,
        "wind_gust": 12.0,
        "rain_1h": 0.0,
        "rain_24h": 0.5,
        "rain_since_midnight": 0.3
      },
      "symbol": {
        "table_id": "/",
        "code": "_"
      },
      "comment": "Davis Vantage Pro2",
      "last_report": "2026-03-22T15:30:00Z"
    }
  ],
  "meta": {
    "count": 12,
    "params": {
      "lat": 37.7749,
      "lon": -122.4194,
      "radius_miles": 25,
      "hours": 6,
      "limit": 50
    }
  }
}
```

**Notes:**
- Weather fields are `null` if not reported by the station
- Distance calculated as straight-line (great circle) distance
- Times in ISO 8601 UTC format

### Error Responses

- `400 Bad Request` - Invalid/missing parameters
- `422 Unprocessable Entity` - Parameters out of valid range
- `500 Internal Server Error` - Database/server error

## Error Handling & Validation

### Parameter Validation

```elixir
# Latitude: -90 to 90
validate_lat(lat) when lat >= -90 and lat <= 90

# Longitude: -180 to 180
validate_lon(lon) when lon >= -180 and lon <= 180

# Radius: > 0, reasonable max (e.g., 1000 miles)
validate_radius(radius) when radius > 0 and radius <= 1000

# Hours: 1 to 168 (7 days max)
validate_hours(hours) when hours >= 1 and hours <= 168

# Limit: 1 to 100
validate_limit(limit) when limit >= 1 and limit <= 100
```

### Error Messages

- Missing required param: `"Missing required parameter: lat"`
- Invalid format: `"Invalid latitude: must be a number between -90 and 90"`
- Out of range: `"Radius must be between 0 and 1000 miles"`

### Fallback Controller

Uses existing `AprsmeWeb.Api.V1.FallbackController` to handle:
- Validation errors → 400/422 responses
- Database errors → 500 with generic message (details logged)
- Rate limiting handled by existing `RateLimiter` plug

### Edge Cases

- No weather stations found → Return empty array with 200 OK
- Antimeridian crossing (longitude wrap) → PostGIS geography handles automatically
- Invalid callsign characters → Sanitized by database layer
- Timeout on large queries → Database query timeout (existing)

## Testing Strategy

### Unit Tests (Controller)
- Valid parameters return 200 with correct structure
- Missing required params return 400
- Out-of-range params return 422
- Parameter type coercion (string to float/int)
- Default values applied correctly

### Integration Tests (Database)
- Find stations within radius (seed test data with known positions)
- Exclude stations outside radius
- Exclude stations without recent weather data
- Exclude stations without `has_weather == true`
- Respect time window (hours parameter)
- Respect result limit
- Order by distance (closest first)
- Handle antimeridian crossing
- DISTINCT ON base_callsign (no duplicate SSIDs)

### Test Data Setup
```elixir
# Center point: 37.7749, -122.4194 (San Francisco)
# Station A: 0.5 miles away, weather 1 hour ago ✓
# Station B: 10 miles away, weather 2 hours ago ✓
# Station C: 50 miles away, weather 1 hour ago ✗ (outside radius)
# Station D: 5 miles away, weather 8 hours ago ✗ (outside time window)
# Station E: 5 miles away, no weather data ✗ (has_weather == false)
```

### Performance Tests
- Query execution time < 100ms for typical queries
- Index usage verified with EXPLAIN ANALYZE
- Large radius (1000 miles, dense area) completes reasonably

### Test File Location
- `test/aprsme_web/controllers/api/v1/weather_controller_test.exs`

## Implementation Files

1. `lib/aprsme_web/controllers/api/v1/weather_controller.ex` (new)
2. `lib/aprsme_web/controllers/api/v1/weather_json.ex` (new)
3. `lib/aprsme/packets/prepared_queries.ex` (add function)
4. `lib/aprsme_web/router.ex` (add route)
5. `test/aprsme_web/controllers/api/v1/weather_controller_test.exs` (new)
