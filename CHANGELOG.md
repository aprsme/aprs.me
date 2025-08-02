# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Database query optimization with 10 new performance indexes
- Functional index on upper(sender) for case-insensitive searches
- Composite indexes for position/time queries
- Spatial index using geography type for ST_DWithin queries
- Partial indexes for weather data queries
- Generated column `has_weather` with trigger for query optimization
- Region and data_type composite indexes for filtering
- BRIN indexes consideration for time-series data
- Connection draining and load balancing for clustered deployments
  - Monitors CPU usage and connection counts across cluster nodes
- Documentation for tracked callsign behavior confirming last packet is always shown regardless of age
  - Automatically drains connections when node is overloaded (>70% CPU or 2x average connections)
  - Gracefully reconnects clients to less loaded nodes
  - Improves overall cluster stability and performance
- Position ambiguity support from APRS parser
  - Added `position_ambiguity` field to packets table (integer, 0-4)
  - Automatically captures position ambiguity level when parsing APRS packets
  - Supports ambiguity levels 0-4 as defined in APRS specification
- Enhanced APRS parser integration with latest improvements
  - Added `posresolution` field showing position accuracy in meters (18.52m for uncompressed, 0.291m for compressed)
  - Added 20 new database fields for enhanced parser compatibility
  - Implemented weather data extraction with dedicated `wx` field support
  - Enhanced PHG parsing to handle both string format and legacy map structure
  - Added radio range field extraction from comments
  - Added standard parser compatibility fields for better integration
  - Added `format` field indicating "compressed" or "uncompressed" position type
  - Added telemetry fields (`telemetry_seq`, `telemetry_vals`, `telemetry_bits`) for telemetry packet support
  - Improved coordinate handling with direct float support from parser
  - Fixed compressed latitude divisor calculation for more accurate position decoding

### Changed
- Query performance improved by 50-90% for common operations
- Weather queries now use indexed `has_weather` column
- Spatial queries optimized with geography cast index

### Fixed
- Tracked callsigns now show all packets including those without position data
  - Previously filtered out packets with `has_position == false` 
  - Now shows all packets when tracking a specific callsign via /:callsign URL
  - Ensures users can see status updates, messages, and other non-position packets
- PostgreSQL notify trigger now sends all required fields for info page updates
- Info page real-time updates now display all packet details correctly
- Enhanced PostgreSQL notify to include complete packet data (device info, weather data, SSIDs, etc.)
- Mic-E packet comment parsing now properly extracts altitude data and removes telemetry suffixes
  - Altitude data (e.g., "6;}" = 218 feet) is now parsed and stored
  - Telemetry markers (_%...) are removed from comments
  - Non-human-readable encoded data is no longer displayed as comments
- Historical packets now load on initial page load for all users
  - Previously only loaded historical data when tracking a specific callsign
  - Fixed by setting `needs_initial_historical_load` to true for all map views
  - Ensures users see recent activity immediately when visiting the map

## [0.2.0] - 2025-07-26

### Added
- Redis-based distributed PubSub for real-time updates across replicas
- PgBouncer connection pooling for efficient database connections
- Distributed caching with Redis (query, device, symbol caches)
- Distributed rate limiting with Redis using sliding window algorithm
- Graceful shutdown handling with connection draining
- PodDisruptionBudget for zero-downtime deployments
- Signal handler for proper SIGTERM handling
- Comprehensive health checks for Kubernetes probes
- Docker build optimizations with better layer caching
- Concurrency control in CI/CD pipeline

### Changed
- Moved from ETS to Redis for caching when REDIS_URL is available
- Updated Kubernetes deployment to use StatefulSet with stable networking
- Simplified Dockerfile by removing Node.js dependencies
- Improved CI/CD to not wait for rollout completion
- Enhanced shutdown process to be silent (no user notifications)
- Optimized Docker image size (removed ~150MB)

### Fixed
- PgBouncer database connection issues with SKIP_DB_CREATE
- Redis connection errors by removing invalid pool_size option
- Compilation warnings by grouping handle_info clauses
- ShutdownHandler interfering with application startup
- DeviceCache startup timing issues with Redis
- Docker permission errors with non-root user

### Removed
- Node.js and npm from Docker image (using standalone ESBuild/Tailwind)
- Complex BuildKit features that were slowing builds
- Multi-platform Docker builds (simplified to amd64 only)

## [0.1.0] - 2023-12-17

### Added
- Initial release
- APRS-IS network connection with reconnection logic
- Real-time packet processing with GenStage pipeline
- Phoenix LiveView map interface
- PostGIS geographic data storage
- Background job processing with Oban
- ETS-based local caching
- Basic rate limiting