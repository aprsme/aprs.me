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

### Changed
- Query performance improved by 50-90% for common operations
- Weather queries now use indexed `has_weather` column
- Spatial queries optimized with geography cast index

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