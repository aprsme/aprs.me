# APRS.me Improvement TODOs

This document tracks potential improvements identified during the multi-replica Kubernetes deployment setup.

## Completed Improvements

### ✅ Redis PubSub Integration (2025-07-26)
- **Impact**: High - Enables real-time updates across all replicas
- **Implementation**:
  - Added `phoenix_pubsub_redis` dependency
  - Configured conditional Redis PubSub adapter in application.ex
  - Fixed configuration to use correct URL format
  - All replicas now share real-time packet updates

### ✅ PgBouncer Connection Pooling (2025-07-26)
- **Impact**: High - Efficient database connection management
- **Implementation**:
  - Deployed PgBouncer with transaction pooling mode
  - Configured for 1000 max client connections, 25 pool size
  - Reduced APRS pool size from 20 to 5 per pod
  - Fixed runtime.exs to remove incompatible PostgreSQL parameters
  - Currently showing 7 client connections served by 3 server connections

### ✅ Distributed Caching with Redis (2025-07-26)
- **Status**: Completed
- **Impact**: High - Reduce database load, improve response times
- **Implementation**:
  - Created `Aprsme.RedisCache` module with Cachex-compatible API
  - Created `Aprsme.Cache` abstraction layer for seamless switching
  - Migrated query_cache, device_cache, and symbol_cache to Redis
  - Automatic fallback to ETS when Redis unavailable
  - All cache data now shared across pods

### ✅ Distributed Rate Limiting with Redis (2025-07-26)
- **Status**: Completed
- **Impact**: High - Consistent rate limiting across replicas
- **Implementation**:
  - Created `Aprsme.RedisRateLimiter` with sliding window algorithm
  - Created `Aprsme.RateLimiterWrapper` for API compatibility
  - Atomic Lua script ensures accurate counting
  - Rate limits now enforced cluster-wide
  - Prevents bypass by hitting different pods

### ✅ Connection Draining for Graceful Shutdowns (2025-07-26)
- **Status**: Completed - Updated for Zero-Downtime
- **Impact**: High - Zero-downtime deployments
- **Implementation**:
  - Created `Aprsme.ShutdownHandler` with configurable drain timeout
  - Created `Aprsme.SignalHandler` for proper SIGTERM handling
  - **Silent shutdowns**: Removed all user notifications during graceful shutdown
  - Updated health endpoint to return 503 when draining (15s delay)
  - Added preStop lifecycle hook with 15s sleep
  - Set terminationGracePeriodSeconds to 60 seconds
  - Configurable DRAIN_TIMEOUT_MS environment variable (default 45s)
  - Added PodDisruptionBudget to ensure minAvailable: 1
  - StatefulSet uses RollingUpdate with parallel pod management
  - Service configured with sessionAffinity: None for better distribution

## High Priority

### ✅ Optimize Database Queries with Better Indexes (2025-07-26)
- **Status**: Completed
- **Impact**: High - Improve query performance
- **Implementation**:
  - Added 10 new performance indexes including:
    - Functional index on upper(sender) for case-insensitive searches
    - Composite indexes for position/time queries
    - Spatial index using geography type for ST_DWithin queries
    - Partial indexes for weather data filtering
    - Generated column `has_weather` with trigger for optimization
    - Region and data_type composite indexes
  - Query performance improved by 50-90%:
    - Upper case sender search: ~10ms
    - Position queries: ~7ms
    - Spatial queries: ~40ms (from 100ms+)
    - Weather queries: ~2-3ms
    - Distinct callsign queries: ~4ms

### ✅ Fix PostgreSQL Notification Trigger for Info Page Updates (2025-07-28)
- **Status**: Completed
- **Impact**: High - Fix real-time updates on info pages
- **Implementation**:
  - Updated `notify_packets_insert` trigger to send all required fields
  - Previously only sent: id, sender, lat, lon, inserted_at
  - Now includes: altitude, course, speed, symbol data, device info, PHG data, etc.
  - Info page (/info/:callsign) now updates in real-time with complete packet data
  - Ensures all UI elements refresh properly when new packets arrive

### 3. Add Metrics and Monitoring with Prometheus
- **Status**: Pending
- **Impact**: High - Production visibility
- **Details**:
  - Add Prometheus metrics exporter (prometheus_ex)
  - Track packet processing rates and latencies
  - Monitor connection pool usage (PgBouncer & app)
  - Track cache hit rates
  - Add custom business metrics
  - Monitor APRS-IS connection stability

## Medium Priority


### 6. Add Comprehensive Health Checks
- **Status**: Pending
- **Impact**: Medium - Better Kubernetes integration
- **Details**:
  - Enhance beyond basic /health endpoint
  - Add database connectivity checks
  - Add Redis connectivity checks
  - Add APRS-IS connection status checks
  - Add resource usage checks (memory, connections)
  - Separate readiness vs liveness probes

### 7. Implement Horizontal Pod Autoscaling
- **Status**: Pending
- **Impact**: Medium - Auto-scaling based on load
- **Details**:
  - Configure HPA based on CPU/memory usage
  - Consider custom metrics (packet processing rate)
  - Ensure proper resource requests/limits
  - Test scaling behavior under load

## Low Priority

### 8. Enhance Circuit Breakers
- **Status**: Pending
- **Impact**: Low - Resilience improvement
- **Details**:
  - Already have Aprsme.CircuitBreaker module
  - Add circuit breakers for database connections
  - Implement fallback mechanisms
  - Add circuit breaker metrics
  - Consider using fuse library

## Additional Improvements Identified

### 9. Session Affinity for WebSockets
- Consider implementing sticky sessions for WebSocket connections
- Or implement WebSocket connection state migration
- May improve user experience during pod scaling

### 10. Background Job Optimization
- Oban jobs could use Redis for better distributed processing
- Implement job priorities and queues
- Add job monitoring and metrics
- Consider using Oban Pro features

### 11. Optimize JavaScript Bundle Size Further
- Analyze bundle with webpack-bundle-analyzer equivalent
- Consider lazy loading more components
- Implement code splitting for routes
- Remove any remaining unused dependencies

### 12. Database Connection Pool Tuning
- Monitor PgBouncer pool usage patterns
- Adjust pool sizes based on actual usage
- Consider separate pools for read/write operations
- Implement connection pool warmup

### 13. Implement Distributed Tracing
- Add OpenTelemetry support
- Trace requests across the system
- Identify performance bottlenecks
- Integrate with Jaeger or similar

### 14. Security Enhancements
- Implement CSRF protection for non-API routes
- Add rate limiting per IP/user
- Implement API key management for external access
- Add security headers (HSTS, CSP, etc.)

### 15. Performance Optimizations
- Implement ETL for historical data
- Add data archival strategies
- Optimize Phoenix Channels for large subscriber counts
- Consider read replicas for heavy read workloads

## Implementation Priority

Based on current system state with Redis and PgBouncer already deployed:

1. **Distributed Caching** - Immediate high impact, infrastructure ready
2. **Metrics/Monitoring** - Essential for production visibility
3. **Database Indexes** - Query performance improvements
4. **Enhanced Health Checks** - Better Kubernetes integration
5. **Connection Draining** - Improved deployment experience

## Notes

- Redis infrastructure is deployed and actively used for:
  - PubSub (Phoenix channels)
  - Distributed caching (query, device, symbol caches)
  - Distributed rate limiting
- PgBouncer is configured with transaction pooling, reducing connection overhead
- Kubernetes cluster uses StatefulSet for stable pod naming and networking
- Current setup handles ~8-21 packets/second with 2 replicas
- All distributed features automatically fallback to local implementations if Redis is unavailable
- Graceful shutdown process ensures zero-downtime deployments
- Users experience no interruption during rolling updates
- PodDisruptionBudget prevents all pods from being evicted simultaneously

## Current Architecture Summary

1. **Load Balancing**: Kubernetes service distributes HTTP traffic across pods
2. **Database Pooling**: PgBouncer provides connection multiplexing (7 clients → 3 server connections)
3. **Distributed State**: Redis handles PubSub, caching, and rate limiting across all replicas
4. **Leader Election**: Only one pod maintains APRS-IS connection, preventing duplicates
5. **High Availability**: Multiple replicas with automatic failover for all components

## Recent Updates (2025-07-26)

### Docker Optimizations
- Removed Node.js from Docker image (saved ~150MB)
- Optimized Dockerfile for faster builds with better layer caching
- Simplified from complex BuildKit features to basic 2-stage build
- Added comprehensive .dockerignore file
- Fixed permission issues with non-root user

### Bug Fixes
- Fixed PgBouncer database connection issues (added SKIP_DB_CREATE)
- Fixed Redis connection errors (removed invalid pool_size option)
- Fixed compilation warnings (grouped handle_info clauses)
- Fixed ShutdownHandler interfering with startup
- Fixed DeviceCache startup timing issues

### CI/CD Improvements
- Removed rollout wait from deployment workflow
- Added concurrency control to cancel in-progress deployments
- Simplified Docker build caching strategy

### Database Optimizations (2025-07-26)
- Created comprehensive migration with 10 new indexes
- Implemented `has_weather` generated column with trigger
- All queries now execute in under 40ms
- Spatial queries optimized with geography cast index
- Weather queries use indexed boolean column

Last updated: 2025-07-26 (Database Query Optimizations)