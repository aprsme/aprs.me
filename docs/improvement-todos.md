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

## High Priority

### 2. Optimize Database Queries with Better Indexes
- **Status**: Pending
- **Impact**: High - Improve query performance
- **Details**:
  - Add composite indexes for common query patterns
  - Optimize spatial queries with better PostGIS indexes
  - Consider materialized views for complex aggregations
  - Analyze slow query logs to identify bottlenecks

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

### 4. Add Connection Draining for Graceful Shutdowns
- **Status**: Pending
- **Impact**: Medium - Better user experience during deployments
- **Details**:
  - Implement proper shutdown handlers for WebSocket connections
  - Allow in-flight requests to complete before pod termination
  - Add preStop hooks to Kubernetes deployment
  - Handle SIGTERM gracefully


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

## Current Architecture Summary

1. **Load Balancing**: Kubernetes service distributes HTTP traffic across pods
2. **Database Pooling**: PgBouncer provides connection multiplexing (7 clients → 3 server connections)
3. **Distributed State**: Redis handles PubSub, caching, and rate limiting across all replicas
4. **Leader Election**: Only one pod maintains APRS-IS connection, preventing duplicates
5. **High Availability**: Multiple replicas with automatic failover for all components

Last updated: 2025-07-26