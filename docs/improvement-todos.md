# APRS.me Improvement TODOs

This document tracks potential improvements identified during the multi-replica Kubernetes deployment setup.

## High Priority

### 1. Implement Distributed Caching with Redis
- **Status**: Pending
- **Impact**: High - Reduce database load, improve response times
- **Details**:
  - Cache frequently accessed packet queries
  - Cache callsign lookups
  - Cache weather data aggregations
  - Cache map viewport data
  - Use Cachex with Redis adapter or direct Redis commands
  - Implement cache invalidation strategies

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

### 5. Implement Distributed Rate Limiting Across Replicas
- **Status**: Pending
- **Impact**: Medium - Consistent rate limiting
- **Details**:
  - Currently using ETS-based rate limiting (not distributed)
  - Use Redis for distributed rate limit counters
  - Implement sliding window rate limiting
  - Share rate limit state across all pods
  - Consider using Hammer with Redis backend

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

- Redis infrastructure is already in place (used for PubSub)
- PgBouncer is configured and working for connection pooling
- Kubernetes cluster is configured with StatefulSet for stable networking
- Current setup handles ~8-21 packets/second with 2 replicas

Last updated: 2025-07-26