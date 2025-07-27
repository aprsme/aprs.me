# Database Migration Strategy for Kubernetes

This document describes how database migrations are handled in the Kubernetes deployment to ensure only one node runs migrations at a time.

## Overview

In a distributed environment with multiple pods, we need to ensure that database migrations only run once, not multiple times concurrently. We use two strategies:

1. **PostgreSQL Advisory Locks** - For runtime migration coordination
2. **Init Containers** - For deployment-time migrations (recommended)

## Implementation

### 1. PostgreSQL Advisory Locks

The `Aprsme.MigrationLock` module uses PostgreSQL advisory locks to ensure only one node can run migrations at a time:

- Uses `pg_try_advisory_lock()` to acquire a non-blocking lock
- Other nodes wait for the lock to be released
- Automatically releases the lock after migrations complete

### 2. Init Container (Recommended)

The StatefulSet can be configured with an init container that runs migrations before the main pods start:

```bash
# Apply the init container patch
kubectl patch statefulset aprs -n aprs --patch-file k8s/statefulset-init-container-patch.yaml
```

This approach:
- Runs migrations sequentially before any pods start
- Prevents race conditions
- Makes migration failures visible in pod events

### 3. Auto-Migration Disabled in Cluster Mode

When `CLUSTER_ENABLED=true`, automatic migrations on startup are disabled to prevent race conditions.

## Manual Migration

To run migrations manually:

```bash
# Run on the first pod
kubectl exec -it aprs-0 -n aprs -- /app/bin/migrate

# Or create a one-off job
kubectl run migrate-job --rm -it --image=ghcr.io/aprsme/aprs.me:latest \
  --env="DATABASE_URL=$DATABASE_URL" \
  --env="SECRET_KEY_BASE=$SECRET_KEY_BASE" \
  --env="MIX_ENV=prod" \
  --env="SKIP_DB_CREATE=true" \
  --restart=Never \
  -- /app/bin/migrate
```

## Configuration

Environment variables:
- `CLUSTER_ENABLED=true` - Enables distributed locking
- `SKIP_DB_CREATE=true` - Skips database creation (for PgBouncer)

## Best Practices

1. **Use Init Containers** for production deployments
2. **Test migrations** in a staging environment first
3. **Monitor migration logs** during deployments
4. **Have rollback plan** ready with `mix ecto.rollback`