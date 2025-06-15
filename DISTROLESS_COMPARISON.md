# Distroless Docker Options for APRS.me

This document compares different distroless approaches for containerizing the APRS.me Elixir/Phoenix application.

## Current Setup vs Distroless Options

### Current Setup (`Dockerfile`)
- **Base Image**: `debian:bullseye-slim` (~80MB)
- **Security**: Good (non-root user, hardened)
- **Attack Surface**: Moderate (full Debian with package manager)
- **Debugging**: Easy (shell access, package manager)
- **Compatibility**: Excellent

### Option 1: Simple Distroless (`Dockerfile.distroless-simple`)
- **Base Image**: `gcr.io/distroless/cc-debian11:nonroot` (~20MB)
- **Security**: Excellent (no shell, no package manager)
- **Attack Surface**: Minimal
- **Debugging**: Difficult (no shell access)
- **Compatibility**: Good for most Elixir applications
- **Recommended**: ✅ **BEST STARTING POINT**

### Option 2: Custom Library Distroless (`Dockerfile.distroless`)
- **Base Image**: `gcr.io/distroless/base-debian11:nonroot` (~15MB)
- **Security**: Excellent
- **Attack Surface**: Minimal
- **Debugging**: Very difficult
- **Compatibility**: May require fine-tuning
- **Complexity**: High (manual library management)

### Option 3: Static Distroless (`Dockerfile.distroless-static`)
- **Base Image**: `gcr.io/distroless/static-debian11:nonroot` (~2MB)
- **Security**: Maximum
- **Attack Surface**: Absolute minimum
- **Debugging**: Nearly impossible
- **Compatibility**: **May not work** with Erlang/OTP
- **Recommended**: ❌ **NOT RECOMMENDED** for Elixir

## Recommended Migration Path

### Phase 1: Test Simple Distroless
1. Use `Dockerfile.distroless-simple`
2. Test all application functionality
3. Monitor for any runtime issues
4. Verify external dependencies work (database, APIs)

### Phase 2: Optimize (if needed)
1. If Phase 1 works perfectly, you're done!
2. If you need smaller images, consider custom library approach
3. Profile which libraries are actually needed

## Key Benefits of Distroless

### Security Improvements
- **No shell access**: Eliminates shell-based attacks
- **No package manager**: Cannot install malicious packages
- **Minimal attack surface**: Only your application + runtime
- **Reduced CVEs**: Fewer components = fewer vulnerabilities
- **Immutable**: Cannot be modified at runtime

### Operational Benefits
- **Smaller images**: Faster pulls, less storage
- **Faster startup**: Less to initialize
- **Better compliance**: Meets strict security requirements
- **Supply chain security**: Google-maintained base images

## Potential Challenges

### Debugging Difficulties
```bash
# This won't work in distroless:
docker exec -it container /bin/bash

# Use this instead:
docker run --rm -it --entrypoint="" your-image:tag /app/bin/server remote
```

### Limited Troubleshooting
- No shell for investigating issues
- Must rely on application logs
- Consider adding detailed logging/metrics

### Dependency Issues
- Some NIFs might need additional libraries
- SSL/TLS certificates need careful handling
- Time zones might not be available

## Testing Checklist

Before switching to distroless, verify:

- [ ] Application starts successfully
- [ ] Database connections work
- [ ] Phoenix LiveView functions
- [ ] Asset serving works
- [ ] SSL/HTTPS connections work
- [ ] External API calls succeed
- [ ] WebSocket connections work
- [ ] Health checks pass
- [ ] Graceful shutdown works
- [ ] Logging outputs correctly

## Build Commands

```bash
# Test simple distroless version
docker build -f Dockerfile.distroless-simple -t aprs:distroless-simple .

# Compare image sizes
docker images | grep aprs

# Test functionality
docker run -p 4000:4000 aprs:distroless-simple
```

## Rollback Plan

If distroless causes issues:
1. Keep current `Dockerfile` as `Dockerfile.debian`
2. Switch CI/CD back to Debian version
3. Investigate specific compatibility issues
4. Gradually migrate features

## Conclusion

**Recommendation**: Start with `Dockerfile.distroless-simple`

This provides:
- Significant security improvements
- Manageable complexity
- Good compatibility with Elixir/Phoenix
- Easy rollback if needed

The `gcr.io/distroless/cc-debian11:nonroot` image includes the C runtime libraries that Erlang/OTP requires, making it the most compatible distroless option for Elixir applications.