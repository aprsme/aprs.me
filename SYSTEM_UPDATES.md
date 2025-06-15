# System Dependency Updates for APRS.me Docker Image

This document outlines strategies for keeping system dependencies updated in the APRS.me Docker image without modifying the existing Dockerfile, which could potentially break the build process.

## Current Challenges

The Dockerfile builds successfully but maintaining up-to-date system dependencies is important for security. Standard approaches like adding `unattended-upgrades` or using `--security` flags with `apt-get` have proven problematic in our build environment.

## Recommended Approaches

### 1. Regular Rebuilds

The most reliable way to keep system dependencies updated is through regular rebuilds:

```bash
# Rebuild the Docker image with latest base image and dependencies
docker build --no-cache --pull -t aprs:latest .
```

Key flags:
- `--no-cache`: Forces all layers to be rebuilt, including running `apt-get update` and `apt-get upgrade`
- `--pull`: Ensures the latest version of the base image is used

### 2. Base Image Updates

Periodically update the base image version in the Dockerfile:

```diff
- ARG DEBIAN_VERSION=bullseye-20250520-slim
+ ARG DEBIAN_VERSION=bullseye-20250615-slim
```

The Debian team regularly releases updated images with security patches.

### 3. CI/CD Integration

Automate the update process:

1. Set up a weekly GitHub Actions workflow to:
   - Build the image with `--no-cache --pull`
   - Run security scans with Trivy
   - Create a PR if updates are needed

2. Include base image version checks:
   ```yaml
   - name: Check for newer base image
     run: |
       # Logic to check for newer base image versions
       # Create PR if newer version available
   ```

### 4. Security Scanning

Regularly scan for vulnerabilities:

```bash
# Install Trivy
brew install aquasecurity/trivy/trivy  # macOS
# or appropriate command for your OS

# Scan the image
trivy image aprs:latest
```

### 5. Manual Update Script

```bash
#!/bin/bash
# update-deps.sh

# Pull latest base image
docker pull debian:$(grep 'DEBIAN_VERSION=' Dockerfile | cut -d'=' -f2 | tr -d '"')

# Rebuild with latest dependencies
docker build --no-cache --pull -t aprs:latest .

# Scan for vulnerabilities
if command -v trivy &> /dev/null; then
  trivy image aprs:latest
fi

echo "Image rebuilt with latest dependencies"
```

## Deployment Strategy

1. Rebuild images at least weekly
2. Deploy updated images after testing
3. Monitor security advisories for critical updates
4. Perform out-of-band updates for critical CVEs

## Monitoring

1. Set up alerts for high/critical vulnerabilities
2. Subscribe to security mailing lists for Debian and key packages
3. Use image scanning in your container registry

By following these approaches, you can maintain up-to-date system dependencies without modifying the Dockerfile in ways that might break the build process.