# Docker Security Strategy for APRS.me

This document outlines our approach to maintaining secure Docker images for the APRS.me application.

## System Dependencies Update Strategy

Keeping system dependencies updated is crucial for security. Our strategy includes:

1. **Automated Security Updates**
   - Base images are updated regularly with the latest security patches
   - `unattended-upgrades` is installed to automatically apply security updates
   - We use `apt-get upgrade --security` to prioritize security-related updates
   - Full system update with `apt-get dist-upgrade` to handle package dependencies

2. **Continuous Integration Checks**
   - Weekly automated builds via GitHub Actions
   - Security scanning on every build with Trivy
   - Detection of outdated dependencies with separate vulnerability reports
   - Automatic failure on critical vulnerabilities

3. **Multi-stage Build Optimization**
   - Builder stage includes only development dependencies
   - Runtime stage includes only production dependencies
   - Each stage performs full security updates

4. **Minimal Attack Surface**
   - Distroless or slim base images when possible
   - Unnecessary packages are removed
   - Non-root user execution
   - Removal of setuid/setgid permissions
   - Only required capabilities are enabled

## How Updates Are Applied

System dependencies are updated at multiple points:

1. **During Image Build**
   ```dockerfile
   RUN apt-get update -y && \
       apt-get upgrade -y --security && \
       apt-get dist-upgrade -y
   ```

2. **Automatically at Runtime**
   - The `unattended-upgrades` package applies security updates automatically
   - Configuration prioritizes official security updates

3. **Via Scheduled Rebuilds**
   - Weekly GitHub Actions workflow rebuilds the image with latest dependencies
   - Base image is pulled with `--pull` flag to ensure latest version

## Manual Update Process

To manually update the Docker image with the latest system dependencies:

1. Run the update script:
   ```bash
   ./scripts/update-docker-image.sh
   ```

2. This script will:
   - Check for newer versions of the base image
   - Update the Dockerfile if needed
   - Rebuild the image with `--no-cache --pull` to ensure fresh dependencies
   - Run a security scan on the updated image

3. Verify that tests pass with the updated image before deployment

## Security Monitoring

We continuously monitor for security issues through:

1. GitHub Security tab showing Trivy scan results
2. Weekly automated security scans
3. Dependency updates via Dependabot
4. Container registry vulnerability scanning

## Best Practices

- Never use the `latest` tag in production
- Pin specific versions in Dockerfile (ARG declarations)
- Update base images at least monthly
- Review and update the security strategy quarterly
- Use multi-stage builds to minimize final image size
- Implement least privilege principle (non-root user, minimal capabilities)
- Keep secrets out of the image (use environment variables or secrets management)