# Docker Security Strategy for APRS.me

This document outlines our approach to maintaining secure Docker images for the APRS.me application.

## System Dependencies Update Strategy

Keeping system dependencies updated is crucial for security. Our strategy includes:

1. **Regular Base Image Updates**
   - We use specific dated versions of Debian slim images (`bullseye-YYYYMMDD-slim`) 
   - Images are rebuilt weekly via CI to incorporate latest security patches
   - Each build uses `--pull` to ensure we get the latest base image versions

2. **Full Package Updates During Build**
   - Every build performs `apt-get update && apt-get upgrade` in both build and runtime stages
   - We explicitly remove package lists after updates to reduce image size
   - Only necessary runtime packages are installed in the final image

3. **Minimized Attack Surface**
   - Non-root user execution with specific UID/GID
   - Removal of setuid/setgid permissions
   - Secure permissions on system files
   - Use of tini as init process
   - Minimal set of installed packages

4. **Continuous Monitoring**
   - Weekly automated builds via GitHub Actions
   - Security scanning with Trivy and Docker Scout
   - Detailed vulnerability reports for OS and application dependencies
   - Automatic failure on critical vulnerabilities

## How System Updates Are Applied

System dependencies are updated at several points:

1. **During Image Build Time**
   ```dockerfile
   RUN apt-get update -y && \
       apt-get upgrade -y && \
       apt-get clean && \
       rm -f /var/lib/apt/lists/*_*
   ```

2. **Via Scheduled Rebuilds**
   - Weekly GitHub Actions workflow rebuilds the image with latest dependencies
   - Base image is pulled with `--pull` flag to ensure latest version
   - No-cache builds ensure all layers are rebuilt with fresh packages

3. **During Deployment**
   - Images are rebuilt for each deployment
   - CI/CD pipeline includes security scanning before deployment

## Security Scanning Process

Our Docker images undergo multiple security scans:

1. **Trivy Scanning**
   - OS package vulnerabilities detection
   - Application dependency vulnerabilities detection
   - Configuration issue detection

2. **Docker Scout**
   - Deep analysis of base image security
   - Comprehensive CVE detection
   - Dependency analysis

3. **Artifact Storage**
   - Scan results are stored as GitHub Actions artifacts
   - Summary reports are generated for easy review
   - Historical data allows tracking security improvements

## Manual Update Process

To manually update the Docker image with the latest system dependencies:

1. Run the update script:
   ```bash
   ./scripts/update-docker-image.sh
   ```

2. This script will:
   - Build a fresh image with latest dependencies using `--no-cache --pull`
   - Run a security scan if Trivy is available
   - Provide guidance on next steps

3. Verify the updated image works as expected before deployment

## Best Practices We Follow

- We pin specific versions of Elixir, OTP, and Debian in our Dockerfile
- Multi-stage builds minimize the final image size
- We use non-root users with minimal permissions
- We secure system files and directories
- We remove unnecessary files and packages
- We use tini as an init process for proper signal handling
- We scan for vulnerabilities in both OS and application dependencies
- We update base images regularly (at least monthly)

## Improvement Roadmap

- Consider distroless images for further attack surface reduction
- Implement auto-update PR creation for base image versions
- Add dependency confusion detection
- Implement more granular vulnerability allowlisting for false positives