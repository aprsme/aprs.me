#!/bin/bash
# update-docker-image.sh - Script to update Docker image with latest dependencies

set -e  # Exit immediately if a command exits with a non-zero status

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== APRS.me Docker Image Update Tool ===${NC}"
echo "This script will update your Docker image with the latest dependencies"

# Check if Docker is installed and running
if ! command -v docker &> /dev/null; then
    echo -e "${RED}Error: Docker is not installed or not in PATH${NC}"
    exit 1
fi

# Check Docker daemon is running
if ! docker info &> /dev/null; then
    echo -e "${RED}Error: Docker daemon is not running${NC}"
    exit 1
fi

# Get the current versions from Dockerfile
ELIXIR_VERSION=$(grep 'ELIXIR_VERSION=' Dockerfile | head -1 | cut -d'=' -f2 | tr -d '"')
OTP_VERSION=$(grep 'OTP_VERSION=' Dockerfile | head -1 | cut -d'=' -f2 | tr -d '"')
DEBIAN_VERSION=$(grep 'DEBIAN_VERSION=' Dockerfile | head -1 | cut -d'=' -f2 | tr -d '"')

echo -e "${GREEN}Current versions:${NC}"
echo "  Elixir: $ELIXIR_VERSION"
echo "  OTP: $OTP_VERSION"
echo "  Debian: $DEBIAN_VERSION"

# Build the Docker image with a clean cache to ensure latest deps
echo -e "${YELLOW}Building Docker image with latest dependencies...${NC}"
echo "This may take a few minutes..."

# Use --pull to ensure we get the latest base image
# Use --no-cache to ensure all layers are rebuilt with latest packages
docker build --no-cache --pull -t aprs:latest .

# Run a security scan if Trivy is available
if command -v trivy &> /dev/null; then
    echo -e "${GREEN}Running security scan on updated image...${NC}"
    trivy image --severity HIGH,CRITICAL aprs:latest
else
    echo -e "${YELLOW}Trivy not installed. Security scan skipped.${NC}"
    echo "To install Trivy:"
    echo "  - macOS: brew install aquasecurity/trivy/trivy"
    echo "  - Linux: see https://aquasecurity.github.io/trivy/latest/getting-started/installation/"
fi

echo
echo -e "${GREEN}=== Docker Image Update Complete ===${NC}"
echo
echo "The Docker image has been rebuilt with the latest dependencies."
echo
echo "Next steps:"
echo "1. Test the image: docker run --rm -it aprs:latest"
echo "2. If tests pass, tag and push the image to your registry"
echo "3. Deploy the updated image to your environment"
echo
echo -e "${YELLOW}Remember to update CI workflows if you've changed base image versions${NC}"

# Check if git is available and we're in a git repository
if command -v git &> /dev/null && git rev-parse --is-inside-work-tree &> /dev/null 2>&1; then
    echo
    echo -e "${GREEN}Git status:${NC}"
    git status -s
fi
