#!/bin/bash
# update-docker-image.sh - Script to update Docker image with latest dependencies
#
# This script helps ensure that your Docker image has the latest system dependencies
# by rebuilding the image with the latest base image and system packages.

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

# Get the latest version of the base image from Dockerfile
BASE_IMAGE=$(grep 'DEBIAN_VERSION=' Dockerfile | head -1 | cut -d'=' -f2 | tr -d '"')
if [ -z "$BASE_IMAGE" ]; then
    echo -e "${RED}Error: Could not determine base image from Dockerfile${NC}"
    exit 1
fi

echo -e "${GREEN}Current base image:${NC} $BASE_IMAGE"

# Check if we need to update the Dockerfile
echo -e "${YELLOW}Checking for newer Debian versions...${NC}"
latest_debian=$(curl -s https://hub.docker.com/v2/repositories/library/debian/tags/ | \
                grep -o '"name":"[^"]*-slim"' | grep bullseye | sort -r | head -1 | cut -d'"' -f4)

if [ -z "$latest_debian" ]; then
    echo -e "${YELLOW}Could not determine latest Debian version, keeping current version${NC}"
else
    echo -e "${GREEN}Latest Debian version:${NC} $latest_debian"

    # Update Dockerfile with latest Debian version
    current_version=$(grep 'DEBIAN_VERSION=' Dockerfile | head -1 | cut -d'=' -f2 | tr -d '"')
    if [ "$current_version" != "$latest_debian" ]; then
        echo -e "${YELLOW}Updating Dockerfile with latest Debian version...${NC}"
        sed -i "s/ARG DEBIAN_VERSION=.*/ARG DEBIAN_VERSION=$latest_debian/" Dockerfile
        echo -e "${GREEN}Updated Dockerfile with new base image:${NC} $latest_debian"
    else
        echo -e "${GREEN}Already using the latest Debian version${NC}"
    fi
fi

# Check for latest Elixir and OTP versions
echo -e "${YELLOW}Checking for newer Elixir and OTP versions...${NC}"
current_elixir=$(grep 'ELIXIR_VERSION=' Dockerfile | head -1 | cut -d'=' -f2 | tr -d '"')
current_otp=$(grep 'OTP_VERSION=' Dockerfile | head -1 | cut -d'=' -f2 | tr -d '"')

echo -e "${GREEN}Current Elixir version:${NC} $current_elixir"
echo -e "${GREEN}Current OTP version:${NC} $current_otp"

echo -e "${YELLOW}Building Docker image with latest dependencies...${NC}"
docker build --no-cache --pull -t aprs:latest .

echo -e "${GREEN}Running security scan on updated image...${NC}"
if command -v trivy &> /dev/null; then
    trivy image --severity HIGH,CRITICAL aprs:latest
else
    echo -e "${YELLOW}Trivy not installed. Skipping security scan.${NC}"
    echo "Install Trivy with: brew install aquasecurity/trivy/trivy (macOS) or similar for your OS"
fi

echo
echo -e "${GREEN}=== Update Complete ===${NC}"
echo "The Docker image has been rebuilt with the latest dependencies"
echo "Recommendations:"
echo "1. Run tests to ensure everything still works"
echo "2. Check for any security issues reported above"
echo "3. If all is well, commit and push your changes"
echo

# Check if git is available and we're in a git repository
if command -v git &> /dev/null && git rev-parse --is-inside-work-tree &> /dev/null; then
    echo "Git changes:"
    git diff Dockerfile
fi
