#!/bin/bash
set -e

echo "Testing Docker build with Gleam support..."

# Build the Docker image
echo "Building Docker image..."
docker build -t aprsme-test:latest .

echo "Docker build completed successfully!"

# Test if the image runs
echo "Testing if the image can run..."
docker run --rm aprsme-test:latest bin/aprsme eval "IO.puts(:ok)"

echo "All tests passed!"