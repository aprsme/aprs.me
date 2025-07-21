#!/bin/bash

# Script to download vendor JavaScript and CSS files
# Run this from the project root: ./assets/download-vendor-files.sh

set -e

echo "Downloading vendor files..."

# Ensure vendor directory exists
mkdir -p assets/vendor

cd assets/vendor

# Download JavaScript files
echo "Downloading Leaflet..."
curl -L -o leaflet.js https://unpkg.com/leaflet@1.9.4/dist/leaflet.js

echo "Downloading Leaflet Heat..."
curl -L -o leaflet-heat.js https://cdn.jsdelivr.net/npm/leaflet.heat@0.2.0/dist/leaflet-heat.js

echo "Downloading Chart.js..."
curl -L -o chart.umd.js https://cdn.jsdelivr.net/npm/chart.js@4.5.0/dist/chart.umd.js

echo "Downloading Chart.js date adapter..."
curl -L -o chartjs-adapter-date-fns.bundle.min.js https://cdn.jsdelivr.net/npm/chartjs-adapter-date-fns@3.0.0/dist/chartjs-adapter-date-fns.bundle.min.js

echo "Downloading Leaflet MarkerCluster..."
curl -L -o leaflet.markercluster.js https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js

echo "Downloading OverlappingMarkerSpiderfier..."
curl -L -o oms.min.js https://cdnjs.cloudflare.com/ajax/libs/OverlappingMarkerSpiderfier-Leaflet/0.2.6/oms.min.js

# Download CSS files
echo "Downloading Leaflet CSS..."
curl -L -o leaflet.css https://unpkg.com/leaflet@1.9.4/dist/leaflet.css

echo "Downloading MarkerCluster CSS..."
curl -L -o MarkerCluster.css https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css
curl -L -o MarkerCluster.Default.css https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css

echo "All vendor files downloaded successfully!"
echo "Run 'mix esbuild vendor' to build the vendor bundle."