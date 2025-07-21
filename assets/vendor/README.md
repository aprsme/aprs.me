# Vendor JavaScript Libraries

This directory contains third-party JavaScript and CSS libraries that are bundled locally instead of loading from CDNs.

## Required Files

The following files need to be downloaded:

### JavaScript Files
- `leaflet.js` - from https://unpkg.com/leaflet@1.9.4/dist/leaflet.js
- `leaflet-heat.js` - from https://cdn.jsdelivr.net/npm/leaflet.heat@0.2.0/dist/leaflet-heat.js
- `chart.umd.js` - from https://cdn.jsdelivr.net/npm/chart.js@4.5.0/dist/chart.umd.js
- `chartjs-adapter-date-fns.bundle.min.js` - from https://cdn.jsdelivr.net/npm/chartjs-adapter-date-fns@3.0.0/dist/chartjs-adapter-date-fns.bundle.min.js
- `leaflet.markercluster.js` - from https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js
- `oms.min.js` - from https://cdnjs.cloudflare.com/ajax/libs/OverlappingMarkerSpiderfier-Leaflet/0.2.6/oms.min.js

### CSS Files
- `leaflet.css` - from https://unpkg.com/leaflet@1.9.4/dist/leaflet.css
- `MarkerCluster.css` - from https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css
- `MarkerCluster.Default.css` - from https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css

## Download Script

To download all vendor files, run:

```bash
# From the project root
cd assets/vendor

# Download JavaScript files
curl -L -o leaflet.js https://unpkg.com/leaflet@1.9.4/dist/leaflet.js
curl -L -o leaflet-heat.js https://cdn.jsdelivr.net/npm/leaflet.heat@0.2.0/dist/leaflet-heat.js
curl -L -o chart.umd.js https://cdn.jsdelivr.net/npm/chart.js@4.5.0/dist/chart.umd.js
curl -L -o chartjs-adapter-date-fns.bundle.min.js https://cdn.jsdelivr.net/npm/chartjs-adapter-date-fns@3.0.0/dist/chartjs-adapter-date-fns.bundle.min.js
curl -L -o leaflet.markercluster.js https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js
curl -L -o oms.min.js https://cdnjs.cloudflare.com/ajax/libs/OverlappingMarkerSpiderfier-Leaflet/0.2.6/oms.min.js

# Download CSS files
curl -L -o leaflet.css https://unpkg.com/leaflet@1.9.4/dist/leaflet.css
curl -L -o MarkerCluster.css https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css
curl -L -o MarkerCluster.Default.css https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css
```

## Building

The vendor files are bundled using esbuild:

```bash
# Build vendor bundle
mix esbuild vendor

# Build with minification (for production)
mix esbuild vendor --minify
```

The vendor bundle is automatically included in the asset deployment pipeline.