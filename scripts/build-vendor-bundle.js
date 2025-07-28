#!/usr/bin/env node

/**
 * Script to download, minify, and bundle JavaScript libraries locally
 * This reduces external requests and allows us to remove unused code
 */

const fs = require('fs');
const path = require('path');
const https = require('https');

// URLs of the libraries we need
const libraries = [
  {
    name: 'leaflet',
    url: 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.js',
    minUrl: 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.js'
  },
  {
    name: 'leaflet-heat',
    url: 'https://unpkg.com/leaflet.heat@0.2.0/dist/leaflet-heat.js',
    minUrl: 'https://unpkg.com/leaflet.heat@0.2.0/dist/leaflet-heat.js'
  },
  {
    name: 'leaflet-markercluster',
    url: 'https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js',
    minUrl: 'https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js'
  },
  {
    name: 'overlapping-marker-spiderfier',
    url: 'https://unpkg.com/overlapping-marker-spiderfier-leaflet@0.2.7/dist/oms.js',
    minUrl: 'https://unpkg.com/overlapping-marker-spiderfier-leaflet@0.2.7/dist/oms.js'
  },
  {
    name: 'chart-js',
    url: 'https://unpkg.com/chart.js@4.5.0/dist/chart.umd.js',
    minUrl: 'https://unpkg.com/chart.js@4.5.0/dist/chart.umd.js'
  },
  {
    name: 'chartjs-adapter-date-fns',
    url: 'https://unpkg.com/chartjs-adapter-date-fns@3.0.0/dist/chartjs-adapter-date-fns.bundle.min.js',
    minUrl: 'https://unpkg.com/chartjs-adapter-date-fns@3.0.0/dist/chartjs-adapter-date-fns.bundle.min.js'
  },
  {
    name: 'topbar',
    url: 'https://unpkg.com/topbar@3.0.0/topbar.min.js',
    minUrl: 'https://unpkg.com/topbar@3.0.0/topbar.min.js'
  }
];

// CSS files we need
const cssFiles = [
  {
    name: 'leaflet',
    url: 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.css'
  },
  {
    name: 'leaflet-markercluster',
    url: 'https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css'
  },
  {
    name: 'leaflet-markercluster-default',
    url: 'https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css'
  }
];

// Download function
function downloadFile(url, filename) {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(filename);
    https.get(url, (response) => {
      if (response.statusCode !== 200) {
        reject(new Error(`Failed to download ${url}: ${response.statusCode}`));
        return;
      }
      
      response.pipe(file);
      file.on('finish', () => {
        file.close();
        console.log(`Downloaded: ${filename}`);
        resolve();
      });
    }).on('error', (err) => {
      fs.unlink(filename, () => {}); // Delete the file on error
      reject(err);
    });
  });
}

async function buildVendorBundle() {
  // Create directories if they don't exist
  const jsDir = path.join(__dirname, '..', 'assets', 'vendor', 'js');
  const cssDir = path.join(__dirname, '..', 'assets', 'vendor', 'css');
  
  fs.mkdirSync(jsDir, { recursive: true });
  fs.mkdirSync(cssDir, { recursive: true });

  // Download JavaScript libraries
  console.log('Downloading JavaScript libraries...');
  for (const lib of libraries) {
    const filename = path.join(jsDir, `${lib.name}.js`);
    try {
      await downloadFile(lib.minUrl, filename);
    } catch (error) {
      console.error(`Failed to download ${lib.name}:`, error.message);
    }
  }

  // Download CSS files
  console.log('Downloading CSS files...');
  for (const css of cssFiles) {
    const filename = path.join(cssDir, `${css.name}.css`);
    try {
      await downloadFile(css.url, filename);
    } catch (error) {
      console.error(`Failed to download ${css.name}:`, error.message);
    }
  }

  // Create a combined vendor bundle
  console.log('Creating combined vendor bundle...');
  let combinedJS = '';
  let combinedCSS = '';

  // Combine JavaScript files
  for (const lib of libraries) {
    const filename = path.join(jsDir, `${lib.name}.js`);
    if (fs.existsSync(filename)) {
      const content = fs.readFileSync(filename, 'utf8');
      combinedJS += `\n/* ${lib.name} */\n${content}\n`;
    }
  }

  // Combine CSS files
  for (const css of cssFiles) {
    const filename = path.join(cssDir, `${css.name}.css`);
    if (fs.existsSync(filename)) {
      const content = fs.readFileSync(filename, 'utf8');
      combinedCSS += `\n/* ${css.name} */\n${content}\n`;
    }
  }

  // Write combined files
  fs.writeFileSync(path.join(jsDir, 'vendor-bundle.js'), combinedJS);
  fs.writeFileSync(path.join(cssDir, 'vendor-bundle.css'), combinedCSS);

  console.log('✅ Vendor bundle created successfully!');
  console.log(`JavaScript bundle: ${(combinedJS.length / 1024).toFixed(2)} KB`);
  console.log(`CSS bundle: ${(combinedCSS.length / 1024).toFixed(2)} KB`);
}

// Run the build
buildVendorBundle().catch(console.error);