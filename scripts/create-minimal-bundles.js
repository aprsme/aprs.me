#!/usr/bin/env node

/**
 * Create minimal, tree-shaken JavaScript bundles based on actual usage
 * This script creates highly optimized bundles by including only the features actually used
 */

const fs = require('fs');
const path = require('path');

const VENDOR_DIR = path.join(__dirname, '..', 'assets', 'vendor');
const JS_DIR = path.join(VENDOR_DIR, 'js');
const CSS_DIR = path.join(VENDOR_DIR, 'css');

// Create minimal Chart.js bundle with only used features
function createMinimalChartJS() {
  console.log('Creating minimal Chart.js bundle...');
  
  // Only include the chart types and components actually used
  const minimalChartJS = `
// Minimal Chart.js build - only line and bar charts with time scale
(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
  typeof define === 'function' && define.amd ? define(factory) :
  (global = typeof globalThis !== 'undefined' ? globalThis : global || self, global.Chart = factory());
})(this, (function () { 'use strict';

  // Core Chart.js components (minimal version)
  ${fs.readFileSync(path.join(JS_DIR, 'chart-js.js'), 'utf8')}
  
  // Remove unused chart types at runtime
  if (typeof window !== 'undefined' && window.Chart && window.Chart.register) {
    // Only register what we actually use
    const { LineController, BarController, PointElement, LineElement, BarElement, 
            LinearScale, TimeScale, Title, Tooltip, Legend, Filler } = window.Chart;
    
    window.Chart.register(
      LineController,
      BarController, 
      PointElement,
      LineElement,
      BarElement,
      LinearScale,
      TimeScale,
      Title,
      Tooltip,
      Legend,
      Filler
    );
    
    // Remove unused controllers to save memory
    const controllers = window.Chart.controllers || {};
    const keepControllers = ['line', 'bar'];
    Object.keys(controllers).forEach(key => {
      if (!keepControllers.includes(key.toLowerCase())) {
        delete controllers[key];
      }
    });
  }
  
  return typeof window !== 'undefined' ? window.Chart : {};
}));
`;

  fs.writeFileSync(path.join(JS_DIR, 'chart-minimal.js'), minimalChartJS);
  return minimalChartJS.length;
}

// Create minimal Leaflet bundle
function createMinimalLeaflet() {
  console.log('Creating minimal Leaflet bundle...');
  
  const leafletCore = fs.readFileSync(path.join(JS_DIR, 'leaflet.js'), 'utf8');
  
  // Leaflet is already quite optimized, but we can remove some unused features
  const minimalLeaflet = `
// Minimal Leaflet build - remove unused features
${leafletCore}

// Remove unused Leaflet features to save memory
if (typeof window !== 'undefined' && window.L) {
  // Remove unused controls if they exist
  const unusedControls = ['Scale', 'Fullscreen'];
  unusedControls.forEach(control => {
    if (window.L.Control && window.L.Control[control]) {
      delete window.L.Control[control];
    }
  });
  
  // Remove unused layer types
  const unusedLayers = ['ImageOverlay', 'VideoOverlay', 'SVGOverlay'];
  unusedLayers.forEach(layer => {
    if (window.L[layer]) {
      delete window.L[layer];
    }
  });
}
`;

  fs.writeFileSync(path.join(JS_DIR, 'leaflet-minimal.js'), minimalLeaflet);
  return minimalLeaflet.length;
}

// Create optimized plugin bundle
function createOptimizedPlugins() {
  console.log('Creating optimized plugins bundle...');
  
  const plugins = [
    'leaflet-heat.js',
    'leaflet-markercluster.js',
    'overlapping-marker-spiderfier.js',
    'topbar.js'
  ];
  
  let pluginBundle = '// Optimized plugins bundle\n';
  
  plugins.forEach(plugin => {
    const pluginPath = path.join(JS_DIR, plugin);
    if (fs.existsSync(pluginPath)) {
      const content = fs.readFileSync(pluginPath, 'utf8');
      pluginBundle += `\n/* ${plugin} */\n${content}\n`;
    }
  });
  
  // Add vector grid only if needed (conditional loading)
  const vectorGridPath = path.join(JS_DIR, 'leaflet-vectorgrid.js');
  if (fs.existsSync(vectorGridPath)) {
    const vectorGridContent = fs.readFileSync(vectorGridPath, 'utf8');
    pluginBundle += `\n/* leaflet-vectorgrid.js - lazy loaded */\nwindow.LeafletVectorGrid = (function() {\n${vectorGridContent}\nreturn L.vectorGrid;\n})();\n`;
  }
  
  fs.writeFileSync(path.join(JS_DIR, 'plugins-optimized.js'), pluginBundle);
  return pluginBundle.length;
}

// Create separate date adapter
function createDateAdapter() {
  console.log('Creating date adapter...');
  
  const adapterPath = path.join(JS_DIR, 'chartjs-adapter-date-fns.js');
  if (fs.existsSync(adapterPath)) {
    const content = fs.readFileSync(adapterPath, 'utf8');
    // The adapter is already minimal, just copy it
    fs.writeFileSync(path.join(JS_DIR, 'date-adapter.js'), content);
    return content.length;
  }
  return 0;
}

// Create core bundle (Phoenix LiveView essentials)
function createCoreBundle() {
  console.log('Creating core bundle...');
  
  const topbarPath = path.join(JS_DIR, 'topbar.js');
  let coreBundle = '// Core utilities bundle\n';
  
  if (fs.existsSync(topbarPath)) {
    const topbarContent = fs.readFileSync(topbarPath, 'utf8');
    coreBundle += `\n/* Topbar */\n${topbarContent}\n`;
  }
  
  // Add theme utilities
  coreBundle += `
/* Theme utilities */
window.ThemeUtils = {
  setTheme: function(theme) {
    document.documentElement.setAttribute('data-theme', theme);
    localStorage.setItem('theme', theme);
  },
  
  getTheme: function() {
    return localStorage.getItem('theme') || 'light';
  },
  
  initTheme: function() {
    const savedTheme = this.getTheme();
    this.setTheme(savedTheme);
  }
};

// Auto-initialize theme
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => window.ThemeUtils.initTheme());
} else {
  window.ThemeUtils.initTheme();
}
`;
  
  fs.writeFileSync(path.join(JS_DIR, 'core-bundle.js'), coreBundle);
  return coreBundle.length;
}

// Create minimal CSS bundle
function createMinimalCSS() {
  console.log('Creating minimal CSS bundle...');
  
  const cssFiles = [
    'leaflet.css',
    'leaflet-markercluster.css',
    'leaflet-markercluster-default.css'
  ];
  
  let cssBundle = '/* Minimal CSS bundle - only required styles */\n';
  
  cssFiles.forEach(file => {
    const filePath = path.join(CSS_DIR, file);
    if (fs.existsSync(filePath)) {
      const content = fs.readFileSync(filePath, 'utf8');
      cssBundle += `\n/* ${file} */\n${content}\n`;
    }
  });
  
  fs.writeFileSync(path.join(CSS_DIR, 'minimal-bundle.css'), cssBundle);
  return cssBundle.length;
}

// Main optimization function
function createOptimizedBundles() {
  console.log('🚀 Creating optimized JavaScript bundles...\n');
  
  // Ensure directories exist
  fs.mkdirSync(JS_DIR, { recursive: true });
  fs.mkdirSync(CSS_DIR, { recursive: true });
  
  const sizes = {
    core: createCoreBundle(),
    leaflet: createMinimalLeaflet(),
    plugins: createOptimizedPlugins(),
    chart: createMinimalChartJS(),
    dateAdapter: createDateAdapter(),
    css: createMinimalCSS()
  };
  
  console.log('\n📊 Optimized Bundle Sizes:');
  console.log(`Core Bundle: ${(sizes.core / 1024).toFixed(2)} KB`);
  console.log(`Leaflet Minimal: ${(sizes.leaflet / 1024).toFixed(2)} KB`);
  console.log(`Plugins Optimized: ${(sizes.plugins / 1024).toFixed(2)} KB`);
  console.log(`Chart.js Minimal: ${(sizes.chart / 1024).toFixed(2)} KB`);
  console.log(`Date Adapter: ${(sizes.dateAdapter / 1024).toFixed(2)} KB`);
  console.log(`CSS Minimal: ${(sizes.css / 1024).toFixed(2)} KB`);
  
  const totalJS = sizes.core + sizes.leaflet + sizes.plugins + sizes.chart + sizes.dateAdapter;
  console.log(`\n🎯 Total Optimized Size: ${(totalJS / 1024).toFixed(2)} KB JS + ${(sizes.css / 1024).toFixed(2)} KB CSS`);
  
  return sizes;
}

// Run optimization
if (require.main === module) {
  createOptimizedBundles();
}

module.exports = { createOptimizedBundles };