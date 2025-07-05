// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
// import "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "../vendor/some-package.js"
//
// Alternatively, you can `npm install some-package --prefix assets` and import
// them using a path starting with the package name:
//
//     import "some-package"
//

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html";
// Establish Phoenix Socket and LiveView configuration.
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar"
import "../vendor/oms.min.js";

let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");

// Import minimal APRS map hook
import MapAPRSMap from "./map";

// Responsive Slideover Hook
let ResponsiveSlideoverHook = {
  mounted() {
    this.isInitialized = false;

    this.handleResize = () => {
      const isDesktop = window.innerWidth >= 1024;
      const isMobile = window.innerWidth < 1024;

      // Set initial state based on screen size
      if (!this.isInitialized) {
        this.isInitialized = true;
        if (isDesktop) {
          this.pushEvent("set_slideover_state", { open: true });
        } else {
          this.pushEvent("set_slideover_state", { open: false });
        }
      }
    };

    // Initial check after a brief delay to ensure LiveView is ready
    setTimeout(() => {
      this.handleResize();
    }, 100);

    // Listen for resize events with debouncing
    let resizeTimer;
    this.debouncedResize = () => {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(this.handleResize, 150);
    };

    window.addEventListener("resize", this.debouncedResize);
  },

  destroyed() {
    if (this.debouncedResize) {
      window.removeEventListener("resize", this.debouncedResize);
    }
  },
};

// APRS Map Hook
let Hooks = {};
Hooks.APRSMap = MapAPRSMap;
Hooks.ResponsiveSlideoverHook = ResponsiveSlideoverHook;

// Register weather chart hooks from TypeScript
import { WeatherChartHooks } from "./features/weather_charts";
Object.assign(Hooks, WeatherChartHooks);

// Helper function to get theme-aware colors
const getThemeColors = () => {
  const isDark = document.documentElement.getAttribute('data-theme') === 'dark';
  return {
    text: isDark ? '#e5e7eb' : '#111827',
    grid: isDark ? '#374151' : '#9ca3af',
    background: isDark ? 'rgba(0, 0, 0, 0.1)' : 'rgba(255, 255, 255, 0.1)'
  };
};

// Theme switching functionality
const theme = (() => {
  if (typeof localStorage !== 'undefined' && localStorage.getItem('theme')) {
    return localStorage.getItem('theme');
  }
  return 'auto';
})();

const applyTheme = (theme) => {
  const element = document.documentElement;
  
  if (theme === 'auto') {
    if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
      element.setAttribute('data-theme', 'dark');
    } else {
      element.setAttribute('data-theme', 'light');
    }
  } else {
    element.setAttribute('data-theme', theme);
  }
};

// Apply initial theme
applyTheme(theme);
window.localStorage.setItem('theme', theme);

// Global function to re-render all charts
window.reRenderAllCharts = () => {
  console.log('Re-rendering all charts...');
  
  // Store all chart instances globally so we can access them
  if (!window.chartInstances) {
    window.chartInstances = new Map();
  }
  
  // Re-render all stored chart instances
  window.chartInstances.forEach((chartInstance, elementId) => {
    console.log('Re-rendering chart:', elementId);
    if (chartInstance && typeof chartInstance.renderChart === 'function') {
      chartInstance.renderChart();
    }
  });
  
  // Also dispatch a custom event that charts can listen to
  console.log('Dispatching themeChanged event');
  window.dispatchEvent(new CustomEvent('themeChanged'));
};

const handleThemeClick = (selectedTheme) => {
  console.log('Theme changed to:', selectedTheme);
  applyTheme(selectedTheme);
  localStorage.setItem('theme', selectedTheme);
  
  // Re-render all charts with new theme colors
  setTimeout(() => {
    console.log('Calling reRenderAllCharts after theme change');
    window.reRenderAllCharts();
  }, 100);
};

// Listen for system theme changes when auto is selected
window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
  if (localStorage.getItem('theme') === 'auto') {
    applyTheme('auto');
    
    // Re-render all charts with new theme colors
    setTimeout(() => {
      window.reRenderAllCharts();
    }, 100);
  }
});

// Add event listeners for theme switching
document.addEventListener('DOMContentLoaded', () => {
  const themeButtons = document.querySelectorAll('[data-set-theme]');
  themeButtons.forEach(button => {
    button.addEventListener('click', () => {
      const theme = button.getAttribute('data-set-theme');
      handleThemeClick(theme);
    });
  });
});

let liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
});

// Show progress bar on live navigation and form submits
topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})
window.addEventListener("phx:page-loading-start", _info => topbar.show(100))
window.addEventListener("phx:page-loading-stop", _info => topbar.hide())


// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;

window.Hooks = Hooks;
export default Hooks;
