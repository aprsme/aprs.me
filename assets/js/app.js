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
// topbar is loaded globally from vendor bundle
const topbar = window.topbar;

// Sentry initialization happens via the loader script in the HTML
// Configure additional Sentry settings if needed
if (typeof window.Sentry !== "undefined" && window.Sentry.onLoad) {
  window.Sentry.onLoad(function () {
    window.Sentry.init({
      environment: "production",
      integrations: [new window.Sentry.BrowserTracing()],
      tracesSampleRate: 1.0, // Capture 100% of transactions for performance monitoring
      sampleRate: 1.0, // Capture 100% of errors
      beforeSend(event, hint) {
        // Filter out known non-critical errors
        if (hint.originalException?.message?.includes("ResizeObserver loop limit exceeded")) {
          return null;
        }
        return event;
      },
    });
  });
}

let csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content") || "";
if (!csrfToken) {
  console.error("CSRF token not found in meta tags");
}

// Import minimal APRS map hook
import MapAPRSMap from "./map";
// Import error boundary hook
import ErrorBoundary from "./hooks/error_boundary";
// Import info map hook
import { InfoMap } from "./hooks/info_map";
// Import time ago hook
import TimeAgoHook from "./hooks/time_ago_hook";

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

// Body Class Hook - Updates body class based on map_page assign
let BodyClassHook = {
  mounted() {
    this.updateBodyClass();
  },

  updated() {
    this.updateBodyClass();
  },

  updateBodyClass() {
    // Get the map_page value from the element's data attribute
    const mapPage = this.el?.dataset?.mapPage === "true";

    // Update body class based on map_page value
    if (document.body && document.body.classList) {
      if (mapPage) {
        document.body.classList.add("map-page");
      } else {
        document.body.classList.remove("map-page");
      }
    }
  },
};

// APRS Map Hook
let Hooks = {};

// Map hooks - load map bundle when needed
Hooks.APRSMap = {
  ...MapAPRSMap,
  mounted() {
    const self = this;
    if (window.VendorLoader && !window.mapBundleLoaded) {
      // Load map bundle and wait for it to complete
      const script = document.createElement('script');
      script.src = window.VendorLoader.mapBundleUrl;
      script.onload = () => {
        window.mapBundleLoaded = true;
        // Now call the original mounted function
        if (MapAPRSMap.mounted) {
          MapAPRSMap.mounted.call(self);
        }
      };
      script.onerror = () => {
        console.error("Failed to load map bundle");
      };
      document.head.appendChild(script);
    } else {
      // Map bundle already loaded, proceed immediately
      if (MapAPRSMap.mounted) {
        MapAPRSMap.mounted.call(this);
      }
    }
  }
};

Hooks.InfoMap = {
  ...InfoMap,
  mounted() {
    const self = this;
    if (window.VendorLoader && !window.mapBundleLoaded) {
      // Load map bundle and wait for it to complete
      const script = document.createElement('script');
      script.src = window.VendorLoader.mapBundleUrl;
      script.onload = () => {
        window.mapBundleLoaded = true;
        // Now call the original mounted function
        if (InfoMap.mounted) {
          InfoMap.mounted.call(self);
        }
      };
      script.onerror = () => {
        console.error("Failed to load map bundle");
      };
      document.head.appendChild(script);
    } else {
      // Map bundle already loaded, proceed immediately
      if (InfoMap.mounted) {
        InfoMap.mounted.call(this);
      }
    }
  }
};

// Chart hooks - load chart bundle when needed
import { WeatherChartHooks } from "./features/weather_charts";
Object.keys(WeatherChartHooks).forEach(hookName => {
  const originalHook = WeatherChartHooks[hookName];
  Hooks[hookName] = {
    ...originalHook,
    mounted() {
      const self = this;
      if (window.VendorLoader && !window.chartBundleLoaded) {
        // Load chart bundle and wait for it to complete
        window.VendorLoader.loadCharts();
        // Wait a bit for charts to load, then call mounted
        const checkChartLoaded = () => {
          if (window.Chart) {
            if (originalHook.mounted) {
              originalHook.mounted.call(self);
            }
          } else {
            setTimeout(checkChartLoaded, 50);
          }
        };
        setTimeout(checkChartLoaded, 100);
      } else {
        // Chart bundle already loaded or loading, call mounted
        if (originalHook.mounted) {
          originalHook.mounted.call(this);
        }
      }
    }
  };
});

// Core hooks - no bundle loading needed
Hooks.ResponsiveSlideoverHook = ResponsiveSlideoverHook;
Hooks.BodyClassHook = BodyClassHook;
Hooks.ErrorBoundary = ErrorBoundary;
Hooks.TimeAgoHook = TimeAgoHook;

// Helper function to get theme-aware colors
const getThemeColors = () => {
  const isDark = document.documentElement.getAttribute("data-theme") === "dark";
  return {
    text: isDark ? "#e5e7eb" : "#111827",
    grid: isDark ? "#374151" : "#9ca3af",
    background: isDark ? "rgba(0, 0, 0, 0.1)" : "rgba(255, 255, 255, 0.1)",
  };
};

// Theme switching functionality
const theme = (() => {
  if (typeof localStorage !== "undefined" && localStorage.getItem("theme")) {
    return localStorage.getItem("theme");
  }
  return "auto";
})();

const applyTheme = (theme) => {
  const element = document.documentElement;
  if (!element) return;

  if (theme === "auto") {
    if (window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches) {
      element.setAttribute("data-theme", "dark");
    } else {
      element.setAttribute("data-theme", "light");
    }
  } else {
    element.setAttribute("data-theme", theme);
  }
};

// Apply initial theme
applyTheme(theme);
window.localStorage.setItem("theme", theme);

// Global function to re-render all charts
window.reRenderAllCharts = () => {
  // Store all chart instances globally so we can access them
  if (!window.chartInstances) {
    window.chartInstances = new Map();
  }

  // Re-render all stored chart instances
  window.chartInstances.forEach((chartInstance, elementId) => {
    if (chartInstance && typeof chartInstance.renderChart === "function") {
      chartInstance.renderChart();
    }
  });

  // Also dispatch a custom event that charts can listen to
  window.dispatchEvent(new CustomEvent("themeChanged"));
};

const handleThemeClick = (selectedTheme) => {
  applyTheme(selectedTheme);
  localStorage.setItem("theme", selectedTheme);

  // Re-render all charts with new theme colors
  setTimeout(() => {
    window.reRenderAllCharts();
  }, 100);
};

// Listen for system theme changes when auto is selected
window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", (e) => {
  if (localStorage.getItem("theme") === "auto") {
    applyTheme("auto");

    // Re-render all charts with new theme colors
    setTimeout(() => {
      window.reRenderAllCharts();
    }, 100);
  }
});

// Add event listeners for theme switching
document.addEventListener("DOMContentLoaded", () => {
  const themeButtons = document.querySelectorAll("[data-set-theme]");
  themeButtons.forEach((button) => {
    button.addEventListener("click", () => {
      const theme = button.getAttribute("data-set-theme");
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
topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (_info) => topbar.show(100));
window.addEventListener("phx:page-loading-stop", (_info) => topbar.hide());

// Handle connection draining reconnect events
window.addEventListener("phx:reconnect", (e) => {
  const delay = e.detail.delay || 1000;
  console.log(`[LiveSocket] Reconnecting in ${delay}ms due to connection draining...`);
  setTimeout(() => {
    // Disconnect and reconnect to potentially land on a different server
    liveSocket.disconnect();
    setTimeout(() => {
      liveSocket.connect();
    }, 100);
  }, delay);
});

// connect if there are any LiveViews on the page
liveSocket.connect();

// Workaround for Phoenix LiveView bug where fallback timer isn't cleared
// after successful WebSocket connection
window.addEventListener("phx:live_socket:connect", (info) => {
  const socket = liveSocket.socket;
  if (socket && socket.fallbackTimer) {
    clearTimeout(socket.fallbackTimer);
    socket.fallbackTimer = null;
    console.log("[LiveSocket] Cleared fallback timer after successful connection");
  }
});

// Also check periodically in case the event doesn't fire
setTimeout(() => {
  const socket = liveSocket.socket;
  if (socket && socket.isConnected() && socket.fallbackTimer) {
    clearTimeout(socket.fallbackTimer);
    socket.fallbackTimer = null;
    console.log("[LiveSocket] Cleared lingering fallback timer");
  }
}, 5000);

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;

window.Hooks = Hooks;
export default Hooks;
