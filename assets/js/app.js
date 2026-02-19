console.log("app.js loading...");

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

// APRS MapAPRSMap Hook
let Hooks = {};

// Singleton map bundle loader — ensures the script is only appended once
let mapBundleCallbacks = [];
let mapBundleLoading = false;

function loadMapBundle(callback) {
  if (window.mapBundleLoaded) {
    callback();
    return;
  }

  mapBundleCallbacks.push(callback);

  if (mapBundleLoading) {
    return; // Already loading, callback will fire when ready
  }

  if (window.VendorLoader) {
    mapBundleLoading = true;
    const script = document.createElement('script');
    script.src = window.VendorLoader.mapBundleUrl;
    script.onload = () => {
      window.mapBundleLoaded = true;
      mapBundleLoading = false;
      const cbs = mapBundleCallbacks;
      mapBundleCallbacks = [];
      cbs.forEach(cb => cb());
    };
    script.onerror = () => {
      console.error("Failed to load map bundle");
      mapBundleLoading = false;
      mapBundleCallbacks = [];
    };
    document.head.appendChild(script);
  } else {
    callback();
  }
}

// Map hooks - load map bundle when needed
const originalMapMounted = MapAPRSMap.mounted;
Hooks.APRSMap = {
  ...MapAPRSMap,
  mounted() {
    console.log("APRSMap wrapper mounted() called");
    const self = this;
    loadMapBundle(() => {
      console.log("Map bundle ready, calling original mounted");
      if (originalMapMounted) {
        originalMapMounted.call(self);
      }
    });
  }
};

Hooks.InfoMap = {
  ...InfoMap,
  mounted() {
    const self = this;
    loadMapBundle(() => {
      if (InfoMap.mounted) {
        InfoMap.mounted.call(self);
      }
    });
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
Hooks.ErrorBoundary = ErrorBoundary;
Hooks.TimeAgoHook = TimeAgoHook;

// Theme management
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

// Apply initial theme from localStorage
applyTheme(localStorage.getItem("theme") || "auto");

// Handle theme changes dispatched from LiveView via JS.dispatch
window.addEventListener("phx:set-theme", (e) => {
  const theme = e.detail.theme;
  applyTheme(theme);
  localStorage.setItem("theme", theme);
  window.dispatchEvent(new CustomEvent("themeChanged"));
});

// Listen for system theme changes when auto is selected
window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => {
  if (localStorage.getItem("theme") === "auto") {
    applyTheme("auto");
    window.dispatchEvent(new CustomEvent("themeChanged"));
  }
});

console.log("Creating LiveSocket with hooks:", Object.keys(Hooks));
let liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken, viewport_width: window.innerWidth },
  hooks: Hooks,
  timeout: 60000, // 60 second timeout for slow initial loads
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
