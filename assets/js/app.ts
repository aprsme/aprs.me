// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html";
// Establish Phoenix Socket and LiveView configuration.
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";

declare global {
  interface Window {
    topbar: {
      config: (opts: { barColors: Record<number, string>; shadowColor: string }) => void;
      show: (delay?: number) => void;
      hide: () => void;
    };
    mapBundleLoaded?: boolean;
    chartBundleLoaded?: boolean;
    Chart?: unknown;
    VendorLoader?: {
      mapBundleUrl: string;
      loadCharts: () => void;
    };
    liveSocket: any;
    Hooks: Record<string, any>;
  }
}

// topbar is loaded globally from vendor bundle
const topbar = window.topbar;

const csrfToken =
  document
    .querySelector("meta[name='csrf-token']")
    ?.getAttribute("content") || "";
if (!csrfToken) {
  console.error("CSRF token not found in meta tags");
}

// Import hooks
import MapAPRSMap from "./map";
import ErrorBoundary from "./hooks/error_boundary";
import { InfoMap } from "./hooks/info_map";
import TimeAgoHook from "./hooks/time_ago_hook";

interface HookDef {
  mounted?: () => void;
  updated?: () => void;
  destroyed?: () => void;
  [key: string]: unknown;
}

const Hooks: Record<string, HookDef> = {};

// Singleton map bundle loader — ensures the script is only appended once
let mapBundleCallbacks: Array<() => void> = [];
let mapBundleLoading = false;

function loadMapBundle(callback: () => void) {
  if (window.mapBundleLoaded) {
    callback();
    return;
  }

  mapBundleCallbacks.push(callback);

  if (mapBundleLoading) {
    return;
  }

  if (window.VendorLoader) {
    mapBundleLoading = true;
    const script = document.createElement("script");
    script.src = window.VendorLoader.mapBundleUrl;
    script.onload = () => {
      window.mapBundleLoaded = true;
      mapBundleLoading = false;
      const cbs = mapBundleCallbacks;
      mapBundleCallbacks = [];
      cbs.forEach((cb) => cb());
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
    const self = this;
    loadMapBundle(() => {
      if (originalMapMounted) {
        originalMapMounted.call(self);
      }
    });
  },
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
  },
};

// Chart hooks - load chart bundle when needed
import { WeatherChartHooks } from "./features/weather_charts";
Object.keys(WeatherChartHooks).forEach((hookName) => {
  const originalHook = (WeatherChartHooks as Record<string, HookDef>)[hookName];
  Hooks[hookName] = {
    ...originalHook,
    mounted() {
      const self = this;
      if (window.VendorLoader && !window.chartBundleLoaded) {
        window.VendorLoader.loadCharts();
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
        if (originalHook.mounted) {
          originalHook.mounted.call(this);
        }
      }
    },
  };
});

// Core hooks - no bundle loading needed
Hooks.ErrorBoundary = ErrorBoundary;
Hooks.TimeAgoHook = TimeAgoHook;

// Theme management
const applyTheme = (theme: string | null) => {
  const element = document.documentElement;
  if (!element) return;

  if (theme === "auto" || !theme) {
    if (
      window.matchMedia &&
      window.matchMedia("(prefers-color-scheme: dark)").matches
    ) {
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
window.addEventListener("phx:set-theme", ((e: CustomEvent<{ theme: string }>) => {
  const theme = e.detail.theme;
  applyTheme(theme);
  localStorage.setItem("theme", theme);
  window.dispatchEvent(new CustomEvent("themeChanged"));
}) as EventListener);

// Listen for system theme changes when auto is selected
window
  .matchMedia("(prefers-color-scheme: dark)")
  .addEventListener("change", () => {
    if (localStorage.getItem("theme") === "auto") {
      applyTheme("auto");
      window.dispatchEvent(new CustomEvent("themeChanged"));
    }
  });

const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 5000,
  params: { _csrf_token: csrfToken, viewport_width: window.innerWidth },
  hooks: Hooks,
  timeout: 60000,
});

// Show progress bar on live navigation and form submits
topbar.config({
  barColors: { 0: "#29d" },
  shadowColor: "rgba(0, 0, 0, .3)",
});
window.addEventListener("phx:page-loading-start", (_info) => topbar.show(100));
window.addEventListener("phx:page-loading-stop", (_info) => topbar.hide());

// Handle connection draining reconnect events
window.addEventListener("phx:reconnect", ((e: CustomEvent<{ delay?: number }>) => {
  const delay = e.detail.delay || 1000;
  setTimeout(() => {
    liveSocket.disconnect();
    setTimeout(() => {
      liveSocket.connect();
    }, 100);
  }, delay);
}) as EventListener);

// connect if there are any LiveViews on the page
liveSocket.connect();

// Handle visibility change to ensure reconnection when page becomes visible
// This helps with iOS Safari background/foreground transitions
document.addEventListener("visibilitychange", () => {
  if (document.visibilityState === "visible") {
    const socket = (liveSocket as any).socket;
    if (socket && socket.connectionState() !== "open") {
      socket.disconnect(() => socket.connect());
    }
  }
});

// Workaround for Phoenix LiveView bug where fallback timer isn't cleared
// after successful WebSocket connection
window.addEventListener("phx:live_socket:connect", (_info) => {
  const socket = (liveSocket as any).socket;
  if (socket && socket.fallbackTimer) {
    clearTimeout(socket.fallbackTimer);
    socket.fallbackTimer = null;
  }
});

// Also check periodically in case the event doesn't fire
setTimeout(() => {
  const socket = (liveSocket as any).socket;
  if (socket && socket.isConnected() && socket.fallbackTimer) {
    clearTimeout(socket.fallbackTimer);
    socket.fallbackTimer = null;
  }
}, 5000);

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;

window.Hooks = Hooks;
export default Hooks;
