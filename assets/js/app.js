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
import topbar from "../vendor/topbar";
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

Hooks.PlotlyTempChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => d.timestamp);
    const temps = data.map(d => d.temperature);
    const dews = data.map(d => d.dew_point);
    Plotly.newPlot(this.el, [
      { x: times, y: temps, name: "Temperature", type: "scatter", line: { color: "red" } },
      { x: times, y: dews, name: "Dew Point", type: "scatter", line: { color: "blue" } }
    ], {
      title: "Temperature & Dew Point (°F)",
      xaxis: { title: "Time" },
      yaxis: { title: "°F" }
    }, {
      responsive: true,
      staticPlot: true,
      displayModeBar: false
    });
  }
};

Hooks.PlotlyHumidityChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => d.timestamp);
    const hums = data.map(d => d.humidity);
    Plotly.newPlot(this.el, [
      { x: times, y: hums, name: "Humidity", type: "scatter", line: { color: "blue" } }
    ], {
      title: "Humidity (%)",
      xaxis: { title: "Time" },
      yaxis: { title: "%" }
    }, {
      responsive: true,
      staticPlot: true,
      displayModeBar: false
    });
  }
};

Hooks.PlotlyPressureChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => d.timestamp);
    const pressures = data.map(d => d.pressure);
    Plotly.newPlot(this.el, [
      { x: times, y: pressures, name: "Pressure", type: "scatter", line: { color: "green" } }
    ], {
      title: "Pressure (hPa)",
      xaxis: { title: "Time" },
      yaxis: { title: "hPa" }
    }, {responsive: true, staticPlot: true, displayModeBar: false});
  }
};

Hooks.PlotlyWindDirectionChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => d.timestamp);
    const dirs = data.map(d => d.wind_direction);
    Plotly.newPlot(this.el, [
      { x: times, y: dirs, name: "Wind Direction", type: "scatter", line: { color: "orange" } }
    ], {
      title: "Wind Direction (°)",
      xaxis: { title: "Time" },
      yaxis: { title: "°" }
    }, {responsive: true, staticPlot: true, displayModeBar: false});
  }
};

Hooks.PlotlyWindSpeedChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => d.timestamp);
    const speeds = data.map(d => d.wind_speed);
    Plotly.newPlot(this.el, [
      { x: times, y: speeds, name: "Wind Speed", type: "scatter", line: { color: "purple" } }
    ], {
      title: "Wind Speed (mph)",
      xaxis: { title: "Time" },
      yaxis: { title: "mph" }
    }, {responsive: true, staticPlot: true, displayModeBar: false});
  }
};

Hooks.PlotlyRainChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => d.timestamp);
    const rain1h = data.map(d => d.rain_1h);
    const rain24h = data.map(d => d.rain_24h);
    const rainMid = data.map(d => d.rain_since_midnight);
    Plotly.newPlot(this.el, [
      { x: times, y: rain1h, name: "Rain (1h)", type: "scatter", line: { color: "blue" } },
      { x: times, y: rain24h, name: "Rain (24h)", type: "scatter", line: { color: "navy" } },
      { x: times, y: rainMid, name: "Rain (since midnight)", type: "scatter", line: { color: "teal" } }
    ], {
      title: "Rain (inches)",
      xaxis: { title: "Time" },
      yaxis: { title: "in" }
    }, {responsive: true, staticPlot: true, displayModeBar: false});
  }
};

Hooks.PlotlyLuminosityChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => d.timestamp);
    const lum = data.map(d => d.luminosity);
    Plotly.newPlot(this.el, [
      { x: times, y: lum, name: "Luminosity", type: "scatter", line: { color: "gold" } }
    ], {
      title: "Luminosity",
      xaxis: { title: "Time" },
      yaxis: { title: "" }
    }, {responsive: true, staticPlot: true, displayModeBar: false});
  }
};

let liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
});

// Show progress bar on live navigation and form submits
topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (info) => topbar.delayedShow(200));
window.addEventListener("phx:page-loading-stop", (info) => topbar.hide());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;

window.Hooks = Hooks;
export default Hooks;
