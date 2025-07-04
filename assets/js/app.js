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

Hooks.ChartJSTempChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const temps = data.map(d => d.temperature);
    const dews = data.map(d => d.dew_point);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for temperature chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [
          {
            label: 'Temperature (°F)',
            data: temps,
            borderColor: 'red',
            backgroundColor: 'rgba(255, 0, 0, 0.1)',
            tension: 0.1
          },
          {
            label: 'Dew Point (°F)',
            data: dews,
            borderColor: 'blue',
            backgroundColor: 'rgba(0, 0, 255, 0.1)',
            tension: 0.1
          }
        ]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Temperature & Dew Point (°F)'
          }
        },
        scales: {
          x: {
            type: 'time',
            time: {
              displayFormats: {
                hour: 'HH:mm',
                minute: 'HH:mm'
              }
            },
            title: {
              display: true,
              text: 'Time'
            }
          },
          y: {
            title: {
              display: true,
              text: '°F'
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSHumidityChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const hums = data.map(d => d.humidity);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for humidity chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [{
          label: 'Humidity (%)',
          data: hums,
          borderColor: 'blue',
          backgroundColor: 'rgba(0, 0, 255, 0.1)',
          tension: 0.1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Humidity (%)'
          }
        },
        scales: {
          x: {
            type: 'time',
            time: {
              displayFormats: {
                hour: 'HH:mm',
                minute: 'HH:mm'
              }
            },
            title: {
              display: true,
              text: 'Time'
            }
          },
          y: {
            title: {
              display: true,
              text: '%'
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSPressureChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const pressures = data.map(d => d.pressure);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for pressure chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [{
          label: 'Pressure (hPa)',
          data: pressures,
          borderColor: 'green',
          backgroundColor: 'rgba(0, 128, 0, 0.1)',
          tension: 0.1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Pressure (hPa)'
          }
        },
        scales: {
          x: {
            type: 'time',
            time: {
              displayFormats: {
                hour: 'HH:mm',
                minute: 'HH:mm'
              }
            },
            title: {
              display: true,
              text: 'Time'
            }
          },
          y: {
            title: {
              display: true,
              text: 'hPa'
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSWindDirectionChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const dirs = data.map(d => d.wind_direction);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for wind direction chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [{
          label: 'Wind Direction (°)',
          data: dirs,
          borderColor: 'orange',
          backgroundColor: 'rgba(255, 165, 0, 0.1)',
          tension: 0.1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Wind Direction (°)'
          }
        },
        scales: {
          x: {
            type: 'time',
            time: {
              displayFormats: {
                hour: 'HH:mm',
                minute: 'HH:mm'
              }
            },
            title: {
              display: true,
              text: 'Time'
            }
          },
          y: {
            title: {
              display: true,
              text: '°'
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSWindSpeedChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const speeds = data.map(d => d.wind_speed);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for wind speed chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [{
          label: 'Wind Speed (mph)',
          data: speeds,
          borderColor: 'purple',
          backgroundColor: 'rgba(128, 0, 128, 0.1)',
          tension: 0.1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Wind Speed (mph)'
          }
        },
        scales: {
          x: {
            type: 'time',
            time: {
              displayFormats: {
                hour: 'HH:mm',
                minute: 'HH:mm'
              }
            },
            title: {
              display: true,
              text: 'Time'
            }
          },
          y: {
            title: {
              display: true,
              text: 'mph'
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSRainChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const rain1h = data.map(d => d.rain_1h);
    const rain24h = data.map(d => d.rain_24h);
    const rainMid = data.map(d => d.rain_since_midnight);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for rain chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [
          {
            label: 'Rain (1h)',
            data: rain1h,
            borderColor: 'blue',
            backgroundColor: 'rgba(0, 0, 255, 0.1)',
            tension: 0.1
          },
          {
            label: 'Rain (24h)',
            data: rain24h,
            borderColor: 'navy',
            backgroundColor: 'rgba(0, 0, 128, 0.1)',
            tension: 0.1
          },
          {
            label: 'Rain (since midnight)',
            data: rainMid,
            borderColor: 'teal',
            backgroundColor: 'rgba(0, 128, 128, 0.1)',
            tension: 0.1
          }
        ]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Rain (inches)'
          }
        },
        scales: {
          x: {
            type: 'time',
            time: {
              displayFormats: {
                hour: 'HH:mm',
                minute: 'HH:mm'
              }
            },
            title: {
              display: true,
              text: 'Time'
            }
          },
          y: {
            title: {
              display: true,
              text: 'in'
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSLuminosityChart = {
  mounted() { this.renderChart(); },
  updated() { this.renderChart(); },
  renderChart() {
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const lum = data.map(d => d.luminosity);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for luminosity chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [{
          label: 'Luminosity',
          data: lum,
          borderColor: 'gold',
          backgroundColor: 'rgba(255, 215, 0, 0.1)',
          tension: 0.1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Luminosity'
          }
        },
        scales: {
          x: {
            type: 'time',
            time: {
              displayFormats: {
                hour: 'HH:mm',
                minute: 'HH:mm'
              }
            },
            title: {
              display: true,
              text: 'Time'
            }
          },
          y: {
            title: {
              display: true,
              text: ''
            }
          }
        }
      }
    });
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
