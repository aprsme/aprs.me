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
  mounted() { 
    console.log('Temperature chart mounted');
    
    // Initialize global chart instances map if it doesn't exist
    if (!window.chartInstances) {
      window.chartInstances = new Map();
    }
    
    // Register this chart instance
    window.chartInstances.set(this.el.id, this);
    
    this.renderChart();
    // Listen for theme changes
    this.themeChangeHandler = () => {
      console.log('Temperature chart received themeChanged event');
      if (this.chart) {
        console.log('Re-rendering temperature chart');
        this.renderChart();
      }
    };
    window.addEventListener('themeChanged', this.themeChangeHandler);
  },
  updated() { this.renderChart(); },
  destroyed() {
    // Clean up event listener
    if (this.themeChangeHandler) {
      window.removeEventListener('themeChanged', this.themeChangeHandler);
    }
    // Remove from global chart instances
    if (window.chartInstances) {
      window.chartInstances.delete(this.el.id);
    }
  },
  renderChart() {
    console.log('Temperature chart renderChart called');
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const temps = data.map(d => d.temperature);
    const dews = data.map(d => d.dew_point);
    const colors = getThemeColors();
    console.log('Temperature chart colors:', colors);
    
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
            text: 'Temperature & Dew Point (°F)',
            color: colors.text
          },
          legend: {
            labels: {
              color: colors.text
            }
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
              text: 'Time',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          },
          y: {
            title: {
              display: true,
              text: '°F',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSHumidityChart = {
  mounted() { 
    console.log('Humidity chart mounted');
    
    // Initialize global chart instances map if it doesn't exist
    if (!window.chartInstances) {
      window.chartInstances = new Map();
    }
    
    // Register this chart instance
    window.chartInstances.set(this.el.id, this);
    
    this.renderChart();
    // Listen for theme changes
    this.themeChangeHandler = () => {
      console.log('Humidity chart received themeChanged event');
      if (this.chart) {
        console.log('Re-rendering humidity chart');
        this.renderChart();
      }
    };
    window.addEventListener('themeChanged', this.themeChangeHandler);
  },
  updated() { this.renderChart(); },
  destroyed() {
    // Clean up event listener
    if (this.themeChangeHandler) {
      window.removeEventListener('themeChanged', this.themeChangeHandler);
    }
    // Remove from global chart instances
    if (window.chartInstances) {
      window.chartInstances.delete(this.el.id);
    }
  },
  renderChart() {
    console.log('Humidity chart renderChart called');
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const humidity = data.map(d => d.humidity);
    const colors = getThemeColors();
    console.log('Humidity chart colors:', colors);
    
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
          data: humidity,
          borderColor: 'green',
          backgroundColor: 'rgba(0, 255, 0, 0.1)',
          tension: 0.1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Humidity (%)',
            color: colors.text
          },
          legend: {
            labels: {
              color: colors.text
            }
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
              text: 'Time',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          },
          y: {
            title: {
              display: true,
              text: '%',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSPressureChart = {
  mounted() { 
    console.log('Pressure chart mounted');
    
    // Initialize global chart instances map if it doesn't exist
    if (!window.chartInstances) {
      window.chartInstances = new Map();
    }
    
    // Register this chart instance
    window.chartInstances.set(this.el.id, this);
    
    this.renderChart();
    // Listen for theme changes
    this.themeChangeHandler = () => {
      console.log('Pressure chart received themeChanged event');
      if (this.chart) {
        console.log('Re-rendering pressure chart');
        this.renderChart();
      }
    };
    window.addEventListener('themeChanged', this.themeChangeHandler);
  },
  updated() { this.renderChart(); },
  destroyed() {
    // Clean up event listener
    if (this.themeChangeHandler) {
      window.removeEventListener('themeChanged', this.themeChangeHandler);
    }
    // Remove from global chart instances
    if (window.chartInstances) {
      window.chartInstances.delete(this.el.id);
    }
  },
  renderChart() {
    console.log('Pressure chart renderChart called');
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const pressure = data.map(d => d.pressure);
    const colors = getThemeColors();
    console.log('Pressure chart colors:', colors);
    
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
          label: 'Pressure (mb)',
          data: pressure,
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
            text: 'Pressure (mb)',
            color: colors.text
          },
          legend: {
            labels: {
              color: colors.text
            }
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
              text: 'Time',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          },
          y: {
            title: {
              display: true,
              text: 'mb',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSWindChart = {
  mounted() { 
    console.log('Wind chart mounted');
    
    // Initialize global chart instances map if it doesn't exist
    if (!window.chartInstances) {
      window.chartInstances = new Map();
    }
    
    // Register this chart instance
    window.chartInstances.set(this.el.id, this);
    
    this.renderChart();
    // Listen for theme changes
    this.themeChangeHandler = () => {
      console.log('Wind chart received themeChanged event');
      if (this.chart) {
        console.log('Re-rendering wind chart');
        this.renderChart();
      }
    };
    window.addEventListener('themeChanged', this.themeChangeHandler);
  },
  updated() { this.renderChart(); },
  destroyed() {
    // Clean up event listener
    if (this.themeChangeHandler) {
      window.removeEventListener('themeChanged', this.themeChangeHandler);
    }
    // Remove from global chart instances
    if (window.chartInstances) {
      window.chartInstances.delete(this.el.id);
    }
  },
  renderChart() {
    console.log('Wind chart renderChart called');
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const windSpeed = data.map(d => d.wind_speed);
    const windGust = data.map(d => d.wind_gust);
    const colors = getThemeColors();
    console.log('Wind chart colors:', colors);
    
    const canvas = this.el.querySelector('canvas');
    if (!canvas) {
      console.error('Canvas element not found for wind chart');
      return;
    }
    
    this.chart = new Chart(canvas, {
      type: 'line',
      data: {
        labels: times,
        datasets: [
          {
            label: 'Wind Speed (mph)',
            data: windSpeed,
            borderColor: 'orange',
            backgroundColor: 'rgba(255, 165, 0, 0.1)',
            tension: 0.1
          },
          {
            label: 'Wind Gust (mph)',
            data: windGust,
            borderColor: 'brown',
            backgroundColor: 'rgba(165, 42, 42, 0.1)',
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
            text: 'Wind Speed & Gust (mph)',
            color: colors.text
          },
          legend: {
            labels: {
              color: colors.text
            }
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
              text: 'Time',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          },
          y: {
            title: {
              display: true,
              text: 'mph',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSRainChart = {
  mounted() { 
    console.log('Rain chart mounted');
    
    // Initialize global chart instances map if it doesn't exist
    if (!window.chartInstances) {
      window.chartInstances = new Map();
    }
    
    // Register this chart instance
    window.chartInstances.set(this.el.id, this);
    
    this.renderChart();
    // Listen for theme changes
    this.themeChangeHandler = () => {
      console.log('Rain chart received themeChanged event');
      if (this.chart) {
        console.log('Re-rendering rain chart');
        this.renderChart();
      }
    };
    window.addEventListener('themeChanged', this.themeChangeHandler);
  },
  updated() { this.renderChart(); },
  destroyed() {
    // Clean up event listener
    if (this.themeChangeHandler) {
      window.removeEventListener('themeChanged', this.themeChangeHandler);
    }
    // Remove from global chart instances
    if (window.chartInstances) {
      window.chartInstances.delete(this.el.id);
    }
  },
  renderChart() {
    console.log('Rain chart renderChart called');
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const rain1h = data.map(d => d.rain_1h);
    const rain24h = data.map(d => d.rain_24h);
    const rainSinceMidnight = data.map(d => d.rain_since_midnight);
    const colors = getThemeColors();
    console.log('Rain chart colors:', colors);
    
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
            borderColor: 'cyan',
            backgroundColor: 'rgba(0, 255, 255, 0.1)',
            tension: 0.1
          },
          {
            label: 'Rain (since midnight)',
            data: rainSinceMidnight,
            borderColor: 'navy',
            backgroundColor: 'rgba(0, 0, 128, 0.1)',
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
            text: 'Rainfall (inches)',
            color: colors.text
          },
          legend: {
            labels: {
              color: colors.text
            }
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
              text: 'Time',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          },
          y: {
            title: {
              display: true,
              text: 'inches',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          }
        }
      }
    });
  }
};

Hooks.ChartJSLuminosityChart = {
  mounted() { 
    console.log('Luminosity chart mounted');
    
    // Initialize global chart instances map if it doesn't exist
    if (!window.chartInstances) {
      window.chartInstances = new Map();
    }
    
    // Register this chart instance
    window.chartInstances.set(this.el.id, this);
    
    this.renderChart();
    // Listen for theme changes
    this.themeChangeHandler = () => {
      console.log('Luminosity chart received themeChanged event');
      if (this.chart) {
        console.log('Re-rendering luminosity chart');
        this.renderChart();
      }
    };
    window.addEventListener('themeChanged', this.themeChangeHandler);
  },
  updated() { this.renderChart(); },
  destroyed() {
    // Clean up event listener
    if (this.themeChangeHandler) {
      window.removeEventListener('themeChanged', this.themeChangeHandler);
    }
    // Remove from global chart instances
    if (window.chartInstances) {
      window.chartInstances.delete(this.el.id);
    }
  },
  renderChart() {
    console.log('Luminosity chart renderChart called');
    if (this.chart) {
      this.chart.destroy();
    }
    
    const data = JSON.parse(this.el.dataset.weatherHistory);
    const times = data.map(d => new Date(d.timestamp));
    const luminosity = data.map(d => d.luminosity);
    const colors = getThemeColors();
    console.log('Luminosity chart colors:', colors);
    
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
          data: luminosity,
          borderColor: 'yellow',
          backgroundColor: 'rgba(255, 255, 0, 0.1)',
          tension: 0.1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          title: {
            display: true,
            text: 'Luminosity',
            color: colors.text
          },
          legend: {
            labels: {
              color: colors.text
            }
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
              text: 'Time',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          },
          y: {
            title: {
              display: true,
              text: 'Luminosity',
              color: colors.text
            },
            ticks: {
              color: colors.text
            },
            grid: {
              color: colors.grid
            }
          }
        }
      }
    });
  }
};

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
