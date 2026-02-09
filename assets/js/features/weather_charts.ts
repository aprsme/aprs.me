// Chart.js and date adapter are loaded globally from vendor bundle
// We'll access it later when it's actually loaded

import type { ChartConfiguration, ChartType } from "chart.js";
import type { WeatherChartDataset, YAxisOptions } from "../types/chart-types";
import type { HandleEventFunction } from "../types/events";

// Declare global Chart object
declare global {
  interface Window {
    Chart: typeof Chart;
    chartInstances?: Map<string, ChartHookContext>;
  }
}

// Type for LiveView hooks
interface Hook {
  mounted?: () => void;
  updated?: () => void;
  destroyed?: () => void;
  el: HTMLElement;
  handleEvent: HandleEventFunction;
}

// Define chart hook context type
interface ChartHookContext extends Hook {
  chart?: Chart;
  themeChangeHandler?: () => void;
  renderChart: () => void;
}

// Type for weather history data
interface WeatherHistoryDatum {
  timestamp: string;
  temperature?: number;
  dew_point?: number;
  humidity?: number;
  pressure?: number;
  wind_direction?: number;
  wind_speed?: number;
  wind_gust?: number;
  rain_1h?: number;
  rain_24h?: number;
  rain_since_midnight?: number;
  luminosity?: number;
}

// Type for the event payload
interface UpdateWeatherChartsPayload {
  weather_history: string;
}

// Helper function to safely parse weather history data
const parseWeatherHistory = (
  dataStr: string | undefined,
): WeatherHistoryDatum[] => {
  if (!dataStr) {
    console.warn("No weather history data provided");
    return [];
  }
  try {
    return JSON.parse(dataStr);
  } catch (error) {
    console.error("Failed to parse weather history data:", error);
    return [];
  }
};

// Helper function to get theme-aware colors
const getThemeColors = () => {
  const isDark = document.documentElement.getAttribute("data-theme") === "dark";
  return {
    text: isDark ? "#e5e7eb" : "#111827",
    grid: isDark ? "#374151" : "#9ca3af",
    background: isDark ? "rgba(0, 0, 0, 0.1)" : "rgba(255, 255, 255, 0.1)",
  };
};

// Register a chart instance
const registerChartInstance = (
  element: HTMLElement,
  instance: ChartHookContext,
) => {
  if (!window.chartInstances) {
    window.chartInstances = new Map();
  }
  const elementId = element.id || `chart-${Date.now()}`;
  if (!element.id) element.id = elementId;
  window.chartInstances.set(elementId, instance);
};

// Unregister a chart instance
const unregisterChartInstance = (element: HTMLElement) => {
  if (window.chartInstances && element.id) {
    const instance = window.chartInstances.get(element.id);
    if (instance?.chart) {
      instance.chart.destroy();
    }
    window.chartInstances.delete(element.id);
  }
};

// Get labels from the element
const getLabels = (el: HTMLElement | null): Record<string, string> => {
  if (!el || !el.dataset.labels) return {};
  const raw = el.dataset.labels;
  if (!raw) return {};
  try {
    return JSON.parse(raw);
  } catch {
    return {};
  }
};

// Chart configurations
interface ChartConfig {
  type: ChartType;
  datasets: (
    data: WeatherHistoryDatum[],
    labels: Record<string, string>,
  ) => WeatherChartDataset[];
  title: (labels: Record<string, string>) => string;
  yAxisLabel?: (labels: Record<string, string>) => string;
  yAxisOptions?: YAxisOptions;
}

const chartConfigs: Record<string, ChartConfig> = {
  temperature: {
    type: "line",
    datasets: (data, labels) => [
      {
        label: labels.temp_label || "Temperature (°F)",
        data: data.map((d) => d.temperature),
        borderColor: "red",
        backgroundColor: "rgba(255, 0, 0, 0.1)",
        tension: 0.1,
        pointRadius: 0,
      },
      {
        label: labels.dew_label || "Dew Point (°F)",
        data: data.map((d) => d.dew_point),
        borderColor: "blue",
        backgroundColor: "rgba(0, 0, 255, 0.1)",
        tension: 0.1,
        pointRadius: 0,
      },
    ],
    title: (labels) => labels.temp_title || "Temperature & Dew Point (°F)",
    yAxisLabel: (labels) => labels.degf || "°F",
  },
  humidity: {
    type: "line",
    datasets: (data, labels) => [
      {
        label: labels.hum_label || "Humidity (%)",
        data: data.map((d) => d.humidity),
        borderColor: "green",
        backgroundColor: "rgba(0, 255, 0, 0.1)",
        tension: 0.1,
        pointRadius: 0,
      },
    ],
    title: (labels) => labels.hum_title || "Humidity (%)",
    yAxisLabel: (labels) => labels.percent || "%",
    yAxisOptions: { min: 0, max: 100 },
  },
  pressure: {
    type: "line",
    datasets: (data, labels) => [
      {
        label: labels.prs_label || "Pressure (mb)",
        data: data.map((d) => d.pressure),
        borderColor: "purple",
        backgroundColor: "rgba(128, 0, 128, 0.1)",
        tension: 0.1,
        pointRadius: 0,
      },
    ],
    title: (labels) => labels.prs_title || "Barometric Pressure (mb)",
    yAxisLabel: (labels) => labels.mb || "mb",
  },
  wind: {
    type: "line",
    datasets: (data, labels) => [
      {
        label: labels.spd_label || "Wind Speed (mph)",
        data: data.map((d) => d.wind_speed),
        borderColor: "orange",
        backgroundColor: "rgba(255, 165, 0, 0.1)",
        tension: 0.1,
        pointRadius: 0,
      },
      {
        label: labels.gst_label || "Wind Gust (mph)",
        data: data.map((d) => d.wind_gust),
        borderColor: "red",
        backgroundColor: "rgba(255, 0, 0, 0.1)",
        tension: 0.1,
        pointRadius: 0,
      },
    ],
    title: (labels) => labels.wnd_title || "Wind Speed & Gust (mph)",
    yAxisLabel: (labels) => labels.mph || "mph",
    yAxisOptions: { min: 0 },
  },
  rain: {
    type: "bar",
    datasets: (data, labels) => [
      {
        label: labels.h1_label || "Rain 1h (in)",
        data: data.map((d) => d.rain_1h),
        backgroundColor: "rgba(54, 162, 235, 0.8)",
        borderColor: "rgba(54, 162, 235, 1)",
        borderWidth: 1,
      },
      {
        label: labels.h24_label || "Rain 24h (in)",
        data: data.map((d) => d.rain_24h),
        backgroundColor: "rgba(153, 102, 255, 0.8)",
        borderColor: "rgba(153, 102, 255, 1)",
        borderWidth: 1,
      },
    ],
    title: (labels) => labels.rain_title || "Rainfall (inches)",
    yAxisLabel: (labels) => labels.inches || "inches",
    yAxisOptions: { min: 0 },
  },
  luminosity: {
    type: "line",
    datasets: (data, labels) => [
      {
        label: labels.lum_label || "Luminosity (W/m²)",
        data: data.map((d) => d.luminosity),
        borderColor: "gold",
        backgroundColor: "rgba(255, 215, 0, 0.1)",
        tension: 0.1,
        pointRadius: 0,
      },
    ],
    title: (labels) => labels.lum_title || "Solar Radiation (W/m²)",
    yAxisLabel: (labels) => labels.wm2 || "W/m²",
    yAxisOptions: { min: 0 },
  },
};

// Create a chart hook
function createChartHook(configKey: string): Hook {
  const config = chartConfigs[configKey];
  if (!config) {
    throw new Error(`Unknown chart config: ${configKey}`);
  }

  return {
    mounted() {
      const self = this as ChartHookContext;
      registerChartInstance(self.el, self);
      self.renderChart = () => {
        if (self.chart) self.chart.destroy();

        const data: WeatherHistoryDatum[] = parseWeatherHistory(
          self.el.dataset.weatherHistory,
        );
        if (data.length === 0) {
          console.log("No weather data available for chart");
          return;
        }

        // Skip rendering if we have less than 2 data points (can't create a meaningful time series)
        if (data.length < 2) {
          console.log(
            "Insufficient weather data for chart (need at least 2 data points)",
          );
          return;
        }

        const canvas = self.el.querySelector(
          "canvas",
        ) as HTMLCanvasElement | null;
        if (!canvas) {
          console.error("Canvas element not found for chart");
          return;
        }

        const labels = getLabels(self.el);
        const times = data.map((d) => new Date(d.timestamp));
        const colors = getThemeColors();

        const chartConfig: ChartConfiguration = {
          type: config.type,
          data: {
            labels: times,
            datasets: config.datasets(data, labels),
          },
          options: {
            adapters: { date: { locale: "en-GB" } },
            responsive: true,
            maintainAspectRatio: false,
            plugins: {
              title: {
                display: true,
                text: config.title(labels),
                color: colors.text,
              },
              legend: { labels: { color: colors.text } },
            },
            scales: {
              x: {
                type: "time",
                time: {
                  unit: "minute",
                  tooltipFormat: "HH:mm",
                  displayFormats: { minute: "HH:mm", hour: "HH:mm" },
                  locale: "en-GB",
                },
                title: {
                  display: true,
                  text: labels.time || "Time",
                  color: colors.text,
                },
                ticks: { color: colors.text, maxTicksLimit: 8 },
                grid: { color: colors.grid },
              },
              y: {
                title: {
                  display: true,
                  text: config.yAxisLabel ? config.yAxisLabel(labels) : "",
                  color: colors.text,
                },
                ticks: { color: colors.text },
                grid: { color: colors.grid },
                ...(config.yAxisOptions || {}),
              },
            },
          },
        };

        // Check if Chart.js is loaded
        if (!window.Chart) {
          console.warn("Chart.js not loaded yet, retrying...");
          setTimeout(() => self.renderChart(), 100);
          return;
        }

        self.chart = new window.Chart(canvas, chartConfig);
      };

      self.renderChart();
      self.themeChangeHandler = () => self.renderChart();
      window.addEventListener("themeChanged", self.themeChangeHandler);
      self.handleEvent(
        "update_weather_charts",
        ({ weather_history }: UpdateWeatherChartsPayload) => {
          self.el.dataset.weatherHistory = weather_history;
          self.renderChart();
        },
      );
    },

    updated() {
      (this as ChartHookContext).renderChart();
    },

    destroyed() {
      const self = this as ChartHookContext;
      if (self.themeChangeHandler) {
        window.removeEventListener("themeChanged", self.themeChangeHandler);
      }
      unregisterChartInstance(self.el);
    },
  };
}

// Export weather chart hooks
export const WeatherChartHooks: Record<string, Hook> = {
  ChartJSTempChart: createChartHook("temperature"),
  ChartJSHumidityChart: createChartHook("humidity"),
  ChartJSPressureChart: createChartHook("pressure"),
  ChartJSWindChart: createChartHook("wind"),
  ChartJSRainChart: createChartHook("rain"),
  ChartJSLuminosityChart: createChartHook("luminosity"),
};

export default WeatherChartHooks;
