// Declare global Chart object from CDN
declare global {
    interface Window {
        Chart: any;
        chartInstances?: Map<string, any>;
    }
}

// Type for LiveView hooks
type Hook = any;

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

// Helper function to get theme-aware colors
const getThemeColors = () => {
    const isDark = document.documentElement.getAttribute('data-theme') === 'dark';
    return {
        text: isDark ? '#e5e7eb' : '#111827',
        grid: isDark ? '#374151' : '#9ca3af',
        background: isDark ? 'rgba(0, 0, 0, 0.1)' : 'rgba(255, 255, 255, 0.1)'
    };
};

declare global {
    interface Window {
        chartInstances?: Map<string, any>;
    }
}

function registerChartInstance(el: HTMLElement, hook: any) {
    if (!window.chartInstances) {
        window.chartInstances = new Map();
    }
    window.chartInstances.set(el.id, hook);
}

function unregisterChartInstance(el: HTMLElement) {
    if (window.chartInstances) {
        window.chartInstances.delete(el.id);
    }
}

function getLabels(el: HTMLElement) {
    const raw = el.getAttribute('data-chart-labels');
    if (!raw) return {};
    try {
        return JSON.parse(raw);
    } catch {
        return {};
    }
}

// All hooks are typed as 'any' for LiveView context compatibility
export const WeatherChartHooks: Record<string, Hook> = {
    ChartJSTempChart: {
        mounted() {
            const self = this as any;
            registerChartInstance(self.el as HTMLElement, self);
            self.renderChart();
            self.themeChangeHandler = () => self.renderChart();
            window.addEventListener('themeChanged', self.themeChangeHandler);
            self.handleEvent("update_weather_charts", ({ weather_history }: UpdateWeatherChartsPayload) => {
                self.el.dataset.weatherHistory = weather_history;
                self.renderChart();
            });
        },
        updated() { (this as any).renderChart(); },
        destroyed() {
            const self = this as any;
            if (self.themeChangeHandler) window.removeEventListener('themeChanged', self.themeChangeHandler);
            unregisterChartInstance(self.el as HTMLElement);
        },
        renderChart() {
            const self = this as any;
            if (self.chart) self.chart.destroy();
            const data: WeatherHistoryDatum[] = JSON.parse(self.el.dataset.weatherHistory!);
            const labels = getLabels(self.el);
            const times = data.map(d => new Date(d.timestamp));
            const temps = data.map(d => d.temperature);
            const dews = data.map(d => d.dew_point);
            const colors = getThemeColors();
            const canvas = self.el.querySelector('canvas') as HTMLCanvasElement;
            if (!canvas) return;
            self.chart = new window.Chart(canvas, {
                type: 'line',
                data: {
                    labels: times,
                    datasets: [
                        { label: labels.temp_label || 'Temperature (°F)', data: temps, borderColor: 'red', backgroundColor: 'rgba(255, 0, 0, 0.1)', tension: 0.1, pointRadius: 0 },
                        { label: labels.dew_label || 'Dew Point (°F)', data: dews, borderColor: 'blue', backgroundColor: 'rgba(0, 0, 255, 0.1)', tension: 0.1, pointRadius: 0 }
                    ]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: {
                        title: { display: true, text: labels.temp_title || 'Temperature & Dew Point (°F)', color: colors.text },
                        legend: { labels: { color: colors.text } }
                    },
                    scales: {
                        x: { type: 'time', time: { displayFormats: { hour: 'HH:mm', minute: 'HH:mm' } }, title: { display: true, text: labels.time || 'Time', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } },
                        y: { title: { display: true, text: labels.degf || '°F', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } }
                    }
                }
            });
        }
    },
    ChartJSHumidityChart: {
        mounted() {
            const self = this as any;
            registerChartInstance(self.el as HTMLElement, self);
            self.renderChart();
            self.themeChangeHandler = () => self.renderChart();
            window.addEventListener('themeChanged', self.themeChangeHandler);
            self.handleEvent("update_weather_charts", ({ weather_history }: UpdateWeatherChartsPayload) => {
                self.el.dataset.weatherHistory = weather_history;
                self.renderChart();
            });
        },
        updated() { (this as any).renderChart(); },
        destroyed() {
            const self = this as any;
            if (self.themeChangeHandler) window.removeEventListener('themeChanged', self.themeChangeHandler);
            unregisterChartInstance(self.el as HTMLElement);
        },
        renderChart() {
            const self = this as any;
            if (self.chart) self.chart.destroy();
            const data: WeatherHistoryDatum[] = JSON.parse(self.el.dataset.weatherHistory!);
            const labels = getLabels(self.el);
            const times = data.map(d => new Date(d.timestamp));
            const humidity = data.map(d => d.humidity);
            const colors = getThemeColors();
            const canvas = self.el.querySelector('canvas') as HTMLCanvasElement;
            if (!canvas) return;
            self.chart = new window.Chart(canvas, {
                type: 'line',
                data: { labels: times, datasets: [{ label: labels.humidity_label || 'Humidity (%)', data: humidity, borderColor: 'green', backgroundColor: 'rgba(0, 255, 0, 0.1)', tension: 0.1, pointRadius: 0 }] },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: { title: { display: true, text: labels.humidity_title || 'Humidity (%)', color: colors.text }, legend: { labels: { color: colors.text } } },
                    scales: { x: { type: 'time', time: { displayFormats: { hour: 'HH:mm', minute: 'HH:mm' } }, title: { display: true, text: labels.time || 'Time', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } }, y: { title: { display: true, text: labels.percent || '%', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } } }
                }
            });
        }
    },
    ChartJSPressureChart: {
        mounted() {
            const self = this as any;
            registerChartInstance(self.el as HTMLElement, self);
            self.renderChart();
            self.themeChangeHandler = () => self.renderChart();
            window.addEventListener('themeChanged', self.themeChangeHandler);
            self.handleEvent("update_weather_charts", ({ weather_history }: UpdateWeatherChartsPayload) => {
                self.el.dataset.weatherHistory = weather_history;
                self.renderChart();
            });
        },
        updated() { (this as any).renderChart(); },
        destroyed() {
            const self = this as any;
            if (self.themeChangeHandler) window.removeEventListener('themeChanged', self.themeChangeHandler);
            unregisterChartInstance(self.el as HTMLElement);
        },
        renderChart() {
            const self = this as any;
            if (self.chart) self.chart.destroy();
            const data: WeatherHistoryDatum[] = JSON.parse(self.el.dataset.weatherHistory!);
            const labels = getLabels(self.el);
            const times = data.map(d => new Date(d.timestamp));
            const pressure = data.map(d => d.pressure);
            const colors = getThemeColors();
            const canvas = self.el.querySelector('canvas') as HTMLCanvasElement;
            if (!canvas) return;
            self.chart = new window.Chart(canvas, {
                type: 'line',
                data: { labels: times, datasets: [{ label: labels.pressure_label || 'Pressure (mb)', data: pressure, borderColor: 'purple', backgroundColor: 'rgba(128, 0, 128, 0.1)', tension: 0.1, pointRadius: 0 }] },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: { title: { display: true, text: labels.pressure_title || 'Pressure (mb)', color: colors.text }, legend: { labels: { color: colors.text } } },
                    scales: { x: { type: 'time', time: { displayFormats: { hour: 'HH:mm', minute: 'HH:mm' } }, title: { display: true, text: labels.time || 'Time', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } }, y: { title: { display: true, text: labels.mb || 'mb', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } } }
                }
            });
        }
    },
    ChartJSWindChart: {
        mounted() {
            const self = this as any;
            registerChartInstance(self.el as HTMLElement, self);
            self.renderChart();
            self.themeChangeHandler = () => self.renderChart();
            window.addEventListener('themeChanged', self.themeChangeHandler);
            self.handleEvent("update_weather_charts", ({ weather_history }: UpdateWeatherChartsPayload) => {
                self.el.dataset.weatherHistory = weather_history;
                self.renderChart();
            });
        },
        updated() { (this as any).renderChart(); },
        destroyed() {
            const self = this as any;
            if (self.themeChangeHandler) window.removeEventListener('themeChanged', self.themeChangeHandler);
            unregisterChartInstance(self.el as HTMLElement);
        },
        renderChart() {
            const self = this as any;
            if (self.chart) self.chart.destroy();
            const data: WeatherHistoryDatum[] = JSON.parse(self.el.dataset.weatherHistory!);
            const times = data.map(d => new Date(d.timestamp));
            const windSpeed = data.map(d => d.wind_speed);
            const windGust = data.map(d => d.wind_gust);
            const colors = getThemeColors();
            const canvas = self.el.querySelector('canvas') as HTMLCanvasElement;
            if (!canvas) return;
            self.chart = new window.Chart(canvas, {
                type: 'line',
                data: {
                    labels: times, datasets: [
                        { label: 'Wind Speed (mph)', data: windSpeed, borderColor: 'orange', backgroundColor: 'rgba(255, 165, 0, 0.1)', tension: 0.1, pointRadius: 0 },
                        { label: 'Wind Gust (mph)', data: windGust, borderColor: 'brown', backgroundColor: 'rgba(165, 42, 42, 0.1)', tension: 0.1, pointRadius: 0 }
                    ]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: { title: { display: true, text: 'Wind Speed & Gust (mph)', color: colors.text }, legend: { labels: { color: colors.text } } },
                    scales: { x: { type: 'time', time: { displayFormats: { hour: 'HH:mm', minute: 'HH:mm' } }, title: { display: true, text: 'Time', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } }, y: { title: { display: true, text: 'mph', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } } }
                }
            });
        }
    },
    ChartJSRainChart: {
        mounted() {
            const self = this as any;
            registerChartInstance(self.el as HTMLElement, self);
            self.renderChart();
            self.themeChangeHandler = () => self.renderChart();
            window.addEventListener('themeChanged', self.themeChangeHandler);
            self.handleEvent("update_weather_charts", ({ weather_history }: UpdateWeatherChartsPayload) => {
                self.el.dataset.weatherHistory = weather_history;
                self.renderChart();
            });
        },
        updated() { (this as any).renderChart(); },
        destroyed() {
            const self = this as any;
            if (self.themeChangeHandler) window.removeEventListener('themeChanged', self.themeChangeHandler);
            unregisterChartInstance(self.el as HTMLElement);
        },
        renderChart() {
            const self = this as any;
            if (self.chart) self.chart.destroy();
            const data: WeatherHistoryDatum[] = JSON.parse(self.el.dataset.weatherHistory!);
            const times = data.map(d => new Date(d.timestamp));
            const rain1h = data.map(d => d.rain_1h);
            const rain24h = data.map(d => d.rain_24h);
            const rainSinceMidnight = data.map(d => d.rain_since_midnight);
            const colors = getThemeColors();
            const canvas = self.el.querySelector('canvas') as HTMLCanvasElement;
            if (!canvas) return;
            self.chart = new window.Chart(canvas, {
                type: 'line',
                data: {
                    labels: times, datasets: [
                        { label: 'Rain (1h)', data: rain1h, borderColor: 'blue', backgroundColor: 'rgba(0, 0, 255, 0.1)', tension: 0.1, pointRadius: 0 },
                        { label: 'Rain (24h)', data: rain24h, borderColor: 'cyan', backgroundColor: 'rgba(0, 255, 255, 0.1)', tension: 0.1, pointRadius: 0 },
                        { label: 'Rain (since midnight)', data: rainSinceMidnight, borderColor: 'navy', backgroundColor: 'rgba(0, 0, 128, 0.1)', tension: 0.1, pointRadius: 0 }
                    ]
                },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: { title: { display: true, text: 'Rainfall (inches)', color: colors.text }, legend: { labels: { color: colors.text } } },
                    scales: { x: { type: 'time', time: { displayFormats: { hour: 'HH:mm', minute: 'HH:mm' } }, title: { display: true, text: 'Time', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } }, y: { title: { display: true, text: 'inches', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } } }
                }
            });
        }
    },
    ChartJSLuminosityChart: {
        mounted() {
            const self = this as any;
            registerChartInstance(self.el as HTMLElement, self);
            self.renderChart();
            self.themeChangeHandler = () => self.renderChart();
            window.addEventListener('themeChanged', self.themeChangeHandler);
            self.handleEvent("update_weather_charts", ({ weather_history }: UpdateWeatherChartsPayload) => {
                self.el.dataset.weatherHistory = weather_history;
                self.renderChart();
            });
        },
        updated() { (this as any).renderChart(); },
        destroyed() {
            const self = this as any;
            if (self.themeChangeHandler) window.removeEventListener('themeChanged', self.themeChangeHandler);
            unregisterChartInstance(self.el as HTMLElement);
        },
        renderChart() {
            const self = this as any;
            if (self.chart) self.chart.destroy();
            const data: WeatherHistoryDatum[] = JSON.parse(self.el.dataset.weatherHistory!);
            const times = data.map(d => new Date(d.timestamp));
            const luminosity = data.map(d => d.luminosity);
            const colors = getThemeColors();
            const canvas = self.el.querySelector('canvas') as HTMLCanvasElement;
            if (!canvas) return;
            self.chart = new window.Chart(canvas, {
                type: 'line',
                data: { labels: times, datasets: [{ label: 'Luminosity', data: luminosity, borderColor: 'yellow', backgroundColor: 'rgba(255, 255, 0, 0.1)', tension: 0.1, pointRadius: 0 }] },
                options: {
                    responsive: true,
                    maintainAspectRatio: false,
                    plugins: { title: { display: true, text: 'Luminosity', color: colors.text }, legend: { labels: { color: colors.text } } },
                    scales: { x: { type: 'time', time: { displayFormats: { hour: 'HH:mm', minute: 'HH:mm' } }, title: { display: true, text: 'Time', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } }, y: { title: { display: true, text: 'Luminosity', color: colors.text }, ticks: { color: colors.text }, grid: { color: colors.grid } } }
                }
            });
        }
    }
};

export default WeatherChartHooks; 