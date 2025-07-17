// Type definitions for Chart.js configurations used in APRS.me

import type { ChartType, ChartDataset, ScaleOptions } from 'chart.js';

// Chart dataset types
export interface LineDataset extends ChartDataset<'line'> {
  label: string;
  data: (number | null | undefined)[];
  borderColor: string;
  backgroundColor: string;
  tension: number;
  pointRadius: number;
}

export interface BarDataset extends ChartDataset<'bar'> {
  label: string;
  data: (number | null | undefined)[];
  backgroundColor: string;
  borderColor: string;
  borderWidth: number;
}

export type WeatherChartDataset = LineDataset | BarDataset;

// Y-axis options
export interface YAxisOptions extends Partial<ScaleOptions<'linear'>> {
  min?: number;
  max?: number;
}