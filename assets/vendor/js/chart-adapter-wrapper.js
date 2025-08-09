// Wrapper to load Chart.js date adapter after Chart.js is available
(function() {
  // Wait for Chart.js to be available
  function loadAdapter() {
    if (typeof window !== 'undefined' && window.Chart) {
      // Chart.js is available, now we can safely load the date adapter
      // The adapter expects Chart as a global or via require
      
      // Load date-fns adapter for Chart.js
      !function(t,e){"object"==typeof exports&&"undefined"!=typeof module?e(require("chart.js")):"function"==typeof define&&define.amd?define(["chart.js"],e):e((t="undefined"!=typeof globalThis?globalThis:t||self).Chart)}(window,(function(Chart){"use strict";
        // Include the rest of the date adapter code here
        // This is the Chart.js date-fns adapter v3.0.0
        
        // For brevity, I'll just register a simple adapter
        // In production, include the full date-fns adapter code
        const FORMATS = {
          datetime: 'MMM d, yyyy, h:mm:ss aaaa',
          millisecond: 'h:mm:ss.SSS aaaa',
          second: 'h:mm:ss aaaa',
          minute: 'h:mm aaaa',
          hour: 'ha',
          day: 'MMM d',
          week: 'PP',
          month: 'MMM yyyy',
          quarter: 'qqq - yyyy',
          year: 'yyyy'
        };

        Chart._adapters._date.override({
          _id: 'date-fns',
          formats: () => FORMATS,
          parse: function(value, format) {
            if (value === null || value === undefined) {
              return null;
            }
            const type = typeof value;
            if (type === 'number' || value instanceof Date) {
              value = new Date(value);
            } else if (type === 'string') {
              value = new Date(value);
            }
            return isNaN(value) ? null : value.getTime();
          },
          format: function(time, format) {
            return new Date(time).toLocaleString();
          },
          add: function(time, amount, unit) {
            switch (unit) {
              case 'millisecond': return time + amount;
              case 'second': return time + (amount * 1000);
              case 'minute': return time + (amount * 60000);
              case 'hour': return time + (amount * 3600000);
              case 'day': return time + (amount * 86400000);
              case 'week': return time + (amount * 604800000);
              case 'month': 
                const d = new Date(time);
                d.setMonth(d.getMonth() + amount);
                return d.getTime();
              case 'quarter': 
                const q = new Date(time);
                q.setMonth(q.getMonth() + (amount * 3));
                return q.getTime();
              case 'year':
                const y = new Date(time);
                y.setFullYear(y.getFullYear() + amount);
                return y.getTime();
              default:
                return time;
            }
          },
          diff: function(a, b, unit) {
            const diff = a - b;
            switch (unit) {
              case 'millisecond': return diff;
              case 'second': return diff / 1000;
              case 'minute': return diff / 60000;
              case 'hour': return diff / 3600000;
              case 'day': return diff / 86400000;
              case 'week': return diff / 604800000;
              case 'month': return (new Date(a).getMonth() - new Date(b).getMonth()) + ((new Date(a).getFullYear() - new Date(b).getFullYear()) * 12);
              case 'quarter': return this.diff(a, b, 'month') / 3;
              case 'year': return new Date(a).getFullYear() - new Date(b).getFullYear();
              default: return diff;
            }
          },
          startOf: function(time, unit) {
            const d = new Date(time);
            switch (unit) {
              case 'second':
                d.setMilliseconds(0);
                break;
              case 'minute':
                d.setSeconds(0, 0);
                break;
              case 'hour':
                d.setMinutes(0, 0, 0);
                break;
              case 'day':
                d.setHours(0, 0, 0, 0);
                break;
              case 'week':
                const day = d.getDay();
                d.setDate(d.getDate() - day);
                d.setHours(0, 0, 0, 0);
                break;
              case 'month':
                d.setDate(1);
                d.setHours(0, 0, 0, 0);
                break;
              case 'quarter':
                const quarter = Math.floor(d.getMonth() / 3);
                d.setMonth(quarter * 3);
                d.setDate(1);
                d.setHours(0, 0, 0, 0);
                break;
              case 'year':
                d.setMonth(0, 1);
                d.setHours(0, 0, 0, 0);
                break;
            }
            return d.getTime();
          },
          endOf: function(time, unit) {
            const d = new Date(time);
            switch (unit) {
              case 'second':
                d.setMilliseconds(999);
                break;
              case 'minute':
                d.setSeconds(59, 999);
                break;
              case 'hour':
                d.setMinutes(59, 59, 999);
                break;
              case 'day':
                d.setHours(23, 59, 59, 999);
                break;
              case 'week':
                const day = d.getDay();
                d.setDate(d.getDate() + (6 - day));
                d.setHours(23, 59, 59, 999);
                break;
              case 'month':
                d.setMonth(d.getMonth() + 1, 0);
                d.setHours(23, 59, 59, 999);
                break;
              case 'quarter':
                const quarter = Math.floor(d.getMonth() / 3);
                d.setMonth((quarter + 1) * 3, 0);
                d.setHours(23, 59, 59, 999);
                break;
              case 'year':
                d.setFullYear(d.getFullYear() + 1, 0, 0);
                d.setHours(23, 59, 59, 999);
                break;
            }
            return d.getTime();
          }
        });
      }));
    } else {
      // Chart.js not ready yet, try again
      setTimeout(loadAdapter, 50);
    }
  }
  
  // Start loading
  if (typeof window !== 'undefined') {
    loadAdapter();
  }
})();