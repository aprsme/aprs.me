// Hook to automatically update time ago displays
export default {
  mounted() {
    this.startTimer();
  },

  destroyed() {
    this.stopTimer();
  },

  updated() {
    // Restart timer when the element updates
    this.stopTimer();
    this.startTimer();
  },

  startTimer() {
    const updateTimeAgo = () => {
      const timestampStr = this.el.dataset.timestamp;
      if (!timestampStr) return;

      const timestamp = new Date(timestampStr);
      const now = new Date();
      const diffMs = now - timestamp;
      const diffSeconds = Math.floor(diffMs / 1000);

      if (diffSeconds < 60) {
        this.el.textContent = `${diffSeconds} second${diffSeconds !== 1 ? 's' : ''} ago`;
      } else if (diffSeconds < 3600) {
        const minutes = Math.floor(diffSeconds / 60);
        this.el.textContent = `${minutes} minute${minutes !== 1 ? 's' : ''} ago`;
      } else if (diffSeconds < 86400) {
        const hours = Math.floor(diffSeconds / 3600);
        this.el.textContent = `${hours} hour${hours !== 1 ? 's' : ''} ago`;
      } else {
        const days = Math.floor(diffSeconds / 86400);
        this.el.textContent = `${days} day${days !== 1 ? 's' : ''} ago`;
      }
    };

    // Update immediately
    updateTimeAgo();

    // Update every second for recent timestamps, less frequently for older ones
    const timestampStr = this.el.dataset.timestamp;
    if (timestampStr) {
      const timestamp = new Date(timestampStr);
      const age = Date.now() - timestamp;

      let interval;
      if (age < 60000) { // Less than 1 minute old
        interval = 1000; // Update every second
      } else if (age < 3600000) { // Less than 1 hour old
        interval = 60000; // Update every minute
      } else {
        interval = 300000; // Update every 5 minutes
      }

      this.timer = setInterval(updateTimeAgo, interval);
    }
  },

  stopTimer() {
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = null;
    }
  }
};