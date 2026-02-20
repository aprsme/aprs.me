// Hook to automatically update time ago displays

interface TimeAgoHookContext {
  el: HTMLElement;
  timer: ReturnType<typeof setInterval> | null;
}

const TimeAgoHook = {
  mounted(this: TimeAgoHookContext) {
    this.timer = null;
    startTimer.call(this);
  },

  destroyed(this: TimeAgoHookContext) {
    stopTimer.call(this);
  },

  updated(this: TimeAgoHookContext) {
    stopTimer.call(this);
    startTimer.call(this);
  },
};

function startTimer(this: TimeAgoHookContext) {
  const updateTimeAgo = () => {
    const timestampStr = this.el.dataset.timestamp;
    if (!timestampStr) return;

    const timestamp = new Date(timestampStr);
    const now = new Date();
    const diffMs = now.getTime() - timestamp.getTime();
    const diffSeconds = Math.floor(diffMs / 1000);

    if (diffSeconds < 60) {
      this.el.textContent = `${diffSeconds} second${diffSeconds !== 1 ? "s" : ""} ago`;
    } else if (diffSeconds < 3600) {
      const minutes = Math.floor(diffSeconds / 60);
      this.el.textContent = `${minutes} minute${minutes !== 1 ? "s" : ""} ago`;
    } else if (diffSeconds < 86400) {
      const hours = Math.floor(diffSeconds / 3600);
      this.el.textContent = `${hours} hour${hours !== 1 ? "s" : ""} ago`;
    } else {
      const days = Math.floor(diffSeconds / 86400);
      this.el.textContent = `${days} day${days !== 1 ? "s" : ""} ago`;
    }
  };

  updateTimeAgo();

  const timestampStr = this.el.dataset.timestamp;
  if (timestampStr) {
    const timestamp = new Date(timestampStr);
    const age = Date.now() - timestamp.getTime();

    let interval: number;
    if (age < 60000) {
      interval = 1000;
    } else if (age < 3600000) {
      interval = 60000;
    } else {
      interval = 300000;
    }

    this.timer = setInterval(updateTimeAgo, interval);
  }
}

function stopTimer(this: TimeAgoHookContext) {
  if (this.timer) {
    clearInterval(this.timer);
    this.timer = null;
  }
}

export default TimeAgoHook;
