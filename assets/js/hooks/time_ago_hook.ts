// Hook to automatically update time ago displays

interface TimeAgoHookContext {
  el: HTMLElement;
  timer: ReturnType<typeof setTimeout> | null;
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
  const timestampStr = this.el.dataset.timestamp;
  if (!timestampStr) {
    return;
  }

  const timestampMs = new Date(timestampStr).getTime();
  if (Number.isNaN(timestampMs)) {
    return;
  }

  const updateTimeAgo = () => {
    const nowMs = Date.now();
    const diffSeconds = Math.max(0, Math.floor((nowMs - timestampMs) / 1000));

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

    const nextDelay = getNextUpdateDelay(diffSeconds);
    this.timer = setTimeout(updateTimeAgo, nextDelay);
  };

  updateTimeAgo();
}

function stopTimer(this: TimeAgoHookContext) {
  if (this.timer) {
    clearTimeout(this.timer);
    this.timer = null;
  }
}

function getNextUpdateDelay(diffSeconds: number): number {
  if (diffSeconds < 60) {
    return 1000;
  }

  if (diffSeconds < 3600) {
    return (60 - (diffSeconds % 60)) * 1000;
  }

  if (diffSeconds < 86400) {
    return (3600 - (diffSeconds % 3600)) * 1000;
  }

  return (86400 - (diffSeconds % 86400)) * 1000;
}

export default TimeAgoHook;
