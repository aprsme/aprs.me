// Error Boundary Hook for Phoenix LiveView
// Catches JavaScript errors and displays fallback content

interface ErrorBoundaryContext {
  el: HTMLElement;
  content: HTMLElement | null;
  fallback: HTMLElement | null;
  errorHandler: (event: ErrorEvent) => void;
  unhandledRejectionHandler: (event: PromiseRejectionEvent) => void;
  pushEvent?: (event: string, payload: Record<string, unknown>) => void;
}

interface ErrorLike {
  message?: string;
  stack?: string;
  target?: EventTarget | null;
}

const ErrorBoundary = {
  mounted(this: ErrorBoundaryContext) {
    this.content = this.el.querySelector(".error-boundary-content");
    this.fallback = this.el.querySelector(".error-boundary-fallback");

    this.errorHandler = handleGlobalError.bind(this);
    this.unhandledRejectionHandler = handleUnhandledRejection.bind(this);

    window.addEventListener("error", this.errorHandler, true);
    window.addEventListener(
      "unhandledrejection",
      this.unhandledRejectionHandler,
      true,
    );
  },

  destroyed(this: ErrorBoundaryContext) {
    if (this.errorHandler) {
      window.removeEventListener("error", this.errorHandler, true);
    }
    if (this.unhandledRejectionHandler) {
      window.removeEventListener(
        "unhandledrejection",
        this.unhandledRejectionHandler,
        true,
      );
    }
  },
};

function handleGlobalError(this: ErrorBoundaryContext, event: ErrorEvent) {
  const error: ErrorLike = event.error || { message: event.message };

  if (isErrorInComponent.call(this, error)) {
    handleError.call(this, error);
    event.preventDefault();
    event.stopPropagation();
  }
}

function handleUnhandledRejection(
  this: ErrorBoundaryContext,
  event: PromiseRejectionEvent,
) {
  const error = event.reason as ErrorLike | undefined;
  if (isErrorInComponent.call(this, error)) {
    handleError.call(this, error);
    event.preventDefault();
    event.stopPropagation();
  }
}

function isErrorInComponent(
  this: ErrorBoundaryContext,
  error: ErrorLike | undefined,
): boolean {
  if (!error) return false;

  if (
    error.target &&
    error.target instanceof Node &&
    this.el.contains(error.target)
  ) {
    return true;
  }

  if (error.stack) {
    const componentId = this.el.id;
    if (componentId && error.stack.includes(componentId)) {
      return true;
    }

    const hooks = this.el.querySelectorAll("[phx-hook]");
    for (const hook of hooks) {
      const hookName = hook.getAttribute("phx-hook");
      if (hookName && error.stack.includes(hookName)) {
        return true;
      }
    }
  }

  return false;
}

function handleError(
  this: ErrorBoundaryContext,
  error: ErrorLike | undefined,
) {
  console.error("Error caught by boundary:", error);

  if (this.content) {
    this.content.style.display = "none";
  }
  if (this.fallback) {
    this.fallback.classList.remove("hidden");
  }

  if (this.pushEvent && typeof this.pushEvent === "function") {
    try {
      this.pushEvent("error_boundary_triggered", {
        message: error?.message || "Unknown error",
        stack: error?.stack || "No stack trace",
        component_id: this.el?.id || "unknown",
      });
    } catch (e) {
      console.error("Failed to send error to server:", e);
    }
  }
}

export default ErrorBoundary;
