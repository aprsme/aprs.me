// Error Boundary Hook for Phoenix LiveView
// Catches JavaScript errors and displays fallback content

const ErrorBoundary = {
  mounted() {
    // Store original error handler
    this.originalErrorHandler = window.onerror;
    this.originalUnhandledRejection = window.onunhandledrejection;
    
    // Get content and fallback elements
    this.content = this.el.querySelector('.error-boundary-content');
    this.fallback = this.el.querySelector('.error-boundary-fallback');
    
    // Set up error handlers for this component
    this.setupErrorHandlers();
  },
  
  setupErrorHandlers() {
    // Handle synchronous errors
    window.onerror = (message, source, lineno, colno, error) => {
      if (this.isErrorInComponent(error)) {
        this.handleError(error || new Error(message));
        return true; // Prevent default error handling
      }
      
      // Call original handler if error is not in this component
      if (this.originalErrorHandler) {
        return this.originalErrorHandler(message, source, lineno, colno, error);
      }
      return false;
    };
    
    // Handle async errors (unhandled promise rejections)
    window.onunhandledrejection = (event) => {
      if (this.isErrorInComponent(event.reason)) {
        this.handleError(event.reason);
        event.preventDefault();
        return;
      }
      
      // Call original handler if error is not in this component
      if (this.originalUnhandledRejection) {
        this.originalUnhandledRejection(event);
      }
    };
  },
  
  isErrorInComponent(error) {
    // Check if the error occurred within this component's DOM tree
    // This is a simplified check - in production you might want more sophisticated logic
    if (!error || !error.stack) return false;
    
    // Check if any element in the component has an error
    const hasError = this.el.querySelector('[data-phx-error]');
    return !!hasError;
  },
  
  handleError(error) {
    console.error('Error caught by boundary:', error);
    
    // Hide content and show fallback
    if (this.content) {
      this.content.style.display = 'none';
    }
    if (this.fallback) {
      this.fallback.classList.remove('hidden');
    }
    
    // Log error to server
    this.pushEvent('error_boundary_triggered', {
      message: error.message,
      stack: error.stack,
      component_id: this.el.id
    });
  },
  
  destroyed() {
    // Restore original error handlers
    window.onerror = this.originalErrorHandler;
    window.onunhandledrejection = this.originalUnhandledRejection;
  }
};

export default ErrorBoundary;