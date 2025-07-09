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
    if (!error) return false;
    
    // If the error has a target element, check if it's within our component
    if (error.target && this.el.contains(error.target)) {
      return true;
    }
    
    // Check if the error stack trace contains references to our component's hooks or elements
    if (error.stack) {
      // Look for our component's ID in the stack trace
      const componentId = this.el.id;
      if (componentId && error.stack.includes(componentId)) {
        return true;
      }
      
      // Check if the error originated from a hook within this component
      const hooks = this.el.querySelectorAll('[phx-hook]');
      for (let hook of hooks) {
        const hookName = hook.getAttribute('phx-hook');
        if (hookName && error.stack.includes(hookName)) {
          return true;
        }
      }
    }
    
    // For now, we'll be conservative and only handle errors we're sure about
    // In a more sophisticated implementation, you might analyze the error source
    return false;
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