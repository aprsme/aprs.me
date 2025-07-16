// Error Boundary Hook for Phoenix LiveView
// Catches JavaScript errors and displays fallback content

const ErrorBoundary = {
  mounted() {
    // Get content and fallback elements
    this.content = this.el.querySelector('.error-boundary-content');
    this.fallback = this.el.querySelector('.error-boundary-fallback');
    
    // Create bound error handlers
    this.errorHandler = this.handleGlobalError.bind(this);
    this.unhandledRejectionHandler = this.handleUnhandledRejection.bind(this);
    
    // Set up error handlers for this component
    this.setupErrorHandlers();
  },
  
  setupErrorHandlers() {
    // Use event listeners instead of overriding global handlers
    window.addEventListener('error', this.errorHandler, true);
    window.addEventListener('unhandledrejection', this.unhandledRejectionHandler, true);
  },
  
  handleGlobalError(event) {
    // Extract error from ErrorEvent
    const error = event.error || new Error(event.message);
    
    if (this.isErrorInComponent(error)) {
      this.handleError(error);
      event.preventDefault();
      event.stopPropagation();
    }
  },
  
  handleUnhandledRejection(event) {
    if (this.isErrorInComponent(event.reason)) {
      this.handleError(event.reason);
      event.preventDefault();
      event.stopPropagation();
    }
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
    if (this.content && this.content.style) {
      this.content.style.display = 'none';
    }
    if (this.fallback && this.fallback.classList) {
      this.fallback.classList.remove('hidden');
    }
    
    // Log error to server if pushEvent is available
    if (this.pushEvent && typeof this.pushEvent === 'function') {
      try {
        this.pushEvent('error_boundary_triggered', {
          message: error?.message || 'Unknown error',
          stack: error?.stack || 'No stack trace',
          component_id: this.el?.id || 'unknown'
        });
      } catch (e) {
        console.error('Failed to send error to server:', e);
      }
    }
  },
  
  destroyed() {
    // Remove event listeners
    if (this.errorHandler) {
      window.removeEventListener('error', this.errorHandler, true);
    }
    if (this.unhandledRejectionHandler) {
      window.removeEventListener('unhandledrejection', this.unhandledRejectionHandler, true);
    }
  }
};

export default ErrorBoundary;