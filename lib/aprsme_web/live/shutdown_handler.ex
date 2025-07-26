defmodule AprsmeWeb.Live.ShutdownHandler do
  @moduledoc """
  LiveView mixin for handling graceful shutdowns.
  Subscribes to shutdown events and shows a user-friendly message.
  """

  defmacro __using__(_opts) do
    quote do
      # Override mount to add shutdown handling
      defp mount_with_shutdown(params, session, socket) do
        # Subscribe to shutdown events
        Phoenix.PubSub.subscribe(Aprsme.PubSub, "shutdown")

        # Add shutdown state
        assign(socket, shutting_down: false, shutdown_countdown: nil)
      end

      @impl true
      def handle_info({:shutdown_initiated, drain_timeout}, socket) do
        # Silently handle shutdown - no user notification
        # Just mark the socket as shutting down internally
        {:noreply, assign(socket, shutting_down: true, shutdown_countdown: nil)}
      end

      def handle_info(:shutdown_tick, socket) do
        # No longer used - removing countdown
        {:noreply, socket}
      end

      def handle_info(:shutdown_now, socket) do
        # Silent shutdown - just stop the socket
        {:stop, :normal, socket}
      end

      # Allow the module using this macro to override handle_info
      defoverridable handle_info: 2
    end
  end
end
