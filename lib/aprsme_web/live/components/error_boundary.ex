defmodule AprsmeWeb.Components.ErrorBoundary do
  @moduledoc """
  Error boundary component for gracefully handling errors in LiveView.

  This component wraps other components and catches errors, displaying
  a user-friendly error message instead of crashing the entire view.
  """
  use Phoenix.Component

  @doc """
  Wraps content in an error boundary that catches and displays errors gracefully.

  ## Examples

      <.error_boundary id="map-error-boundary">
        <%= live_render(@socket, AprsmeWeb.MapLive.Index) %>
      </.error_boundary>
  """
  attr :id, :string, required: true
  attr :class, :string, default: nil
  slot :inner_block, required: true

  slot :fallback do
    attr :error, :string
  end

  def error_boundary(assigns) do
    ~H"""
    <div id={@id} class={@class} phx-hook="ErrorBoundary">
      <div class="error-boundary-content">
        {render_slot(@inner_block)}
      </div>
      <div class="error-boundary-fallback hidden">
        <%= if @fallback == [] do %>
          <.default_error_message />
        <% else %>
          {render_slot(@fallback, %{})}
        <% end %>
      </div>
    </div>
    """
  end

  defp default_error_message(assigns) do
    ~H"""
    <div class="rounded-md bg-red-50 p-4 my-4">
      <div class="flex">
        <div class="flex-shrink-0">
          <svg class="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
            <path
              fill-rule="evenodd"
              d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
              clip-rule="evenodd"
            />
          </svg>
        </div>
        <div class="ml-3">
          <h3 class="text-sm font-medium text-red-800">
            Something went wrong
          </h3>
          <div class="mt-2 text-sm text-red-700">
            <p>
              We're sorry, but something unexpected happened. The error has been logged
              and we'll look into it. Please try refreshing the page.
            </p>
          </div>
          <div class="mt-4">
            <button
              type="button"
              onclick="window.location.reload()"
              class="bg-red-50 px-2 py-1.5 rounded-md text-sm font-medium text-red-800 hover:bg-red-100 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-red-50 focus:ring-red-600"
            >
              Refresh page
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
